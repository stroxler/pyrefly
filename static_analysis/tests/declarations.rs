/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests of the shared declaration walker, the lexical navigation engine, and the runtime
//! scope table built on them.

use pyrefly_python::ast::Ast;
use pyrefly_static_analysis::declarations::ChildScope;
use pyrefly_static_analysis::declarations::Context;
use pyrefly_static_analysis::declarations::Declaration;
use pyrefly_static_analysis::declarations::DeclarationSink;
use pyrefly_static_analysis::declarations::NameSite;
use pyrefly_static_analysis::declarations::Position;
use pyrefly_static_analysis::declarations::TestSite;
use pyrefly_static_analysis::declarations::walk_child;
use pyrefly_static_analysis::declarations::walk_suite;
use pyrefly_static_analysis::lexical;
use pyrefly_static_analysis::lexical::Category;
use pyrefly_static_analysis::lexical::NonlocalError;
use pyrefly_static_analysis::lexical::Probe;
use pyrefly_static_analysis::lexical::Resolution;
use pyrefly_static_analysis::lexical::ScopeChain;
use pyrefly_static_analysis::*;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprName;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Stmt;

/// Records every event, walking nested scopes when `runtime`. Otherwise it declines every
/// optional position and treats a test named `STATIC_FALSE` as statically false.
struct Trace {
    runtime: bool,
    events: Vec<String>,
}

impl<'a> DeclarationSink<'a> for Trace {
    fn declare(&mut self, d: Declaration<'a>) {
        let event = match d {
            Declaration::Import(a) => format!("import {}", a.name),
            Declaration::ImportFrom(x) => format!(
                "from {}",
                x.names
                    .iter()
                    .map(|a| a.name.as_str())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Declaration::Function(x) => format!("def {}", x.name),
            Declaration::Class(x) => format!("class {}", x.name),
            Declaration::Assign(x, _) => format!("assign {}", x.id),
            Declaration::AugAssign(x, _) => format!("aug {}", x.id),
            Declaration::Annotated(x, _) => format!("annotated {}", x.id),
            Declaration::Store(x) => format!("store {}", x.id),
            Declaration::Delete(x) => format!("del {}", x.id),
            Declaration::Walrus(x) => format!("walrus {}", x.id),
            Declaration::Handler(x) => format!("handler {}", x.id),
            Declaration::Capture(x) => format!("capture {}", x.id),
            Declaration::Global(x) => format!("global {}", x.id),
            Declaration::Nonlocal(x) => format!("nonlocal {}", x.id),
            Declaration::Parameter(x) => format!("param {}", x.id),
        };
        self.events.push(event);
    }

    fn name(&mut self, name: &'a ExprName, site: NameSite) {
        let site = match site {
            NameSite::Expression => "read",
            NameSite::AnnAssignTarget => "ann",
            NameSite::AugAssignTarget => "aug",
            NameSite::WalrusTarget => "target",
            NameSite::Pattern => "pattern",
        };
        self.events.push(format!("{site} {}", name.id));
    }

    fn keeps(&mut self, _: Position) -> bool {
        self.runtime
    }

    fn enter(&mut self, context: Context<'a>) {
        if let Context::Comprehension = context {
            self.events.push("(".to_owned());
        }
    }

    fn exit(&mut self, context: Context<'a>) {
        if let Context::Comprehension = context {
            self.events.push(")".to_owned());
        }
    }

    fn static_truth(&mut self, test: &'a Expr, _: TestSite) -> Option<bool> {
        match test {
            Expr::Name(x) if !self.runtime && x.id.as_str() == "STATIC_FALSE" => Some(false),
            _ => None,
        }
    }

    fn statement(&mut self, stmt: &'a Stmt) {
        if let Stmt::Expr(_) = stmt {
            self.events.push(";".to_owned());
        }
    }

    fn child(&mut self, child: ChildScope<'a>) {
        let kind = match child {
            ChildScope::Function(_) => "function",
            ChildScope::Lambda(_) => "lambda",
            ChildScope::Class(_) => "class",
            ChildScope::Comprehension { .. } => "comprehension",
        };
        self.events.push(format!("{kind} {{"));
        if self.runtime {
            walk_child(child, self);
        }
        self.events.push("}".to_owned());
    }
}

fn trace(source: &str, runtime: bool) -> String {
    let body = Ast::parse(source, PySourceType::Python).0.body;
    let mut sink = Trace {
        runtime,
        events: Vec::new(),
    };
    walk_suite(&body, &mut sink);
    sink.events.join(", ")
}

#[test]
fn comprehension_walrus_binds_in_the_enclosing_scope() {
    let source = "[(a := f(x)) for x in (b := xs) if (c := x) for y in [(d := 1) for _ in z]]\n";
    // The enclosing scope reads only the first iterable, and declares every walrus target, target target
    // in source order, even inside the nested comprehension.
    assert_eq!(
        trace(source, false),
        "(, walrus a, target a, walrus b, target b, read xs, walrus c, target c, walrus d, target d, ), comprehension {, }, ;"
    );
    // The comprehension declares its targets and evaluates the rest, without redeclaring
    // the walrus targets, target targets; the inner comprehension's first iterable runs in the outer one.
    assert_eq!(
        trace(source, true),
        "(, walrus a, target a, walrus b, target b, read xs, walrus c, target c, walrus d, target d, ), comprehension {, \
         store x, read x, read x, (, read z, ), comprehension {, store _, read _, }, store y, \
         read y, read f, read x, }, ;"
    );
}

#[test]
fn lambdas_stop_walrus_but_not_their_defaults() {
    let source = "[lambda a=(d := 1): (body := 2) for _ in xs]\n";
    assert_eq!(
        trace(source, true),
        "(, walrus d, target d, read xs, ), comprehension {, store _, read _, lambda {, param a, \
         walrus body, target body, }, }, ;"
    );
    // A sink that declines parameter defaults sees neither the default nor its walrus.
    assert_eq!(trace(source, false), "(, read xs, ), comprehension {, }, ;");
}

#[test]
fn headers_and_optional_positions() {
    let source = "\
@dec
def f(a, b=(w1 := 1), *args, c=(w2 := 2), **kw):
    return a
class C(Base, metaclass=(w3 := M)):
    x: int = 1
if t:
    pass
elif (w4 := u):
    pass
try:
    pass
except (w5 := E) as err:
    pass
match s:
    case [p] if (w6 := p):
        pass
    case Point(x=Color.RED):
        pass
target[(w7 := 0)] += 1
total += 1
label: str = s
";
    let runtime = trace(source, true);
    for name in ["w1", "w2", "w3", "w4", "w5", "w6", "w7"] {
        assert!(runtime.contains(&format!("walrus {name}")), "{runtime}");
    }
    assert!(runtime.contains("function {, param a, param b, param args, param c, param kw"));
    // Every name expression is reported, with its site.
    for event in ["pattern Point", "pattern Color", "aug total", "ann label"] {
        assert!(runtime.contains(event), "{runtime}");
    }
    let typing = trace(source, false);
    for name in ["w1", "w2", "w3", "w4", "w5", "w6", "w7"] {
        assert!(!typing.contains(&format!("walrus {name}")), "{typing}");
    }
    // Decorators run in the enclosing scope, then the name is bound, then the scope opens.
    assert!(typing.starts_with("read dec, def f, function {, }, class C, class {, }"));
}

#[test]
fn statement_order_and_static_pruning() {
    let source = "\
import a.b, c as d
from m import *, x as y
try:
    t = 1
except E as e:
    pass
match v:
    case [q, *r]:
        pass
del (z := w)[0], gone
first, second[(s := 0)] = __all__ = value
if STATIC_FALSE:
    dead = 1
elif other:
    live = 1
else:
    rest = 1
pick = p if STATIC_FALSE else (o := 1)
type Alias[T] = list[T]
global g
nonlocal n
";
    assert_eq!(
        trace(source, false),
        "import a.b, import c, from * x, handler e, assign t, read t, read v, capture q, \
         capture r, walrus z, target z, read w, read gone, del gone, read value, store first, \
         read first, read second, walrus s, target s, assign __all__, read __all__, read STATIC_FALSE, \
         assign live, read live, assign rest, read rest, read STATIC_FALSE, walrus o, target o, \
         assign pick, read pick, store Alias, read Alias, global g, nonlocal n"
    );
    // The runtime prunes nothing.
    assert!(trace(source, true).contains("assign dead"));
}

/// A chain of scopes given by their categories, innermost last.
#[derive(Clone, Copy)]
struct Chain<'a>(&'a [Category]);

impl ScopeChain for Chain<'_> {
    type Id = usize;
    type Barrier = usize;

    fn parent(&self, id: usize) -> Option<usize> {
        id.checked_sub(1)
    }

    fn category(&self, id: usize) -> Category {
        self.0[id]
    }

    fn barrier(&self, id: usize) -> usize {
        id
    }

    fn join(a: usize, b: usize) -> usize {
        a.max(b)
    }
}

#[test]
fn visible_scopes_skip_classes_except_from_their_annotation_scopes() {
    use Category::*;
    let views = |categories: &[Category]| {
        lexical::visible(Chain(categories), categories.len() - 1)
            .map(|v| (v.id, v.depth, v.barrier))
            .collect::<Vec<_>>()
    };
    // A method does not see its class, which contributes no barrier.
    assert_eq!(
        views(&[Module, Class, Function]),
        vec![(2, 0, 0), (0, 2, 2)]
    );
    // A class body sees itself, and its annotation and `type` alias scopes see it.
    assert_eq!(views(&[Module, Class]), vec![(1, 0, 0), (0, 1, 1)]);
    assert_eq!(
        views(&[Module, Class, Annotation { class_scope: false }]),
        vec![(2, 0, 0), (1, 1, 2), (0, 2, 2)]
    );
    assert_eq!(
        views(&[Module, Class, TypeAlias]),
        vec![(2, 0, 0), (1, 1, 2), (0, 2, 2)]
    );
    // Scopes nested further in do not.
    assert_eq!(
        views(&[Module, Class, Annotation { class_scope: false }, Function]),
        vec![(3, 0, 0), (2, 1, 3), (0, 3, 3)]
    );
}

#[test]
fn nonlocal_resolution() {
    use Category::*;
    let resolve = |categories: &[Category], binds: &[Probe<()>]| {
        let probe = |id: usize| match binds[id] {
            Probe::Owned(()) => Probe::Owned(id),
            Probe::Absent => Probe::Absent,
            Probe::Global => Probe::Global,
            Probe::Nonlocal => Probe::Nonlocal,
        };
        lexical::resolve_nonlocal(Chain(categories), Some(categories.len() - 1), probe)
    };
    let (o, a, g, n) = (
        || Probe::Owned(()),
        || Probe::Absent,
        || Probe::Global,
        || Probe::Nonlocal,
    );
    assert_eq!(resolve(&[Module, Function], &[a(), o()]), Ok(1));
    // Classes are skipped, enclosing `nonlocal` declarations are followed.
    assert_eq!(
        resolve(&[Module, Function, Class, Function], &[a(), o(), o(), n()]),
        Ok(1)
    );
    assert_eq!(
        resolve(&[Module, Function, Function], &[a(), o(), g()]),
        Err(NonlocalError::Global)
    );
    assert_eq!(
        resolve(&[Module, Function], &[o(), a()]),
        Err(NonlocalError::Global)
    );
    // A `nonlocal` declaration at the root refers to nothing.
    assert_eq!(
        resolve(&[Module, Function], &[n(), a()]),
        Err(NonlocalError::NotFound)
    );
    assert_eq!(
        resolve(&[Module, Function], &[a(), a()]),
        Err(NonlocalError::NotFound)
    );
}

#[test]
fn resolution_redirects_global_and_nonlocal() {
    use Category::*;
    let categories = [Module, Function, Function];
    let probe = |binds: [Probe<usize>; 3]| {
        let mut binds = binds.map(Some);
        move |id: usize| binds[id].take().unwrap_or(Probe::Absent)
    };
    let found = |r: Resolution<usize>| match r {
        Resolution::Found(id) => Some(id),
        Resolution::Unbound | Resolution::Nonlocal(_) => None,
    };
    let chain = Chain(&categories);
    assert_eq!(
        found(lexical::resolve(
            chain,
            2,
            probe([Probe::Owned(0), Probe::Owned(1), Probe::Global])
        )),
        Some(0)
    );
    assert_eq!(
        found(lexical::resolve(
            chain,
            2,
            probe([Probe::Owned(0), Probe::Owned(1), Probe::Nonlocal])
        )),
        Some(1)
    );
    assert!(matches!(
        lexical::resolve(
            chain,
            2,
            probe([Probe::Absent, Probe::Absent, Probe::Absent])
        ),
        Resolution::Unbound
    ));
}

fn program(source: &str) -> Program {
    analyze_python(source, ModuleName::from_str("pkg.mod"), false)
}

fn scope_kinds(p: &Program) -> Vec<(ScopeKind, Option<u32>, u32)> {
    p.scopes
        .iter()
        .map(|s| (s.kind, s.parent.map(|x| x.0), s.region.0))
        .collect()
}

#[test]
fn runtime_ownership_is_unpruned() {
    let p = program(
        "\
import sys
def f():
    if False:
        dead = 1
    if sys.version_info < (3, 0):
        old = 1
    return dead, old
",
    );
    let function = p
        .scopes
        .iter()
        .position(|s| s.kind == ScopeKind::Function)
        .unwrap();
    for name in ["dead", "old"] {
        let symbol = p
            .resolve(ScopeId(function as u32), &Name::new(name))
            .unwrap();
        assert_eq!(p.symbol(symbol).scope.0, function as u32, "{name}");
    }
}

#[test]
fn runtime_scope_tree_and_regions() {
    let p = program(
        "\
def f(a=lambda: 1):
    return [x for x in a]
g = (y for y in ())
class C:
    z = lambda: 2
",
    );
    // Default lambdas belong to the module and are created before the function they
    // parameterize; the eager comprehension shares the function's region; the generator
    // and lambdas get their own regions; the class runs in the module's region.
    assert_eq!(
        scope_kinds(&p)
            .into_iter()
            .filter(|(k, ..)| *k != ScopeKind::Builtins)
            .collect::<Vec<_>>(),
        vec![
            (ScopeKind::Module, None, 0),
            (ScopeKind::Lambda, Some(0), 1),
            (ScopeKind::Function, Some(0), 2),
            (ScopeKind::Comprehension, Some(2), 2),
            (ScopeKind::Comprehension, Some(0), 3),
            (ScopeKind::Class, Some(0), 0),
            (ScopeKind::Lambda, Some(5), 4),
        ]
    );
    let generator = &p.scopes[4];
    assert!(generator.symbols.contains_key(&Name::new(".0")));
    assert!(!p.scopes[3].symbols.contains_key(&Name::new(".0")));
}

#[test]
fn runtime_class_body_in_function_falls_back_to_the_module() {
    let p = program(
        "\
def f():
    x = 1
    class C:
        x = x
        y = x
    return C
",
    );
    let class = p
        .scopes
        .iter()
        .position(|s| s.kind == ScopeKind::Class)
        .unwrap();
    let class_x = *p.scopes[class].symbols.get(&Name::new("x")).unwrap();
    // `LOAD_NAME` in a class body skips the enclosing function, so an unbound class `x`
    // falls back to the (absent) module `x`, which is the builtin.
    let fallback = p.symbol(class_x).fallback.unwrap();
    assert_eq!(p.symbol(fallback).origin, SymbolOrigin::Builtin);
}

#[test]
fn runtime_builtins_and_module_declarations() {
    let p = program(
        "\
global g
g = 1
nonlocal n
n = 2
def f():
    global n
    return print(g, n)
",
    );
    let print = p.resolve(ScopeId(0), &Name::new("print")).unwrap();
    // The builtins scope resolves its own names, and nothing else.
    let builtins = p.symbol(print).scope;
    assert_eq!(p.resolve(builtins, &Name::new("print")), Some(print));
    assert_eq!(p.resolve(builtins, &Name::new("missing")), None);
    // `global` at module level is a no-op, and a module-level `nonlocal` (a syntax error)
    // does not hide the module's own binding.
    let function = p
        .scopes
        .iter()
        .position(|s| s.kind == ScopeKind::Function)
        .unwrap();
    for name in ["g", "n"] {
        let module = *p.scopes[0].symbols.get(&Name::new(name)).unwrap();
        assert_eq!(
            p.resolve(ScopeId(0), &Name::new(name)),
            Some(module),
            "{name}"
        );
    }
    let n = *p.scopes[0].symbols.get(&Name::new("n")).unwrap();
    assert_eq!(
        p.resolve(ScopeId(function as u32), &Name::new("n")),
        Some(n)
    );
}

#[test]
fn runtime_nonlocal_cannot_cross_a_global_declaration() {
    let p = program(
        "\
x = 1
def f():
    global x
    def g():
        nonlocal x
        return x
",
    );
    assert!(p.ops.iter().any(|op| matches!(
        op.kind,
        OpKind::Gap {
            kind: GapKind::ScopeError,
            ..
        }
    )));
}
