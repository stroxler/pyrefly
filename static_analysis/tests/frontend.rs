/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests of the shared frontend: scopes, evaluation order, control flow, and the
//! domain-free history queries.

use std::collections::BTreeSet;

use pyrefly_static_analysis::*;

fn program(source: &str) -> Program {
    analyze_python(source, ModuleName::from_str("pkg.mod"), false)
}

fn region(p: &Program, name: &str) -> RegionId {
    let i = p
        .regions
        .iter()
        .position(|r| r.name.as_str() == name)
        .unwrap_or_else(|| panic!("no region named {name}"));
    RegionId(i as u32)
}

/// The `Name` operations on symbols called `name` that run in `region`, in emission order.
fn name_ops(p: &Program, region: RegionId, name: &str) -> Vec<(OpId, SymbolId, Access)> {
    p.ops
        .iter()
        .enumerate()
        .filter_map(|(i, op)| match op.kind {
            OpKind::Name { symbol, access }
                if op.region == region && p.symbol(symbol).name.as_str() == name =>
            {
                Some((OpId(i as u32), symbol, access))
            }
            _ => None,
        })
        .collect()
}

fn loads(p: &Program, region: RegionId, name: &str) -> Vec<OpId> {
    name_ops(p, region, name)
        .into_iter()
        .filter(|(_, _, a)| matches!(a, Access::Load(_)))
        .map(|(op, _, _)| op)
        .collect()
}

fn stores(p: &Program, region: RegionId, name: &str) -> Vec<OpId> {
    name_ops(p, region, name)
        .into_iter()
        .filter(|(_, _, a)| matches!(a, Access::Store(_)))
        .map(|(op, _, _)| op)
        .collect()
}

fn defs(ops: &[OpId]) -> BTreeSet<Def> {
    ops.iter().map(|op| Def::Op(*op)).collect()
}

fn reaching_last_load(p: &Program, h: &History, region: RegionId, name: &str) -> BTreeSet<Def> {
    let load = *loads(p, region, name).last().expect("a load");
    h.reaching(load).expect("a reachable owned load").clone()
}

fn gaps(p: &Program) -> Vec<GapKind> {
    p.ops
        .iter()
        .filter_map(|op| match op.kind {
            OpKind::Gap { kind, .. } => Some(kind),
            _ => None,
        })
        .collect()
}

#[test]
fn assignment_evaluates_value_before_targets() {
    let p = program("a.b[c] = d = f(x)\n");
    assert_eq!(
        p.dump(Program::MODULE),
        "\
b0:
  r1 = f@builtins
  r2 = x@builtins
  r0 = r1(r2)
  r4 = a@builtins
  r3 = r4.b
  r5 = c@builtins
  r3[r5] = r0
  d = r0
  return
"
    );
}

#[test]
fn decorators_then_defaults_then_function_then_application() {
    let p = program("@d1\n@d2\ndef g(p=h()):\n    pass\n");
    assert_eq!(
        p.dump(Program::MODULE),
        "\
b0:
  r0 = d1@builtins
  r1 = d2@builtins
  r4 = h@builtins
  r3 = r4()
  r2 = function g(r3)
  r5 = r1(r2)
  r6 = r0(r5)
  g = r6
  return
"
    );
    let g = p.region(region(&p, "g"));
    assert_eq!(g.trigger, Trigger::OnCall);
    assert_eq!(g.parent.map(|(r, _)| r), Some(Program::MODULE));
    assert!(g.params[0].has_default);
}

#[test]
fn augmented_assignment_reads_target_then_value_then_writes() {
    let p = program("a.b += v()\n");
    assert_eq!(
        p.dump(Program::MODULE),
        "\
b0:
  r1 = a@builtins
  r2 = r1.b
  r4 = v@builtins
  r3 = r4()
  r0 = r2 InPlace(Add) r3
  r1.b = r0
  return
"
    );
}

#[test]
fn short_circuit_and_conditional_branches_join() {
    let p = program(
        "\
x = 0
y = (x := 1) and (x := 2)
z = x
w = (x := 3) if c else 4
v = x
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    let x_loads = loads(&p, Program::MODULE, "x");
    // `and` may stop after its first operand.
    assert_eq!(h.reaching(x_loads[0]).unwrap(), &defs(&[x[1], x[2]]));
    // The walrus runs only on the `if` arm of the conditional expression.
    assert_eq!(h.reaching(x_loads[1]).unwrap(), &defs(&[x[1], x[2], x[3]]));
}

#[test]
fn chained_comparison_evaluates_middle_once_and_short_circuits() {
    let p = program("r = a < f() < c\n");
    let dump = p.dump(Program::MODULE);
    assert_eq!(dump.matches("f@builtins").count(), 1);
    assert_eq!(dump.matches("c@builtins").count(), 1);
    // `c` is evaluated only on the branch where the first comparison holds.
    let c = loads(&p, Program::MODULE, "c")[0];
    let a = loads(&p, Program::MODULE, "a")[0];
    assert_ne!(p.op(c).block, p.op(a).block);
}

#[test]
fn loop_else_break_and_continue() {
    let p = program(
        "\
x = 0
for i in it:
    if c:
        x = 1
        break
    if d:
        x = 2
        continue
    x = 3
else:
    x = 4
y = x
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    // `else` runs exactly when the loop was not broken, so only `break` and `else` reach.
    assert_eq!(
        reaching_last_load(&p, &h, Program::MODULE, "x"),
        defs(&[x[1], x[4]])
    );
}

#[test]
fn while_loop_paths() {
    let p = program(
        "\
x = 0
while c:
    x = 1
    if d:
        continue
    x = 2
y = x
while True:
    x = 5
    if e:
        break
z = x
while True:
    pass
after = 1
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    let x_loads = loads(&p, Program::MODULE, "x");
    assert_eq!(h.reaching(x_loads[0]).unwrap(), &defs(&[x[0], x[1], x[2]]));
    // `while True` exits only through `break`.
    assert_eq!(h.reaching(x_loads[1]).unwrap(), &defs(&[x[3]]));
    // Code after an infinite loop without `break` is unreachable.
    let after = stores(&p, Program::MODULE, "after")[0];
    assert!(h.uses(after).is_empty());
    struct Unit;
    impl Domain for Unit {
        type Value = ();
        type State = ();
        type Diagnostic = ();
        fn top(&self) {}
        fn join(&self, _: &(), _: &()) {}
        fn join_state(&self, _: &(), _: &()) {}
    }
    let result = Engine::new(&p, &Unit).analyze(Program::MODULE, Entry::new(()));
    assert!(!result.is_reachable(after, &p));
    assert!(result.exit.is_none());
}

#[test]
fn deferred_function_regions_and_outer_defaults() {
    let p = program(
        "\
def f(a, b=(y := 1), *args, c, **kw) -> R:
    z = a
    lam = lambda q=y: q
    return z
",
    );
    let f = region(&p, "f");
    // Body operations live only in the function region.
    assert!(stores(&p, Program::MODULE, "z").is_empty());
    assert_eq!(stores(&p, f, "z").len(), 1);
    // The default's walrus binds in the module, where it is evaluated.
    let y = stores(&p, Program::MODULE, "y");
    assert_eq!(y.len(), 1);
    assert_eq!(
        p.symbol(name_ops(&p, Program::MODULE, "y")[0].1).scope,
        p.region(Program::MODULE).scope
    );
    // The lambda default is evaluated in `f`, reading the module's `y`.
    let y_in_f = loads(&p, f, "y");
    assert_eq!(y_in_f.len(), 1);
    assert_eq!(p.owner(name_ops(&p, f, "y")[0].1), Some(Program::MODULE));
    let kinds: Vec<ParamKind> = p.region(f).params.iter().map(|p| p.kind).collect();
    assert_eq!(
        kinds,
        vec![
            ParamKind::Positional,
            ParamKind::Positional,
            ParamKind::VarArgs,
            ParamKind::KeywordOnly,
            ParamKind::KwArgs
        ]
    );
    assert!(matches!(&p.region(f).returns, Some(StaticExpr::Name(n, _)) if n.as_str() == "R"));
    let lam = p.region(region(&p, "<lambda>"));
    assert_eq!(lam.parent.map(|(r, _)| r), Some(f));
}

#[test]
fn imported_aliases_and_local_shadowing() {
    let p = program(
        "\
import os.path
import os.path as osp
from . import sibling
from .sub import thing as t
def f():
    t = 1
    return t, osp
def g():
    print(t)
    if False:
        t = 2
",
    );
    let imports: Vec<(String, ImportItem)> = p
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            OpKind::Import { module, item, .. } => Some((module.to_string(), item.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(
        imports,
        vec![
            ("os.path".to_owned(), ImportItem::Root),
            ("os.path".to_owned(), ImportItem::Module),
            ("pkg".to_owned(), ImportItem::Name(Name::new("sibling"))),
            ("pkg.sub".to_owned(), ImportItem::Name(Name::new("thing"))),
        ]
    );
    // `import os.path` binds `os`.
    assert!(p.global("os").is_some());
    let f = region(&p, "f");
    let g = region(&p, "g");
    let module_t = p.global("t").unwrap();
    assert!(name_ops(&p, f, "t").iter().all(|(_, s, _)| *s != module_t));
    assert_eq!(name_ops(&p, f, "osp")[0].1, p.global("osp").unwrap());
    // A binding in dead code still makes `t` local to `g`: the read is unbound.
    let h = History::new(&p);
    assert_eq!(
        h.reaching(loads(&p, g, "t")[0]).unwrap(),
        &BTreeSet::from([Def::Entry])
    );
}

#[test]
fn global_nonlocal_and_closure_ownership() {
    let p = program(
        "\
counter = 0
def outer():
    v = 1
    def inner():
        nonlocal v
        global counter, fresh
        v = v + 1
        counter = 2
        fresh = 3
        return w
    w = 4
    return inner
",
    );
    let outer = region(&p, "outer");
    let inner = region(&p, "inner");
    let v = name_ops(&p, inner, "v");
    assert!(v.iter().all(|(_, s, _)| p.owner(*s) == Some(outer)));
    assert!(p.symbol(v[0].1).captured);
    let counter = name_ops(&p, inner, "counter")[0].1;
    assert_eq!(counter, p.global("counter").unwrap());
    let fresh = p.global("fresh").unwrap();
    assert_eq!(p.symbol(fresh).origin, SymbolOrigin::GlobalDeclaration);
    assert_eq!(name_ops(&p, inner, "fresh")[0].1, fresh);
    // A free read in `inner` resolves to `outer`'s `w`, even though it is assigned later.
    assert_eq!(p.owner(name_ops(&p, inner, "w")[0].1), Some(outer));
    // History lists accesses from every region.
    let h = History::new(&p);
    assert_eq!(h.accesses(counter).len(), 2);
    // Loads of another region's symbol have no reaching definitions.
    assert!(h.reaching(loads(&p, inner, "v")[0]).is_none());
}

#[test]
fn class_and_comprehension_scopes() {
    let p = program(
        "\
x = 'module'
class C(Base):
    x = 'class'
    y = x
    def m(self):
        return x
    z = [x for _ in range(3)]
def f(items):
    total = [last := i for i in items]
    return last
",
    );
    let module_x = p.global("x").unwrap();
    // The class body reads its own `x`.
    let class_scope = p
        .scopes
        .iter()
        .position(|s| s.kind == ScopeKind::Class)
        .unwrap();
    let class_x = *p.scopes[class_scope].symbols.get(&Name::new("x")).unwrap();
    let x_loads: Vec<SymbolId> = name_ops(&p, Program::MODULE, "x")
        .into_iter()
        .filter(|(_, _, a)| matches!(a, Access::Load(_)))
        .map(|(_, s, _)| s)
        .collect();
    // `y = x` reads the class `x`; the comprehension element cannot see class scope.
    assert_eq!(x_loads, vec![class_x, module_x]);
    let m = region(&p, "m");
    assert_eq!(name_ops(&p, m, "x")[0].1, module_x);
    // The class body is lowered inline, followed by class creation and binding.
    assert!(p.dump(Program::MODULE).contains("= class C("));
    // A walrus in a comprehension binds in the enclosing function.
    let f = region(&p, "f");
    let last = name_ops(&p, f, "last");
    assert!(last.iter().all(|(_, s, _)| p.owner(*s) == Some(f)
        && p.scope(p.symbol(*s).scope).kind == ScopeKind::Function));
    let i = name_ops(&p, f, "i")[0].1;
    assert_eq!(p.scope(p.symbol(i).scope).kind, ScopeKind::Comprehension);
}

#[test]
fn generator_expression_is_deferred_except_first_iterable() {
    let p = program("g = (f(e) for e in xs if e)\n");
    let genexpr = region(&p, "<genexpr>");
    assert_eq!(p.region(genexpr).trigger, Trigger::OnIteration);
    assert_eq!(loads(&p, Program::MODULE, "xs").len(), 1);
    assert!(loads(&p, Program::MODULE, "f").is_empty());
    assert_eq!(loads(&p, genexpr, "f").len(), 1);
}

#[test]
fn triggers() {
    let p = program(
        "\
def plain(): pass
def gen(): yield 1
async def coro(): await x
async def agen(): yield 1
",
    );
    let trigger = |n| p.region(region(&p, n)).trigger;
    assert_eq!(trigger("<module>"), Trigger::OnImport);
    assert_eq!(trigger("plain"), Trigger::OnCall);
    assert_eq!(trigger("gen"), Trigger::OnIteration);
    assert_eq!(trigger("coro"), Trigger::OnAwait);
    assert_eq!(trigger("agen"), Trigger::OnIteration);
}

#[test]
fn context_exit_and_finally_run_on_every_normal_exit() {
    let p = program(
        "\
def f():
    for i in xs:
        with m() as c:
            if c:
                break
            try:
                return c
            finally:
                cleanup()
",
    );
    let dump = p.dump(region(&p, "f"));
    // `return c` runs `finally`, then exits the context, then returns.
    assert!(
        dump.contains("  r7 = c\n  r9 = cleanup@builtins\n  r8 = r9()\n  exit r3\n  return r7\n"),
        "{dump}"
    );
    // `break` exits the context before leaving the loop.
    assert!(dump.contains("  exit r3\n  jump b4\n"), "{dump}");
    assert!(gaps(&p).contains(&GapKind::Exceptions));
}

#[test]
fn try_except_models_exceptions_at_statement_boundaries() {
    let p = program(
        "\
x = 0
try:
    x = 1
    x = 2
except E as e:
    y = x
else:
    x = 3
z = x
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    let x_loads = loads(&p, Program::MODULE, "x");
    // The handler sees the state before, between and after the body statements.
    assert_eq!(h.reaching(x_loads[0]).unwrap(), &defs(&[x[0], x[1], x[2]]));
    // After the statement: the handler path or the `else` path.
    assert_eq!(
        h.reaching(x_loads[1]).unwrap(),
        &defs(&[x[0], x[1], x[2], x[3]])
    );
    // The handler deletes its exception name.
    assert!(
        name_ops(&p, Program::MODULE, "e")
            .iter()
            .any(|(_, _, a)| *a == Access::Delete)
    );
}

#[test]
fn explicit_gaps() {
    let p = program(
        "\
from os import *
from .... import too_far
match v:
    case [a, b]:
        pass
type Alias = int
x: make_type() = 1
def f[T](a: T): pass
",
    );
    let found: BTreeSet<GapKind> = gaps(&p).into_iter().collect();
    for kind in [
        GapKind::StarImport,
        GapKind::InvalidRelativeImport,
        GapKind::Match,
        GapKind::TypeParams,
        GapKind::AnnotationEvaluation,
    ] {
        assert!(found.contains(&kind), "missing {kind:?} in {found:?}");
    }
    let bad = program("def f():\n    nonlocal q\n    q = 1\n");
    assert!(gaps(&bad).contains(&GapKind::ScopeError));
    let syntax = program("x = (\n");
    assert!(!syntax.syntax_errors.is_empty());
    assert!(gaps(&syntax).contains(&GapKind::SyntaxError));
}

#[test]
fn history_def_use_chains() {
    let p = program(
        "\
x = 1
if c:
    x = 2
print(x)
del x
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    let load = loads(&p, Program::MODULE, "x")[0];
    assert_eq!(h.uses(x[0]), &[load]);
    assert_eq!(h.uses(x[1]), &[load]);
    let x_symbol = p.global("x").unwrap();
    assert_eq!(h.accesses(x_symbol).len(), 4);
}

#[test]
fn raise_terminators_carry_cause_and_range() {
    let p = program(
        "\
def f(x):
    assert x, 'msg'
    try:
        g()
    except ValueError:
        pass
    raise KeyError(x)
",
    );
    let f = p.region(region(&p, "f"));
    let mut raises: Vec<(RaiseCause, String)> = f
        .blocks
        .iter()
        .filter_map(|b| match &b.terminator {
            Terminator::Raise { cause, range, .. } => {
                Some((*cause, p.text(*range).lines().next().unwrap().to_owned()))
            }
            _ => None,
        })
        .collect();
    raises.sort();
    assert!(f.blocks.iter().all(|b| !matches!(
        b.terminator,
        Terminator::Raise {
            cause: RaiseCause::AssertFailure,
            exc: Some(_),
            ..
        }
    )));
    assert_eq!(
        raises,
        vec![
            (RaiseCause::Explicit, "raise KeyError(x)".to_owned()),
            (RaiseCause::AssertFailure, "assert x, 'msg'".to_owned()),
            (RaiseCause::Reraise, "try:".to_owned()),
        ]
    );
}

#[test]
fn free_symbols_and_operation_scopes() {
    let p = program(
        "\
import m
def outer():
    v = 1
    def inner():
        return v, m, len
    class C:
        m = m
    return inner
",
    );
    let names = |r: RegionId| -> Vec<String> {
        p.free_symbols(r)
            .iter()
            .map(|s| {
                let owner = p
                    .owner(*s)
                    .map_or("builtins".to_owned(), |o| p.region(o).name.to_string());
                format!("{}@{owner}", p.symbol(*s).name)
            })
            .collect()
    };
    let inner = region(&p, "inner");
    let outer = region(&p, "outer");
    // `inner` reads `outer`'s cell, a module global (falling back to the builtin while
    // unbound) and a builtin.
    let mut inner_free = names(inner);
    inner_free.sort();
    assert_eq!(
        inner_free,
        vec!["len@builtins", "m@<module>", "m@builtins", "v@outer"]
    );
    // `outer`'s free symbols include the module `m` read through the class fallback,
    // and exclude `v`, which `outer` owns.
    let mut outer_free = names(outer);
    outer_free.sort();
    assert_eq!(outer_free, vec!["len@builtins", "m@<module>", "m@builtins"]);
    let module_symbols: Vec<String> = p
        .module_symbols()
        .map(|s| p.symbol(s).name.to_string())
        .collect();
    assert_eq!(module_symbols, vec!["m", "outer"]);
    // Operations in the class body record the class scope.
    let class_store = stores(&p, outer, "m")[0];
    assert_eq!(p.scope(p.op(class_store).scope).kind, ScopeKind::Class);
}

#[test]
fn call_arguments_evaluate_positional_before_keyword() {
    // CPython evaluates `*args` before an earlier keyword argument.
    let p = program("f(k=kw(), *star(), **more())\nclass C(meta=kw(), *star()): pass\n");
    let calls: Vec<String> = p
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            OpKind::Call { args, .. } if args.is_empty() => Some(p.text(op.range).to_owned()),
            _ => None,
        })
        .collect();
    assert_eq!(calls, vec!["star()", "kw()", "more()", "star()", "kw()"]);
    let shapes: Vec<Vec<&str>> = p
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            OpKind::Call { args, .. } if !args.is_empty() => Some(args),
            OpKind::MakeClass { bases, .. } => Some(bases),
            _ => None,
        })
        .map(|args| {
            args.iter()
                .map(|a| match a {
                    Arg::Positional(_) => "positional",
                    Arg::Star(_) => "star",
                    Arg::Keyword(..) => "keyword",
                    Arg::DoubleStar(_) => "double star",
                })
                .collect()
        })
        .collect();
    assert_eq!(
        shapes,
        vec![
            vec!["star", "keyword", "double star"],
            vec!["star", "keyword"]
        ]
    );
}

#[test]
fn annotated_targets_without_values_evaluate_their_receivers() {
    let p = program(
        "\
effect().attr: int
table[key()]: int
obj().field: int = value()
def f():
    local: int
",
    );
    let calls: Vec<String> = p
        .ops
        .iter()
        .filter_map(|op| match &op.kind {
            OpKind::Call { .. } => Some(p.text(op.range).to_owned()),
            _ => None,
        })
        .collect();
    // With a value, the value is evaluated before the target's object.
    assert_eq!(calls, vec!["effect()", "key()", "value()", "obj()"]);
    // Nothing is stored for annotation-only targets.
    let stores = p
        .ops
        .iter()
        .filter(|op| {
            matches!(
                op.kind,
                OpKind::Attribute {
                    access: Access::Store(_),
                    ..
                } | OpKind::Subscript {
                    access: Access::Store(_),
                    ..
                }
            )
        })
        .count();
    assert_eq!(stores, 1);
    // A bare annotation still makes the name local, without a store.
    let f = region(&p, "f");
    assert!(name_ops(&p, f, "local").is_empty());
    assert!(
        p.scope(p.region(f).scope)
            .symbols
            .contains_key(&Name::new("local"))
    );
}

/// Every operation appears exactly once, in the block it records.
fn assert_ops_in_blocks(p: &Program) {
    let mut seen = vec![0; p.ops.len()];
    for (r, region) in p.regions.iter().enumerate() {
        for (b, block) in region.blocks.iter().enumerate() {
            for op in &block.ops {
                seen[op.index()] += 1;
                assert_eq!(p.op(*op).region, RegionId(r as u32));
                assert_eq!(p.op(*op).block, BlockId(b as u32));
            }
        }
    }
    assert!(seen.iter().all(|n| *n == 1), "{seen:?}");
}

#[test]
fn definitions_in_finally_are_lowered_once() {
    let p = program(
        "\
def outer(c):
    try:
        if c:
            return 1
    finally:
        g = lambda: c
    return 2
",
    );
    assert_ops_in_blocks(&p);
    let lambdas = p
        .regions
        .iter()
        .filter(|r| r.name.as_str() == "<lambda>")
        .count();
    assert_eq!(lambdas, 1);
    // Each exit path creates the function from the same region.
    let creations = p
        .ops
        .iter()
        .filter(|op| matches!(op.kind, OpKind::MakeFunction { .. }) && op.region != Program::MODULE)
        .count();
    assert_eq!(creations, 3);
}

#[test]
fn nested_finally_expansion_is_bounded() {
    // Each level's `finally` body holds the next `try`, so copies multiply per level.
    let depth = 14;
    let mut source = String::from("def f(c):\n");
    for i in 0..depth {
        let indent = "    ".repeat(2 * i + 1);
        source.push_str(&format!(
            "{indent}try:\n{indent}    if c: return {i}\n{indent}finally:\n{indent}    g{i}()\n"
        ));
    }
    let p = program(&source);
    assert_ops_in_blocks(&p);
    assert!(p.ops.len() < 100_000, "{}", p.ops.len());
    assert!(gaps(&p).contains(&GapKind::FinallyLimit));
}

#[test]
fn external_writers_make_reaching_definitions_inexact() {
    let p = program(
        "\
x = 1
def set_global():
    global x
    x = 2
set_global()
print(x)
def outer():
    y = 1
    def clear():
        nonlocal y
        del y
    clear()
    return y
",
    );
    let h = History::new(&p);
    let x = stores(&p, Program::MODULE, "x");
    let load = loads(&p, Program::MODULE, "x")[0];
    assert_eq!(
        h.reaching(load).unwrap(),
        &BTreeSet::from([Def::Op(x[0]), Def::External])
    );
    let outer = region(&p, "outer");
    let y_load = loads(&p, outer, "y")[0];
    assert!(h.reaching(y_load).unwrap().contains(&Def::External));
}

#[test]
fn handler_name_is_deleted_on_every_exit() {
    let p = program(
        "\
def f():
    for i in xs:
        try:
            g()
        except E as e:
            if c:
                break
            return e
",
    );
    let f = region(&p, "f");
    let deletes = name_ops(&p, f, "e")
        .into_iter()
        .filter(|(_, _, a)| *a == Access::Delete)
        .count();
    // `break`, `return` (after evaluating `e`) and fallthrough (unreachable here but
    // still lowered) each delete the name.
    assert!(deletes >= 2, "{}", p.dump(f));
    let dump = p.dump(f);
    assert!(dump.contains("del e\n  return r"), "{dump}");
}

#[test]
fn generator_expression_receives_an_iterator() {
    let p = program("g = (e for e in xs)\n");
    let module = p.dump(Program::MODULE);
    assert!(module.contains("= iter r"), "{module}");
    let genexpr = p.dump(region(&p, "<genexpr>"));
    assert!(!genexpr.contains("= iter "), "{genexpr}");
}
