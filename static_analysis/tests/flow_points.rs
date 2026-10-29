/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;
use std::marker::PhantomData;

use pyrefly_python::ast::Ast;
use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::analyze_python;
use pyrefly_static_analysis::analyze_python_with_flow_points;
use pyrefly_static_analysis::flow_point::FlowPointCollector;
use pyrefly_static_analysis::flow_point::FlowPointId;
use pyrefly_static_analysis::flow_point::FlowPointKind;
use pyrefly_static_analysis::structured;
use pyrefly_static_analysis::structured::Arm;
use pyrefly_static_analysis::structured::Borrowed;
use pyrefly_static_analysis::structured::Driver;
use pyrefly_static_analysis::structured::Jump;
use pyrefly_static_analysis::structured::LoopHeader;
use pyrefly_static_analysis::structured::Owned;
use pyrefly_static_analysis::structured::Structured;
use pyrefly_static_analysis::structured::Syntax;
use ruff_python_ast::Expr;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Stmt;
use ruff_text_size::TextRange;

trait TestSyntax: Syntax {
    fn expr(expr: &Self::Expr) -> &Expr;
    fn nested(trace: &mut Trace<Self>, stmt: Self::Stmt);
}

impl TestSyntax for Owned {
    fn expr(expr: &Expr) -> &Expr {
        expr
    }

    fn nested(trace: &mut Trace<Self>, stmt: Stmt) {
        match stmt {
            Stmt::FunctionDef(f) => structured::region(trace, f.body),
            Stmt::ClassDef(c) => structured::suite(trace, c.body),
            Stmt::With(w) => structured::suite(trace, w.body),
            _ => {}
        }
    }
}

impl<'a> TestSyntax for Borrowed<'a> {
    fn expr<'b>(expr: &'b &'a Expr) -> &'b Expr {
        expr
    }

    fn nested(trace: &mut Trace<Self>, stmt: &'a Stmt) {
        match stmt {
            Stmt::FunctionDef(f) => structured::region(trace, &f.body),
            Stmt::ClassDef(c) => structured::suite(trace, &c.body),
            Stmt::With(w) => structured::suite(trace, &w.body),
            _ => {}
        }
    }
}

struct Trace<S: TestSyntax> {
    driver: Driver<()>,
    points: FlowPointCollector,
    prune: bool,
    observe: bool,
    syntax: PhantomData<S>,
}

impl<S: TestSyntax> Trace<S> {
    fn new(prune: bool, observe: bool) -> Self {
        Self {
            driver: Driver::default(),
            points: FlowPointCollector::default(),
            prune,
            observe,
            syntax: PhantomData,
        }
    }
}

impl<S: TestSyntax> Structured for Trace<S> {
    type Syntax = S;
    type Suite = ();
    type If = ();
    type Loop = ();
    type Target = ();

    fn driver(&mut self) -> &mut Driver<()> {
        &mut self.driver
    }
    fn observes_flow_points(&self) -> bool {
        self.observe
    }
    fn flow_point(&mut self, point: FlowPointId) {
        self.points.observe(point);
    }
    fn suite_start(&mut self, _: &[Stmt]) {}
    fn absorbs(&mut self, _: &Stmt, _: &Stmt) -> bool {
        false
    }
    fn stmt_before(&mut self, _: &mut (), _: &Stmt, _: Option<&Stmt>) {}
    fn stmt_after(&mut self, _: &mut ()) {}
    fn suite_end(&mut self, _: ()) {}
    fn other_stmt(&mut self, stmt: S::Stmt) {
        S::nested(self, stmt);
    }
    fn if_start(&mut self, _: TextRange, _: &mut S::Expr) {}
    fn arm_start(
        &mut self,
        _: &mut (),
        _: TextRange,
        _: bool,
        test: Option<&mut S::Expr>,
    ) -> Option<bool> {
        if self.prune {
            test.and_then(|test| match S::expr(test) {
                Expr::BooleanLiteral(b) => Some(b.value),
                _ => None,
            })
        } else {
            None
        }
    }
    fn arm_skip(&mut self, _: &mut (), _: Arm<S>) {}
    fn arm_enter(&mut self, _: &mut (), _: TextRange, _: Option<S::Expr>, _: &[Stmt]) {}
    fn arm_exit(&mut self, _: &mut (), _: TextRange) {}
    fn arm_dead(&mut self, _: &mut (), _: Arm<S>) {}
    fn if_end(&mut self, _: (), _: bool) {}
    fn loop_start(&mut self, _: LoopHeader<S>, _: &[Stmt]) -> ((), ()) {
        ((), ())
    }
    fn loop_else(&mut self, _: &mut ()) {}
    fn loop_end(&mut self, _: ()) {}
    fn loop_jump(&mut self, _: Jump, _: TextRange, _: Option<()>) {}
}

const SOURCE: &str = r#"
if False:
    def omitted():
        while c:
            break
elif flag:
    value = 1
else:
    value = 2

def outer():
    while running:
        def inner():
            for item in items:
                if item:
                    continue
                else:
                    break
            else:
                pass
        if True:
            pass
        elif c:
            while other:
                pass
        break
    else:
        pass
    class C:
        if c:
            pass
    def sibling():
        while c:
            pass
"#;

fn owned(source: &str, prune: bool) -> Vec<FlowPointId> {
    let (ast, errors, _) = Ast::parse(source, PySourceType::Python);
    assert!(errors.is_empty(), "{errors:?}");
    let mut trace = Trace::<Owned>::new(prune, true);
    structured::suite(&mut trace, ast.body);
    trace.points.points
}

#[test]
fn owned_and_borrowed_source_ids_agree_and_repeat_parse() {
    let (ast, errors, _) = Ast::parse(SOURCE, PySourceType::Python);
    assert!(errors.is_empty());
    let mut trace = Trace::<Borrowed>::new(false, true);
    structured::suite(&mut trace, &ast.body);
    let full = owned(SOURCE, false);
    assert_eq!(trace.points.points, full);
    assert_eq!(owned(SOURCE, false), full);
    assert_eq!(full.iter().collect::<HashSet<_>>().len(), full.len());
    for kind in [
        FlowPointKind::ScopeEntry,
        FlowPointKind::ScopeExit,
        FlowPointKind::ArmTest,
        FlowPointKind::ArmEntry,
        FlowPointKind::ArmExit,
        FlowPointKind::IfJoin,
        FlowPointKind::LoopBody,
        FlowPointKind::LoopBackedge,
        FlowPointKind::LoopExit,
        FlowPointKind::LoopElse,
        FlowPointKind::LoopJoin,
        FlowPointKind::Continue,
        FlowPointKind::Break,
    ] {
        assert!(full.iter().any(|p| p.kind == kind), "{kind:?}");
    }
    assert!(full.iter().map(|p| p.region).collect::<HashSet<_>>().len() >= 5);
}

#[test]
fn pruning_keeps_surviving_source_ids_without_visit_ordinals() {
    let full = owned(SOURCE, false);
    let pruned = owned(SOURCE, true);
    assert!(pruned.len() < full.len());
    let full: HashSet<_> = full.into_iter().collect();
    assert!(pruned.iter().all(|p| full.contains(p)));
    let (ast, _, _) = Ast::parse(SOURCE, PySourceType::Python);
    let mut trace = Trace::<Borrowed>::new(true, true);
    structured::suite(&mut trace, &ast.body);
    assert_eq!(trace.points.points, pruned);
}

#[test]
fn actual_cfg_backend_emits_the_shared_ids_without_changing_lowering() {
    let observed = analyze_python_with_flow_points(SOURCE, ModuleName::from_str("main"), false);
    assert_eq!(observed.flow_points, owned(SOURCE, false));
    let plain = analyze_python(SOURCE, ModuleName::from_str("main"), false);
    assert_eq!(
        format!("{:?}", observed.program.ops),
        format!("{:?}", plain.ops)
    );
    assert_eq!(
        format!("{:?}", observed.program.regions),
        format!("{:?}", plain.regions)
    );
}

#[test]
fn finally_clones_repeat_source_occurrences_not_runtime_instance_ids() {
    let source = r#"
def f(flag):
    try:
        if flag:
            return
    finally:
        if flag:
            pass
"#;
    let observed = analyze_python_with_flow_points(source, ModuleName::from_str("main"), false);
    let exits: Vec<_> = observed
        .flow_points
        .iter()
        .filter(|p| p.kind == FlowPointKind::ArmExit)
        .collect();
    assert!(exits.iter().collect::<HashSet<_>>().len() < exits.len());
    let repeated = analyze_python_with_flow_points(source, ModuleName::from_str("main"), false);
    assert_eq!(observed.flow_points, repeated.flow_points);
}

#[test]
fn unobserved_and_empty_suites_emit_nothing() {
    let (ast, _, _) = Ast::parse(SOURCE, PySourceType::Python);
    let mut trace = Trace::<Owned>::new(false, false);
    structured::suite(&mut trace, ast.body);
    assert!(trace.points.points.is_empty());
    assert_eq!(trace.points.points.capacity(), 0);
    assert!(owned("", false).is_empty());
    // Driver state is restored after the outermost suite, even on reuse.
    trace.observe = true;
    let (ast, _, _) = Ast::parse("if c:\n    pass\n", PySourceType::Python);
    structured::suite(&mut trace, ast.body);
    assert_eq!(trace.points.points, owned("if c:\n    pass\n", false));
}
