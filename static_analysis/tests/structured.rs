/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The structured driver with a tracing backend that owns the AST and decides tests
//! named `T` and `F` statically.

use pyrefly_python::ast::Ast;
use pyrefly_static_analysis::structured;
use pyrefly_static_analysis::structured::Arm;
use pyrefly_static_analysis::structured::Coverage;
use pyrefly_static_analysis::structured::Driver;
use pyrefly_static_analysis::structured::Jump;
use pyrefly_static_analysis::structured::LoopHeader;
use pyrefly_static_analysis::structured::Owned;
use pyrefly_static_analysis::structured::Structured;
use ruff_python_ast::Expr;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Stmt;
use ruff_text_size::TextRange;

#[derive(Default)]
struct Trace {
    driver: Driver<usize>,
    events: Vec<String>,
    /// Loop ids are assigned in source order.
    next_loop: usize,
}

fn text(x: &Expr) -> String {
    match x {
        Expr::Name(n) => n.id.to_string(),
        Expr::Call(c) => format!("{}()", text(&c.func)),
        _ => "?".to_owned(),
    }
}

impl Trace {
    fn log(&mut self, event: impl Into<String>) {
        self.events.push(event.into());
    }
}

impl Structured for Trace {
    type Syntax = Owned;
    type Suite = ();
    type If = ();
    type Loop = usize;
    type Target = usize;

    fn driver(&mut self) -> &mut Driver<usize> {
        &mut self.driver
    }

    fn suite_start(&mut self, _: &[Stmt]) {}

    fn absorbs(&mut self, stmt: &Stmt, _: &Stmt) -> bool {
        matches!(stmt, Stmt::Expr(e) if text(&e.value) == "merge_next()")
    }

    fn stmt_before(&mut self, _: &mut (), stmt: &Stmt, next: Option<&Stmt>) {
        if matches!(stmt, Stmt::Expr(e) if text(&e.value) == "merge_next()") {
            self.log(format!("next after merge: {}", next.is_some()));
        }
    }

    fn stmt_after(&mut self, _: &mut ()) {}

    fn suite_end(&mut self, _: ()) {}

    fn other_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                let event = text(&e.value);
                self.log(event);
            }
            Stmt::FunctionDef(f) => {
                self.log(format!("def {}", f.name.id));
                structured::region(self, f.body);
                self.log(format!("end def {}", f.name.id));
            }
            Stmt::With(w) => {
                self.log("with");
                structured::suite(self, w.body);
            }
            _ => self.log("other"),
        }
    }

    fn if_start(&mut self, _: TextRange, first_test: &mut Expr) {
        self.log(format!("if {}", text(first_test)));
    }

    fn arm_start(
        &mut self,
        _: &mut (),
        _: TextRange,
        first: bool,
        test: Option<&mut Expr>,
    ) -> Option<bool> {
        let test = test.map(|t| text(t));
        self.log(format!("arm {test:?} first={first}"));
        match test.as_deref() {
            Some("T") => Some(true),
            Some("F") => Some(false),
            _ => None,
        }
    }

    fn arm_skip(&mut self, _: &mut (), arm: Arm<Owned>) {
        self.log(format!("skip {:?}", arm.test.as_ref().map(text)));
    }

    fn arm_enter(&mut self, _: &mut (), _: TextRange, _: Option<Expr>, _: &[Stmt]) {}

    fn arm_exit(&mut self, _: &mut (), _: TextRange) {
        self.log("arm end");
    }

    fn arm_dead(&mut self, _: &mut (), arm: Arm<Owned>) {
        self.log(format!("dead {:?}", arm.test.as_ref().map(text)));
    }

    fn if_end(&mut self, _: (), exhaustive: bool) {
        self.log(format!("end if exhaustive={exhaustive}"));
    }

    fn loop_start(&mut self, header: LoopHeader<Owned>, _: &[Stmt]) -> (usize, usize) {
        let id = self.next_loop;
        self.next_loop += 1;
        let kind = match header {
            LoopHeader::While { test, .. } => format!("while {}", text(&test)),
            LoopHeader::For { iter, .. } => format!("for in {}", text(&iter)),
        };
        self.log(format!("{kind} #{id}"));
        (id, id)
    }

    fn loop_else(&mut self, id: &mut usize) {
        self.log(format!("else #{id}"));
    }

    fn loop_end(&mut self, id: usize) {
        self.log(format!("end loop #{id}"));
    }

    fn loop_jump(&mut self, jump: Jump, _: TextRange, target: Option<usize>) {
        self.log(format!("{jump:?} -> {target:?}"));
    }
}

fn drive(source: &str) -> (Vec<String>, Coverage) {
    let (ast, errors, _) = Ast::parse(source, PySourceType::Python);
    assert!(errors.is_empty(), "{errors:?}");
    let mut trace = Trace::default();
    structured::suite(&mut trace, ast.body);
    (trace.events, trace.driver.coverage)
}

#[test]
fn static_truth_prunes_arms_and_decides_exhaustiveness() {
    let (events, coverage) = drive(
        r#"
if c:
    a()
elif F:
    b()
elif T:
    d()
elif e:
    f()
else:
    g()
if c:
    h()
"#,
    );
    assert_eq!(
        events,
        [
            "if c",
            "arm Some(\"c\") first=true",
            "a()",
            "arm end",
            "arm Some(\"F\") first=false",
            "skip Some(\"F\")",
            "arm Some(\"T\") first=false",
            "d()",
            "arm end",
            "dead Some(\"e\")",
            "dead None",
            "end if exhaustive=true",
            "if c",
            "arm Some(\"c\") first=true",
            "h()",
            "arm end",
            "end if exhaustive=false",
        ]
    );
    assert_eq!(coverage.arms_visited, 3);
    assert_eq!(coverage.arms_skipped, 1);
    assert_eq!(coverage.arms_dead, 2);
}

#[test]
fn jumps_target_the_innermost_loop_of_their_region() {
    let (events, coverage) = drive(
        r#"
break
while c:
    for x in xs:
        with m:
            continue
    break
    def f():
        break
else:
    break
"#,
    );
    assert_eq!(
        events,
        [
            "Break -> None",
            "while c #0",
            "for in xs #1",
            "with",
            "Continue -> Some(1)",
            "else #1",
            "end loop #1",
            "Break -> Some(0)",
            "def f",
            "Break -> None",
            "end def f",
            "else #0",
            "Break -> None",
            "end loop #0",
        ]
    );
    assert_eq!(coverage.jumps, 5);
    assert_eq!(coverage.jumps_outside_loop, 3);
    assert_eq!(coverage.regions, 1);
}

#[test]
fn a_backend_can_absorb_the_next_statement() {
    let (events, coverage) = drive("merge_next()\ndropped()\nkept()\nmerge_next()\ndropped()\n");
    assert_eq!(
        events,
        [
            "next after merge: true",
            "merge_next()",
            "kept()",
            "next after merge: false",
            "merge_next()",
        ]
    );
    assert_eq!(coverage.other, 3);
}
