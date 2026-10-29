/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tests of the generic engine with small domains: value provenance through aliases,
//! a typestate domain with ambient context, convergence limits, branch pruning and
//! outer cells.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use pyrefly_static_analysis::*;

fn program(source: &str) -> Program {
    analyze_python(source, ModuleName::from_str("pkg.mod"), false)
}

fn region(p: &Program, name: &str) -> RegionId {
    RegionId(
        p.regions
            .iter()
            .position(|r| r.name.as_str() == name)
            .expect("region exists") as u32,
    )
}

fn line(p: &Program, start: u32) -> usize {
    p.line_col(TextRange::new(start.into(), start.into()))
        .0
        .get()
}

/// Tracks which operations created each value, so aliases share an origin.
/// Reports attribute stores into values created by an import.
struct Provenance;

impl Domain for Provenance {
    type Value = BTreeSet<OpId>;
    type State = ();
    type Diagnostic = (u32, BTreeSet<OpId>);

    fn top(&self) -> Self::Value {
        BTreeSet::new()
    }

    fn join(&self, a: &Self::Value, b: &Self::Value) -> Self::Value {
        a.union(b).copied().collect()
    }

    fn join_state(&self, _: &(), _: &()) {}

    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            // Names and parameters keep the engine's bookkeeping.
            OpKind::Name { .. } | OpKind::Param { .. } => {}
            OpKind::Attribute {
                object,
                access: Access::Store(_),
                ..
            } => {
                let origins = cx.reg(*object).clone();
                if !origins.is_empty() {
                    cx.report((cx.range().start().into(), origins));
                }
            }
            _ => {
                for dst in kind.writes() {
                    cx.set_reg(dst, BTreeSet::from([op]));
                }
            }
        }
    }
}

#[test]
fn assignment_copies_values_not_name_links() {
    let p = program(
        "\
import foo as b
a = b
b = {}
a.x = 1
b.y = 2
",
    );
    let import = OpId(
        p.ops
            .iter()
            .position(|op| matches!(op.kind, OpKind::Import { .. }))
            .unwrap() as u32,
    );
    let result = Engine::new(&p, &Provenance).analyze(Program::MODULE, Entry::new(()));
    assert!(!result.is_incomplete());
    let lines: Vec<(usize, bool)> = result
        .diagnostics
        .iter()
        .map(|(start, origins)| (line(&p, *start), origins.contains(&import)))
        .collect();
    // `a` still holds the imported module after `b` is rebound.
    assert_eq!(lines, vec![(4, true), (5, false)]);
}

#[test]
fn parameter_rebinding_and_outer_cells() {
    let p = program(
        "\
def f(p):
    global g
    q = p
    p = {}
    q.x = 1
    g = p
    return h
",
    );
    let f = region(&p, "f");
    let arg = OpId(1_000);
    let outer = OpId(2_000);
    // `h` is not defined in the module, so it resolves to a builtins symbol.
    let h = p
        .resolve(p.region(f).scope, &Name::new("h"))
        .expect("h is referenced");
    let g = p.global("g").expect("g is declared global");
    let mut entry = Entry::new(());
    entry.args = vec![BTreeSet::from([arg])];
    entry
        .outer
        .insert(h, Binding::bound(BTreeSet::from([outer])));
    let result = Engine::new(&p, &Provenance).analyze(f, entry);
    // `q` aliases the argument; rebinding `p` does not affect it.
    assert_eq!(result.diagnostics.len(), 1);
    assert_eq!(result.diagnostics[0].1, BTreeSet::from([arg]));
    // A load of a global reads the cell the caller provided.
    assert_eq!(result.returned, Some(BTreeSet::from([outer])));
    // A store to a global updates its cell, visible to the caller at exit.
    let exit = result.exit.expect("f returns");
    let g_value = &exit.outer()[&g].value;
    assert_eq!(g_value.len(), 1);
    assert!(matches!(
        p.op(*g_value.first().unwrap()).kind,
        OpKind::Build { .. }
    ));
}

/// A small typestate domain for file handles, with lock ownership as ambient context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Ref {
    OpenFn,
    LockFn,
    Lock,
    False,
    Handle(OpId),
    Method(OpId, Method),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Method {
    Read,
    Write,
    Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    Open,
    Closed,
    MaybeClosed,
}

#[derive(Debug, Clone, PartialEq)]
struct FileState {
    files: BTreeMap<OpId, Status>,
    /// The possible numbers of held locks: ambient context joined across paths.
    held: BTreeSet<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum FileError {
    UseAfterClose(u32),
    MaybeUseAfterClose(u32),
    WriteWithoutLock(u32),
}

struct Files;

impl Domain for Files {
    type Value = BTreeSet<Ref>;
    type State = FileState;
    type Diagnostic = FileError;

    fn top(&self) -> Self::Value {
        BTreeSet::new()
    }

    fn join(&self, a: &Self::Value, b: &Self::Value) -> Self::Value {
        a.union(b).copied().collect()
    }

    fn join_state(&self, a: &FileState, b: &FileState) -> FileState {
        let mut files = a.files.clone();
        for (h, s) in &b.files {
            let joined = match files.get(h) {
                None => *s,
                Some(old) if old == s => *s,
                Some(_) => Status::MaybeClosed,
            };
            files.insert(*h, joined);
        }
        FileState {
            files,
            held: a.held.union(&b.held).copied().collect(),
        }
    }

    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        let start: u32 = cx.range().start().into();
        match kind {
            OpKind::Name {
                symbol,
                access: Access::Load(dst),
            } if cx.program().owner(*symbol).is_none() => {
                let value = match cx.program().symbol(*symbol).name.as_str() {
                    "open" => BTreeSet::from([Ref::OpenFn]),
                    "lock" => BTreeSet::from([Ref::LockFn]),
                    _ => BTreeSet::new(),
                };
                cx.set_reg(*dst, value);
            }
            OpKind::Literal {
                value: Literal::Bool(false),
                dst,
            } => cx.set_reg(*dst, BTreeSet::from([Ref::False])),
            OpKind::Attribute {
                object,
                attr,
                access: Access::Load(dst),
            } => {
                let method = match attr.as_str() {
                    "read" => Method::Read,
                    "write" => Method::Write,
                    "close" => Method::Close,
                    _ => return,
                };
                let value = cx
                    .reg(*object)
                    .iter()
                    .filter_map(|r| match r {
                        Ref::Handle(h) => Some(Ref::Method(*h, method)),
                        _ => None,
                    })
                    .collect();
                cx.set_reg(*dst, value);
            }
            OpKind::Call { func, dst, .. } => {
                let callee = cx.reg(*func).clone();
                let mut result = BTreeSet::new();
                for r in callee {
                    match r {
                        Ref::OpenFn => {
                            cx.state_mut().files.insert(op, Status::Open);
                            result.insert(Ref::Handle(op));
                        }
                        Ref::LockFn => {
                            result.insert(Ref::Lock);
                        }
                        Ref::Method(h, Method::Close) => {
                            cx.state_mut().files.insert(h, Status::Closed);
                        }
                        Ref::Method(h, m) => {
                            match cx.state().files.get(&h) {
                                Some(Status::Closed) => cx.report(FileError::UseAfterClose(start)),
                                Some(Status::MaybeClosed) => {
                                    cx.report(FileError::MaybeUseAfterClose(start))
                                }
                                Some(Status::Open) | None => {}
                            }
                            if m == Method::Write && cx.state().held.contains(&0) {
                                cx.report(FileError::WriteWithoutLock(start));
                            }
                        }
                        Ref::Lock | Ref::False | Ref::Handle(_) => {}
                    }
                }
                cx.set_reg(*dst, result);
            }
            OpKind::ContextEnter { manager, .. } if cx.reg(*manager).contains(&Ref::Lock) => {
                let held = cx.state().held.iter().map(|n| n + 1).collect();
                cx.state_mut().held = held;
            }
            OpKind::ContextExit { manager, .. } if cx.reg(*manager).contains(&Ref::Lock) => {
                let held = cx.state().held.iter().map(|n| n - 1).collect();
                cx.state_mut().held = held;
            }
            _ => {}
        }
    }

    fn assume(&self, cond: Reg, truth: bool, cx: &mut Cx<'_, Self>) -> bool {
        !(truth && cx.reg(cond) == &BTreeSet::from([Ref::False]))
    }
}

fn files_entry() -> Entry<Files> {
    Entry::new(FileState {
        files: BTreeMap::new(),
        held: BTreeSet::from([0]),
    })
}

#[test]
fn typestate_with_ambient_context() {
    let p = program(
        "\
f = open(p)
if c:
    f.close()
f.read()
g = open(q)
with lock():
    g.write(1)
g.write(2)
g.close()
g.read()
if False:
    g.read()
",
    );
    let result = Engine::new(&p, &Files).analyze(Program::MODULE, files_entry());
    let found: Vec<(&str, usize)> = result
        .diagnostics
        .iter()
        .map(|d| match d {
            FileError::UseAfterClose(s) => ("closed", line(&p, *s)),
            FileError::MaybeUseAfterClose(s) => ("maybe closed", line(&p, *s)),
            FileError::WriteWithoutLock(s) => ("no lock", line(&p, *s)),
        })
        .collect();
    // Diagnostics are sorted by their `Ord`. Line 12 is pruned by `assume`:
    // diagnostics come only from reachable paths.
    assert_eq!(
        found,
        vec![("closed", 10), ("maybe closed", 4), ("no lock", 8)]
    );
    let exit = result.exit.expect("module completes");
    assert_eq!(exit.state.held, BTreeSet::from([0]));
}

#[test]
fn ambient_context_is_restored_on_return() {
    let p = program(
        "\
def h(c):
    k = open(r)
    with lock():
        if c:
            return k
    k.close()
    return None
",
    );
    let result = Engine::new(&p, &Files).analyze(region(&p, "h"), files_entry());
    let exit = result.exit.expect("h returns");
    assert_eq!(exit.state.held, BTreeSet::from([0]));
    // One path returns the open handle, the other closes it.
    assert_eq!(
        exit.state.files.values().collect::<Vec<_>>(),
        vec![&Status::MaybeClosed]
    );
    assert!(matches!(
        result.returned.as_ref().map(|v| v.iter().next().copied()),
        Some(Some(Ref::Handle(_)))
    ));
}

/// Tracks the exact set of integers a value may hold.
struct Counter {
    widen: bool,
}

impl Domain for Counter {
    /// `None` is any integer.
    type Value = Option<BTreeSet<i64>>;
    type State = ();
    type Diagnostic = ();

    fn top(&self) -> Self::Value {
        None
    }

    fn unbound(&self) -> Self::Value {
        Some(BTreeSet::new())
    }

    fn join(&self, a: &Self::Value, b: &Self::Value) -> Self::Value {
        Some(a.as_ref()?.union(b.as_ref()?).copied().collect())
    }

    fn join_state(&self, _: &(), _: &()) {}

    fn widen(&self, old: &Self::Value, new: &Self::Value) -> Self::Value {
        if self.widen && old != new {
            None
        } else {
            new.clone()
        }
    }

    fn transfer(&self, _: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            OpKind::Literal {
                value: Literal::Int(Some(n)),
                dst,
            } => cx.set_reg(*dst, Some(BTreeSet::from([*n]))),
            OpKind::Binary {
                op: BinaryOp::Arith(Operator::Add),
                left,
                right,
                dst,
            } => {
                let sum = match (cx.reg(*left), cx.reg(*right)) {
                    (Some(a), Some(b)) => Some(
                        a.iter()
                            .flat_map(|x| b.iter().map(move |y| x + y))
                            .collect(),
                    ),
                    _ => None,
                };
                cx.set_reg(*dst, sum);
            }
            _ => {}
        }
    }
}

#[test]
fn loop_convergence_needs_widening() {
    let p = program("i = 0\nwhile c:\n    i = i + 1\n");
    let i = p.global("i").unwrap();
    let limits = Limits {
        widen_after: 2,
        max_block_visits: 20,
    };
    let unbounded = Engine::new(&p, &Counter { widen: false })
        .with_limits(limits)
        .analyze(Program::MODULE, Entry::new(()));
    assert_eq!(unbounded.convergence, Convergence::LimitReached);
    assert!(unbounded.is_incomplete());

    let widened = Engine::new(&p, &Counter { widen: true })
        .with_limits(limits)
        .analyze(Program::MODULE, Entry::new(()));
    assert_eq!(widened.convergence, Convergence::Converged);
    let exit = widened.exit.expect("the loop can exit");
    assert_eq!(exit.symbol(&p, i), Some(&None));

    // Without a loop, values stay exact.
    let p = program("i = 1\nif c:\n    i = i + 1\n");
    let result = Engine::new(&p, &Counter { widen: true }).analyze(Program::MODULE, Entry::new(()));
    let i = p.global("i").unwrap();
    assert_eq!(
        result.exit.unwrap().symbol(&p, i),
        Some(&Some(BTreeSet::from([1, 2])))
    );
}

#[test]
fn reachable_gaps_and_replay() {
    let p = program(
        "\
try:
    x = 1
except E:
    x = 2
match x:
    case 1:
        pass
",
    );
    let engine = Engine::new(&p, &Provenance);
    let result = engine.analyze(Program::MODULE, Entry::new(()));
    let kinds: Vec<GapKind> = result.gaps.iter().map(|(k, _)| *k).collect();
    assert_eq!(kinds, vec![GapKind::Exceptions, GapKind::Match]);
    assert!(result.is_incomplete());
    // Replay visits each reachable operation once, in a deterministic order.
    let mut seen = Vec::new();
    engine.replay(&result, &mut |op, _| seen.push(op));
    let unique: BTreeSet<OpId> = seen.iter().copied().collect();
    assert_eq!(unique.len(), seen.len());
    let mut again = Vec::new();
    engine.replay(&result, &mut |op, _| again.push(op));
    assert_eq!(seen, again);
}

fn origins_by_line(p: &Program, result: &RegionResult<Provenance>) -> Vec<(usize, Vec<OpKindTag>)> {
    result
        .diagnostics
        .iter()
        .map(|(start, origins)| {
            (
                line(p, *start),
                origins.iter().map(|op| tag(p, *op)).collect(),
            )
        })
        .collect()
}

#[derive(Debug, PartialEq, Eq)]
enum OpKindTag {
    Import,
    Call(String),
    External(u32),
    Other,
}

fn tag(p: &Program, op: OpId) -> OpKindTag {
    match p.ops.get(op.index()).map(|o| &o.kind) {
        Some(OpKind::Import { .. }) => OpKindTag::Import,
        Some(OpKind::Call { .. }) => OpKindTag::Call(p.text(p.op(op).range).to_owned()),
        Some(_) => OpKindTag::Other,
        None => OpKindTag::External(op.0),
    }
}

#[test]
fn class_body_names_fall_back_to_the_module() {
    let p = program(
        "\
import registry
y = make()
class C:
    registry = registry
    registry.entries = 1
    if c:
        y = other()
    y.attr = 1
    del y
    y.z = 2
",
    );
    let result = Engine::new(&p, &Provenance).analyze(Program::MODULE, Entry::new(()));
    assert_eq!(
        origins_by_line(&p, &result),
        vec![
            // The class `registry` is unbound on the right-hand side: the module's is read.
            (5, vec![OpKindTag::Import]),
            // Possibly bound in the class: the join of the class and module bindings.
            (
                8,
                vec![
                    OpKindTag::Call("make()".to_owned()),
                    OpKindTag::Call("other()".to_owned())
                ]
            ),
            // After `del`, the module binding is visible again.
            (10, vec![OpKindTag::Call("make()".to_owned())]),
        ]
    );
}

#[test]
fn class_fallback_skips_enclosing_function_and_module_falls_back_to_builtins() {
    let p = program(
        "\
x = 0
def f():
    x = 1
    class D:
        x = x
        x.q = 1
len = len
len.q = 2
",
    );
    let f = region(&p, "f");
    let module_x = p.global("x").unwrap();
    let mut entry = Entry::new(());
    entry
        .outer
        .insert(module_x, Binding::bound(BTreeSet::from([OpId(5_000)])));
    let result = Engine::new(&p, &Provenance).analyze(f, entry);
    // `x = x` in a class body reads the module's `x`, not `f`'s local.
    assert_eq!(
        origins_by_line(&p, &result),
        vec![(6, vec![OpKindTag::External(5_000)])]
    );

    let builtin_len = p.symbol(p.global("len").unwrap()).fallback.unwrap();
    assert_eq!(p.symbol(builtin_len).origin, SymbolOrigin::Builtin);
    let mut entry = Entry::new(());
    entry
        .outer
        .insert(builtin_len, Binding::bound(BTreeSet::from([OpId(6_000)])));
    let result = Engine::new(&p, &Provenance).analyze(Program::MODULE, entry);
    assert_eq!(
        origins_by_line(&p, &result),
        vec![(8, vec![OpKindTag::External(6_000)])]
    );
    let exit = result.exit.unwrap();
    assert_eq!(
        exit.presence(&p, p.global("len").unwrap()),
        Some(Presence::Bound)
    );
}

/// Provenance plus calls: reports each call's callee origins, and applies a callee's
/// effect on outer cells at calls of one function.
struct Calls {
    function: Option<OpId>,
    effects: Vec<(SymbolId, Binding<BTreeSet<OpId>>)>,
}

impl Domain for Calls {
    type Value = BTreeSet<OpId>;
    type State = ();
    type Diagnostic = (u32, BTreeSet<OpId>);

    fn top(&self) -> Self::Value {
        BTreeSet::new()
    }

    fn join(&self, a: &Self::Value, b: &Self::Value) -> Self::Value {
        a.union(b).copied().collect()
    }

    fn join_state(&self, _: &(), _: &()) {}

    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            OpKind::Name { .. } | OpKind::Param { .. } => {}
            OpKind::Call { func, dst, .. } => {
                let callee = cx.reg(*func).clone();
                if self.function.is_some_and(|f| callee.contains(&f)) {
                    for (symbol, binding) in &self.effects {
                        cx.set_binding(*symbol, binding.clone());
                    }
                }
                cx.report((cx.range().start().into(), callee));
                cx.set_reg(*dst, BTreeSet::from([op]));
            }
            _ => {
                for dst in kind.writes() {
                    cx.set_reg(dst, BTreeSet::from([op]));
                }
            }
        }
    }
}

#[test]
fn deleted_outer_cells_fall_back_and_propagate_to_callers() {
    let p = program(
        "\
open = lambda *args: None
def f():
    global open
    del open
    open('inside')
f()
open('after')
",
    );
    let f = region(&p, "f");
    let module_open = p.global("open").unwrap();
    let builtin_open = p.symbol(module_open).fallback.unwrap();
    let builtin = BTreeSet::from([OpId(7_000)]);
    let no_calls = Calls {
        function: None,
        effects: Vec::new(),
    };

    // Inside `f`, the deleted global falls back to the builtin cell.
    let mut entry = Entry::new(());
    entry
        .outer
        .insert(module_open, Binding::bound(BTreeSet::from([OpId(1)])));
    entry
        .outer
        .insert(builtin_open, Binding::bound(builtin.clone()));
    let result = Engine::new(&p, &no_calls).analyze(f, entry);
    let callees: Vec<&BTreeSet<OpId>> = result.diagnostics.iter().map(|(_, c)| c).collect();
    assert_eq!(callees, vec![&builtin]);
    // The exit cell records the deletion: the caller sees `open` unbound, not unknown.
    let exit = result.exit.unwrap();
    assert_eq!(exit.outer()[&module_open].presence, Presence::Unbound);

    // Applying `f`'s effect at the module's call makes the later load fall back too.
    let make_f = OpId(
        p.ops
            .iter()
            .position(|op| matches!(op.kind, OpKind::MakeFunction { region, .. } if region == f))
            .unwrap() as u32,
    );
    let calls = Calls {
        function: Some(make_f),
        effects: vec![(module_open, exit.outer()[&module_open].clone())],
    };
    let mut entry = Entry::new(());
    entry
        .outer
        .insert(builtin_open, Binding::bound(builtin.clone()));
    let result = Engine::new(&p, &calls).analyze(Program::MODULE, entry);
    let last = result
        .diagnostics
        .iter()
        .max_by_key(|(start, _)| *start)
        .unwrap();
    assert_eq!(line(&p, last.0), 7);
    assert_eq!(last.1, builtin);
    let exit = result.exit.unwrap();
    assert_eq!(exit.presence(&p, module_open), Some(Presence::Unbound));
}

#[test]
fn unknown_outer_cells_are_not_unbound() {
    // With no cell for the global, a load is unknown (`top`), not a fallback.
    let p = program("def f():\n    return g\n");
    let f = region(&p, "f");
    let result = Engine::new(&p, &Provenance).analyze(f, Entry::new(()));
    assert_eq!(result.returned, Some(BTreeSet::new()));
    assert!(result.exit.unwrap().outer().is_empty());
}
