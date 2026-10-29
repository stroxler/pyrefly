/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A generic forward dataflow engine over one region of a [`Program`].
//!
//! The engine owns the shared bookkeeping: a [`Frame`] holding the value of every symbol
//! the region owns, a cell for every outer symbol (closure variable, global or builtin)
//! the region has seen, the value of every temporary, and an arbitrary domain-owned state
//! (ambient context, typestate, heap summaries, ...). A [`Domain`] supplies the lattice
//! and a transfer function per operation; it never walks syntax.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::Debug;

use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::SmallMap;

use crate::ir::Access;
use crate::ir::BlockId;
use crate::ir::GapKind;
use crate::ir::OpId;
use crate::ir::OpKind;
use crate::ir::Program;
use crate::ir::RaiseCause;
use crate::ir::Reg;
use crate::ir::RegionId;
use crate::ir::SymbolId;
use crate::ir::Terminator;

/// An abstract interpretation of Python operations.
///
/// The engine calls [`Domain::transfer`] for every reachable operation with the frame as
/// it was before the operation. Afterwards, any result temporary the domain did not set
/// receives a default value, and a name `Store`/`Delete` the domain did not override
/// updates the frame:
///
/// - `Name` load: see [`load`]: the symbol's slot (owned symbols, falling back as
///   CPython does while unbound) or cell (other symbols, `top` if absent).
/// - `Param`: the value in [`Entry::args`], else `top`.
/// - `Name` store: the stored temporary's value. `Name` delete: `unbound`.
/// - Every other result: `top`.
///
/// The defaults never claim anything about effects: an unhandled `Call`, `Attribute` or
/// `Subscript` simply produces `top`, and a domain that tracks effects must treat
/// unknown callees and protocol methods conservatively itself.
pub trait Domain {
    type Value: Clone + PartialEq + Debug;
    /// Domain-owned state carried along every path and joined at merges.
    type State: Clone + PartialEq + Debug;
    /// Diagnostics are collected only from reachable operations after the fixpoint,
    /// then sorted and deduplicated.
    type Diagnostic: Clone + Ord + Debug;

    /// The value of anything the domain knows nothing about.
    fn top(&self) -> Self::Value;

    /// The value of a symbol before any binding (and after `del`).
    fn unbound(&self) -> Self::Value {
        self.top()
    }

    fn join(&self, a: &Self::Value, b: &Self::Value) -> Self::Value;

    fn join_state(&self, a: &Self::State, b: &Self::State) -> Self::State;

    /// Applied at loop heads, once `Limits::widen_after` visits have happened, to the
    /// old entry value and the joined new one.
    fn widen(&self, old: &Self::Value, new: &Self::Value) -> Self::Value {
        self.join(old, new)
    }

    fn widen_state(&self, old: &Self::State, new: &Self::State) -> Self::State {
        self.join_state(old, new)
    }

    /// Interpret one operation. The default does nothing beyond the shared bookkeeping.
    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        let _ = (op, kind, cx);
    }

    /// Refine the frame on a `Branch` edge where `cond` has truthiness `truth`. Return
    /// `false` if the edge is infeasible.
    fn assume(&self, cond: Reg, truth: bool, cx: &mut Cx<'_, Self>) -> bool {
        let _ = (cond, truth, cx);
        true
    }
}

/// Whether a symbol holds a binding, tracked independently of the domain's values so
/// that "unknown value" and "no binding" stay distinct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Presence {
    Bound,
    Unbound,
    /// Bound on some paths only, or not known.
    Maybe,
}

impl Presence {
    fn join(self, other: Self) -> Self {
        if self == other { self } else { Presence::Maybe }
    }
}

/// The binding state of one symbol: an owned slot or an outer cell.
#[derive(Debug, Clone, PartialEq)]
pub struct Binding<V> {
    pub presence: Presence,
    /// The bound value; meaningful when `presence` is not `Unbound`.
    pub value: V,
}

impl<V> Binding<V> {
    pub fn bound(value: V) -> Self {
        Self {
            presence: Presence::Bound,
            value,
        }
    }
}

/// The full abstract state at a program point.
pub struct Frame<D: Domain + ?Sized> {
    region: RegionId,
    slots: Vec<Binding<D::Value>>,
    outer: BTreeMap<SymbolId, Binding<D::Value>>,
    /// Only temporaries that may still be read: block entry frames keep just the
    /// temporaries live into the block.
    regs: BTreeMap<Reg, D::Value>,
    pub state: D::State,
}

impl<D: Domain + ?Sized> Clone for Frame<D> {
    fn clone(&self) -> Self {
        Self {
            region: self.region,
            slots: self.slots.clone(),
            outer: self.outer.clone(),
            regs: self.regs.clone(),
            state: self.state.clone(),
        }
    }
}

impl<D: Domain + ?Sized> PartialEq for Frame<D> {
    fn eq(&self, other: &Self) -> bool {
        self.slots == other.slots
            && self.outer == other.outer
            && self.regs == other.regs
            && self.state == other.state
    }
}

impl<D: Domain + ?Sized> Debug for Frame<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Frame")
            .field("slots", &self.slots)
            .field("outer", &self.outer)
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}

impl<D: Domain + ?Sized> Frame<D> {
    /// The value of a temporary; panics if the temporary is not set on this path.
    pub fn reg(&self, reg: Reg) -> &D::Value {
        self.regs
            .get(&reg)
            .expect("lowering defines every temporary before use on each path")
    }

    /// The binding of a symbol: its slot if the region owns it, else its cell if this
    /// activation has one. A missing cell means nothing is known about the symbol.
    pub fn binding(&self, program: &Program, symbol: SymbolId) -> Option<&Binding<D::Value>> {
        match self.slot(program, symbol) {
            Some(slot) => Some(&self.slots[slot]),
            None => self.outer.get(&symbol),
        }
    }

    pub fn presence(&self, program: &Program, symbol: SymbolId) -> Option<Presence> {
        self.binding(program, symbol).map(|b| b.presence)
    }

    /// The raw value of a symbol's binding. See [`load`] for what a load reads,
    /// including fallbacks.
    pub fn symbol(&self, program: &Program, symbol: SymbolId) -> Option<&D::Value> {
        self.binding(program, symbol).map(|b| &b.value)
    }

    /// The cells of symbols owned by other regions: the entry's [`Entry::outer`] cells
    /// plus every store or delete this activation performed on such symbols. Comparing
    /// an exit frame's cells with the entry's gives a callee's effect on its caller.
    pub fn outer(&self) -> &BTreeMap<SymbolId, Binding<D::Value>> {
        &self.outer
    }

    fn slot(&self, program: &Program, symbol: SymbolId) -> Option<usize> {
        let s = program.symbol(symbol);
        match s.slot {
            Some(slot) if program.scope(s.scope).region == self.region => Some(slot as usize),
            _ => None,
        }
    }

    fn write(&mut self, program: &Program, symbol: SymbolId, binding: Binding<D::Value>) {
        match self.slot(program, symbol) {
            Some(slot) => self.slots[slot] = binding,
            None => {
                self.outer.insert(symbol, binding);
            }
        }
    }

    /// Join (or widen) `other` into `self`.
    fn merge(&mut self, other: &Self, domain: &D, widen: bool) {
        let value = |a: &D::Value, b: &D::Value| {
            let joined = domain.join(a, b);
            if widen {
                domain.widen(a, &joined)
            } else {
                joined
            }
        };
        let binding = |a: &Binding<D::Value>, b: &Binding<D::Value>| Binding {
            presence: a.presence.join(b.presence),
            value: value(&a.value, &b.value),
        };
        for (a, b) in self.slots.iter_mut().zip(&other.slots) {
            *a = binding(a, b);
        }
        // A temporary set on only one incoming path is dead here.
        self.regs.retain(|r, _| other.regs.contains_key(r));
        for (r, a) in self.regs.iter_mut() {
            *a = value(a, &other.regs[r]);
        }
        // A cell missing on one side is unknown there.
        let unknown = Binding {
            presence: Presence::Maybe,
            value: domain.top(),
        };
        for (symbol, b) in &other.outer {
            let joined = binding(self.outer.get(symbol).unwrap_or(&unknown), b);
            self.outer.insert(*symbol, joined);
        }
        for (symbol, a) in self.outer.iter_mut() {
            if !other.outer.contains_key(symbol) {
                *a = binding(a, &unknown);
            }
        }
        let joined = domain.join_state(&self.state, &other.state);
        self.state = if widen {
            domain.widen_state(&self.state, &joined)
        } else {
            joined
        };
    }
}

/// The context handed to [`Domain::transfer`] and [`Domain::assume`].
pub struct Cx<'a, D: Domain + ?Sized> {
    program: &'a Program,
    domain: &'a D,
    range: TextRange,
    op: Option<OpId>,
    frame: &'a mut Frame<D>,
    diagnostics: Option<&'a mut BTreeSet<D::Diagnostic>>,
    set_regs: Vec<Reg>,
    set_symbol: bool,
}

impl<'a, D: Domain + ?Sized> Cx<'a, D> {
    pub fn program(&self) -> &'a Program {
        self.program
    }

    pub fn region(&self) -> RegionId {
        self.frame.region
    }

    /// The range of the current operation (or branch condition).
    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn reg(&self, reg: Reg) -> &D::Value {
        self.frame.reg(reg)
    }

    /// Set a result temporary, suppressing its default value.
    pub fn set_reg(&mut self, reg: Reg, value: D::Value) {
        self.frame.regs.insert(reg, value);
        self.set_regs.push(reg);
    }

    /// The value a load of `symbol` produces by default: see [`load`].
    pub fn load(&self, symbol: SymbolId) -> D::Value {
        load(self.program, self.domain, self.frame, symbol)
    }

    /// Set a symbol's slot or cell. While transferring a `Name` store or delete of the
    /// same symbol, this replaces the default update.
    pub fn set_binding(&mut self, symbol: SymbolId, binding: Binding<D::Value>) {
        if let Some(op) = self.op
            && let OpKind::Name {
                symbol: s,
                access: Access::Store(_) | Access::Delete,
            } = self.program.op(op).kind
            && s == symbol
        {
            self.set_symbol = true;
        }
        self.frame.write(self.program, symbol, binding);
    }

    /// Bind a symbol to `value`, as [`Cx::set_binding`] does.
    pub fn set_symbol(&mut self, symbol: SymbolId, value: D::Value) {
        self.set_binding(symbol, Binding::bound(value));
    }

    pub fn state(&self) -> &D::State {
        &self.frame.state
    }

    pub fn state_mut(&mut self) -> &mut D::State {
        &mut self.frame.state
    }

    pub fn frame(&self) -> &Frame<D> {
        self.frame
    }

    /// Report a diagnostic. Ignored until the final pass over the fixpoint states.
    pub fn report(&mut self, diagnostic: D::Diagnostic) {
        if let Some(d) = &mut self.diagnostics {
            d.insert(diagnostic);
        }
    }
}

/// The starting point of one region analysis.
pub struct Entry<D: Domain + ?Sized> {
    /// Argument values by parameter index; missing entries are `top`.
    pub args: Vec<D::Value>,
    /// Initial cells for symbols owned by other regions (closure cells, globals and
    /// builtins) as this activation sees them. A missing cell is unknown: loads of it
    /// read `top`. Seed builtin cells to give builtins values.
    pub outer: SmallMap<SymbolId, Binding<D::Value>>,
    pub state: D::State,
}

impl<D: Domain + ?Sized> Entry<D> {
    pub fn new(state: D::State) -> Self {
        Self {
            args: Vec::new(),
            outer: SmallMap::new(),
            state,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Limits {
    /// Visits of a loop head before joins there switch to widening.
    pub widen_after: u32,
    /// Visits of any single block before the analysis gives up.
    pub max_block_visits: u32,
}

impl Default for Limits {
    fn default() -> Self {
        Self {
            widen_after: 3,
            max_block_visits: 64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Convergence {
    Converged,
    /// The visit limit was hit; states, exits and diagnostics are incomplete.
    LimitReached,
}

/// A reachable exit by raising, with the frame there.
pub struct RaiseSite<D: Domain + ?Sized> {
    pub cause: RaiseCause,
    pub range: TextRange,
    pub frame: Frame<D>,
}

impl<D: Domain + ?Sized> Debug for RaiseSite<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RaiseSite")
            .field("cause", &self.cause)
            .field("range", &self.range)
            .field("frame", &self.frame)
            .finish()
    }
}

/// The outcome of analyzing one region.
pub struct RegionResult<D: Domain + ?Sized> {
    pub region: RegionId,
    pub convergence: Convergence,
    /// The join of frames at normal `Return`s; `None` if no normal exit is reachable.
    pub exit: Option<Frame<D>>,
    /// The join of returned values (function regions only).
    pub returned: Option<D::Value>,
    /// The join of frames at reachable `Raise` terminators that leave the region.
    pub raised: Option<Frame<D>>,
    /// Each reachable `Raise` terminator, in reverse postorder of its block.
    pub raise_sites: Vec<RaiseSite<D>>,
    pub diagnostics: Vec<D::Diagnostic>,
    /// Gaps on reachable operations, deduplicated and in source order.
    pub gaps: Vec<(GapKind, TextRange)>,
    args: Vec<D::Value>,
    block_entries: Vec<Option<Frame<D>>>,
}

impl<D: Domain + ?Sized> Debug for RegionResult<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RegionResult")
            .field("region", &self.region)
            .field("convergence", &self.convergence)
            .field("exit", &self.exit)
            .field("returned", &self.returned)
            .field("diagnostics", &self.diagnostics)
            .field("gaps", &self.gaps)
            .finish_non_exhaustive()
    }
}

impl<D: Domain + ?Sized> RegionResult<D> {
    /// True if the result is not a complete claim: unconverged, or a gap is reachable.
    pub fn is_incomplete(&self) -> bool {
        self.convergence != Convergence::Converged || !self.gaps.is_empty()
    }

    pub fn is_reachable(&self, op: OpId, program: &Program) -> bool {
        let op = program.op(op);
        op.region == self.region && self.block_entries[op.block.index()].is_some()
    }
}

pub struct Engine<'a, D: Domain + ?Sized> {
    program: &'a Program,
    domain: &'a D,
    limits: Limits,
}

/// A callback that sees each operation with the frame before it.
type Visit<'b, D> = &'b mut dyn FnMut(OpId, &Frame<D>);

/// Where a block run reports what it sees.
struct Observer<'b, D: Domain + ?Sized> {
    diagnostics: Option<&'b mut BTreeSet<D::Diagnostic>>,
    gaps: Option<&'b mut BTreeSet<(TextSize, TextSize, GapKind)>>,
    visit: Option<Visit<'b, D>>,
}

impl<'a, D: Domain + ?Sized> Engine<'a, D> {
    pub fn new(program: &'a Program, domain: &'a D) -> Self {
        Self {
            program,
            domain,
            limits: Limits::default(),
        }
    }

    pub fn with_limits(mut self, limits: Limits) -> Self {
        self.limits = limits;
        self
    }

    /// Run the region to a fixpoint, then replay reachable blocks once to collect
    /// exits, diagnostics and gaps.
    pub fn analyze(&self, region: RegionId, entry: Entry<D>) -> RegionResult<D> {
        let r = self.program.region(region);
        let order = self.program.region(region).reverse_postorder();
        let mut rank = vec![usize::MAX; r.blocks.len()];
        for (i, b) in order.iter().enumerate() {
            rank[b.index()] = i;
        }
        let initial = Frame {
            region,
            slots: vec![
                Binding {
                    presence: Presence::Unbound,
                    value: self.domain.unbound(),
                };
                r.num_slots as usize
            ],
            outer: entry.outer.into_iter().collect(),
            regs: BTreeMap::new(),
            state: entry.state,
        };
        let args = entry.args;
        let live = live_temporaries(self.program, region);
        let mut entries: Vec<Option<Frame<D>>> = vec![None; r.blocks.len()];
        entries[r.entry.index()] = Some(initial);
        let mut visits = vec![0u32; r.blocks.len()];
        let mut worklist = BTreeSet::from([rank[r.entry.index()]]);
        let mut convergence = Convergence::Converged;
        while let Some(i) = worklist.pop_first() {
            let block = order[i];
            visits[block.index()] += 1;
            if visits[block.index()] > self.limits.max_block_visits {
                convergence = Convergence::LimitReached;
                break;
            }
            let mut frame = entries[block.index()]
                .clone()
                .expect("only blocks with an entry frame are queued");
            let mut observer = Observer {
                diagnostics: None,
                gaps: None,
                visit: None,
            };
            for (target, mut out) in self.run_block(block, &mut frame, &args, &mut observer) {
                let t = target.index();
                out.regs.retain(|r, _| live[t].contains(r));
                let changed = match &mut entries[t] {
                    None => {
                        entries[t] = Some(out);
                        true
                    }
                    Some(old) => {
                        // A loop head is a block reached by an edge that does not go
                        // forward in reverse postorder.
                        let widen = rank[t] <= i && visits[t] >= self.limits.widen_after;
                        let before = old.clone();
                        old.merge(&out, self.domain, widen);
                        *old != before
                    }
                };
                if changed {
                    worklist.insert(rank[t]);
                }
            }
        }

        let mut result = RegionResult {
            region,
            convergence,
            exit: None,
            returned: None,
            raised: None,
            raise_sites: Vec::new(),
            diagnostics: Vec::new(),
            gaps: Vec::new(),
            args,
            block_entries: entries,
        };
        let mut diagnostics = BTreeSet::new();
        let mut gaps = BTreeSet::new();
        for &block in &order {
            let Some(mut frame) = result.block_entries[block.index()].clone() else {
                continue;
            };
            let mut observer = Observer {
                diagnostics: Some(&mut diagnostics),
                gaps: Some(&mut gaps),
                visit: None,
            };
            self.run_block(block, &mut frame, &result.args, &mut observer);
            match &r.blocks[block.index()].terminator {
                Terminator::Return(value) => {
                    if let Some(value) = value {
                        let v = frame.reg(*value).clone();
                        result.returned = Some(match &result.returned {
                            Some(old) => self.domain.join(old, &v),
                            None => v,
                        });
                    }
                    frame.regs.clear();
                    join_into(&mut result.exit, frame, self.domain);
                }
                Terminator::Raise { cause, range, .. } => {
                    frame.regs.clear();
                    result.raise_sites.push(RaiseSite {
                        cause: *cause,
                        range: *range,
                        frame: frame.clone(),
                    });
                    join_into(&mut result.raised, frame, self.domain)
                }
                Terminator::Jump(_) | Terminator::Branch { .. } | Terminator::Choice(_) => {}
            }
        }
        result.diagnostics = diagnostics.into_iter().collect();
        result.gaps = gaps
            .into_iter()
            .map(|(start, end, kind)| (kind, TextRange::new(start, end)))
            .collect();
        result
    }

    /// Visit every reachable operation of a finished analysis with the frame before it,
    /// in a deterministic order (blocks in reverse postorder, operations in order).
    pub fn replay(&self, result: &RegionResult<D>, f: &mut dyn FnMut(OpId, &Frame<D>)) {
        for block in self.program.region(result.region).reverse_postorder() {
            let Some(mut frame) = result.block_entries[block.index()].clone() else {
                continue;
            };
            let mut observer = Observer {
                diagnostics: None,
                gaps: None,
                visit: Some(&mut *f),
            };
            self.run_block(block, &mut frame, &result.args, &mut observer);
        }
    }

    /// Run a block's operations on `frame` and return the feasible successor frames.
    fn run_block(
        &self,
        block: BlockId,
        frame: &mut Frame<D>,
        args: &[D::Value],
        observer: &mut Observer<'_, D>,
    ) -> Vec<(BlockId, Frame<D>)> {
        let b = &self.program.region(frame.region).blocks[block.index()];
        for &id in &b.ops {
            if let Some(visit) = &mut observer.visit {
                visit(id, frame);
            }
            let op = self.program.op(id);
            if let (OpKind::Gap { kind, .. }, Some(gaps)) = (&op.kind, &mut observer.gaps) {
                gaps.insert((op.range.start(), op.range.end(), *kind));
            }
            let mut cx = Cx {
                program: self.program,
                domain: self.domain,
                range: op.range,
                op: Some(id),
                frame,
                diagnostics: observer.diagnostics.as_deref_mut(),
                set_regs: Vec::new(),
                set_symbol: false,
            };
            self.domain.transfer(id, &op.kind, &mut cx);
            let Cx {
                set_regs,
                set_symbol,
                ..
            } = cx;
            for reg in op.kind.writes() {
                if set_regs.contains(&reg) {
                    continue;
                }
                let value = match &op.kind {
                    OpKind::Name { symbol, .. } => load(self.program, self.domain, frame, *symbol),
                    OpKind::Param { index, .. } => args
                        .get(*index)
                        .cloned()
                        .unwrap_or_else(|| self.domain.top()),
                    _ => self.domain.top(),
                };
                frame.regs.insert(reg, value);
            }
            if !set_symbol && let OpKind::Name { symbol, access } = &op.kind {
                match access {
                    Access::Store(src) => {
                        let v = frame.reg(*src).clone();
                        frame.write(self.program, *symbol, Binding::bound(v));
                    }
                    Access::Delete => frame.write(
                        self.program,
                        *symbol,
                        Binding {
                            presence: Presence::Unbound,
                            value: self.domain.unbound(),
                        },
                    ),
                    Access::Load(_) => {}
                }
            }
        }
        match &b.terminator {
            Terminator::Jump(t) => vec![(*t, frame.clone())],
            Terminator::Branch { cond, then, orelse } => [(*then, true), (*orelse, false)]
                .into_iter()
                .filter_map(|(target, truth)| {
                    let mut out = frame.clone();
                    let mut cx = Cx {
                        program: self.program,
                        domain: self.domain,
                        range: self.cond_range(frame.region, block),
                        op: None,
                        frame: &mut out,
                        diagnostics: observer.diagnostics.as_deref_mut(),
                        set_regs: Vec::new(),
                        set_symbol: false,
                    };
                    self.domain
                        .assume(*cond, truth, &mut cx)
                        .then_some((target, out))
                })
                .collect(),
            Terminator::Choice(targets) => targets.iter().map(|t| (*t, frame.clone())).collect(),
            Terminator::Return(_) | Terminator::Raise { .. } => Vec::new(),
        }
    }

    /// The range of the operation that computed a branch condition: the block's last op.
    fn cond_range(&self, region: RegionId, block: BlockId) -> TextRange {
        self.program.region(region).blocks[block.index()]
            .ops
            .last()
            .map_or(self.program.region(region).range, |op| {
                self.program.op(*op).range
            })
    }
}

/// What a load of `symbol` reads in `frame`. A bound symbol reads its value; an unbound
/// one reads its [`crate::ir::Symbol::fallback`] (or `unbound` if it has none); a
/// possibly bound one reads the join of both. This applies equally to owned slots and
/// outer cells. A symbol with no slot or cell in the frame is unknown and reads `top`.
pub fn load<D: Domain + ?Sized>(
    program: &Program,
    domain: &D,
    frame: &Frame<D>,
    symbol: SymbolId,
) -> D::Value {
    let Some(binding) = frame.binding(program, symbol) else {
        return domain.top();
    };
    let fallback = || match program.symbol(symbol).fallback {
        Some(f) => load(program, domain, frame, f),
        None => domain.unbound(),
    };
    match binding.presence {
        Presence::Bound => binding.value.clone(),
        Presence::Unbound => fallback(),
        Presence::Maybe => domain.join(&binding.value, &fallback()),
    }
}

/// For each block, the temporaries that may be read before being written on some path
/// from its entry (classic backward liveness).
fn live_temporaries(program: &Program, region: RegionId) -> Vec<BTreeSet<Reg>> {
    let r = program.region(region);
    // Per block: temporaries read before any write in the block, and those written.
    let local: Vec<(BTreeSet<Reg>, BTreeSet<Reg>)> = r
        .blocks
        .iter()
        .map(|b| {
            let mut used = BTreeSet::new();
            let mut defined = BTreeSet::new();
            let terminator_reads = match &b.terminator {
                Terminator::Branch { cond, .. } => vec![*cond],
                Terminator::Return(r) | Terminator::Raise { exc: r, .. } => {
                    r.iter().copied().collect()
                }
                Terminator::Jump(_) | Terminator::Choice(_) => Vec::new(),
            };
            let reads = b
                .ops
                .iter()
                .map(|op| (program.op(*op).kind.reads(), program.op(*op).kind.writes()))
                .chain([(terminator_reads, Vec::new())]);
            for (reads, writes) in reads {
                for reg in reads {
                    if !defined.contains(&reg) {
                        used.insert(reg);
                    }
                }
                defined.extend(writes);
            }
            (used, defined)
        })
        .collect();
    let mut live: Vec<BTreeSet<Reg>> = local.iter().map(|(used, _)| used.clone()).collect();
    let mut changed = true;
    while changed {
        changed = false;
        for (i, b) in r.blocks.iter().enumerate().rev() {
            let mut next = live[i].clone();
            for succ in b.terminator.successors() {
                for reg in &live[succ.index()] {
                    if !local[i].1.contains(reg) {
                        next.insert(*reg);
                    }
                }
            }
            if next != live[i] {
                live[i] = next;
                changed = true;
            }
        }
    }
    live
}

fn join_into<D: Domain + ?Sized>(acc: &mut Option<Frame<D>>, frame: Frame<D>, domain: &D) {
    match acc {
        Some(a) => a.merge(&frame, domain, false),
        None => *acc = Some(frame),
    }
}
