/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Domain-free queries over a [`Program`]: reaching definitions, definition/use chains,
//! and per-symbol access history. These run a fixed set-based dataflow over operation
//! ids, without a user-supplied `Domain` and without per-operation abstract states.

use std::collections::BTreeSet;

use starlark_map::small_map::SmallMap;

use crate::ir::Access;
use crate::ir::OpId;
use crate::ir::OpKind;
use crate::ir::Program;
use crate::ir::RegionId;
use crate::ir::SymbolId;

/// A definition reaching a program point.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Def {
    /// The region entry: the symbol has had no binding on some path. A load of a
    /// symbol with a [`crate::ir::Symbol::fallback`] then reads the fallback.
    Entry,
    /// A `Name` store or delete operation.
    Op(OpId),
    /// Code in another region (a `global` or `nonlocal` writer, run by a call) also
    /// stores to or deletes the symbol, so any binding may have been replaced.
    External,
}

pub struct History {
    /// Reaching definitions for every reachable `Name` load of a region-owned symbol.
    reaching: SmallMap<OpId, BTreeSet<Def>>,
    /// The loads each store or delete reaches, in program order.
    uses: SmallMap<OpId, Vec<OpId>>,
    /// Every `Name` operation on each symbol, in program order.
    accesses: SmallMap<SymbolId, Vec<OpId>>,
}

impl History {
    pub fn new(program: &Program) -> Self {
        let externally_written: BTreeSet<SymbolId> = program
            .ops
            .iter()
            .filter_map(|op| match op.kind {
                OpKind::Name {
                    symbol,
                    access: Access::Store(_) | Access::Delete,
                } if program.owner(symbol).is_some_and(|o| o != op.region) => Some(symbol),
                _ => None,
            })
            .collect();
        let mut reaching = SmallMap::new();
        for region in 0..program.regions.len() {
            reaching_definitions(
                program,
                RegionId(region as u32),
                &externally_written,
                &mut reaching,
            );
        }
        let mut uses: SmallMap<OpId, Vec<OpId>> = SmallMap::new();
        let mut loads: Vec<OpId> = reaching.keys().copied().collect();
        loads.sort();
        for load in loads {
            for def in reaching.get(&load).expect("collected from the map's keys") {
                if let Def::Op(def) = *def {
                    uses.entry(def).or_default().push(load);
                }
            }
        }
        let mut accesses: SmallMap<SymbolId, Vec<OpId>> = SmallMap::new();
        for (i, op) in program.ops.iter().enumerate() {
            if let OpKind::Name { symbol, .. } = op.kind {
                accesses.entry(symbol).or_default().push(OpId(i as u32));
            }
        }
        Self {
            reaching,
            uses,
            accesses,
        }
    }

    /// The definitions reaching a `Name` load. `None` if `load` is not a reachable load
    /// of a symbol owned by its region: a load of a closure cell, global or builtin from
    /// another region depends on when that region runs. Includes [`Def::External`] when
    /// another region writes the symbol.
    pub fn reaching(&self, load: OpId) -> Option<&BTreeSet<Def>> {
        self.reaching.get(&load)
    }

    /// The `Name` loads that a store or delete may reach.
    pub fn uses(&self, def: OpId) -> &[OpId] {
        self.uses.get(&def).map_or(&[], |v| v.as_slice())
    }

    /// Every `Name` load, store and delete of `symbol` across all regions.
    pub fn accesses(&self, symbol: SymbolId) -> &[OpId] {
        self.accesses.get(&symbol).map_or(&[], |v| v.as_slice())
    }
}

/// Classic reaching definitions over the slots of one region.
fn reaching_definitions(
    program: &Program,
    region: RegionId,
    externally_written: &BTreeSet<SymbolId>,
    out: &mut SmallMap<OpId, BTreeSet<Def>>,
) {
    let r = program.region(region);
    let slot = |symbol: SymbolId| {
        let s = program.symbol(symbol);
        s.slot
            .filter(|_| program.scope(s.scope).region == region)
            .map(|slot| slot as usize)
    };
    let order = r.reverse_postorder();
    let mut rank = vec![usize::MAX; r.blocks.len()];
    for (i, b) in order.iter().enumerate() {
        rank[b.index()] = i;
    }
    let mut entries: Vec<Option<Vec<BTreeSet<Def>>>> = vec![None; r.blocks.len()];
    entries[r.entry.index()] = Some(vec![BTreeSet::from([Def::Entry]); r.num_slots as usize]);
    // Visiting blocks in reverse postorder makes each block see all of its forward
    // predecessors first, so only loops cause revisits.
    let mut worklist = BTreeSet::from([0]);
    let run = |block: usize,
               state: &mut Vec<BTreeSet<Def>>,
               out: Option<&mut SmallMap<OpId, BTreeSet<Def>>>| {
        let mut out = out;
        for &id in &r.blocks[block].ops {
            if let OpKind::Name { symbol, access } = program.op(id).kind
                && let Some(slot) = slot(symbol)
            {
                match access {
                    Access::Load(_) => {
                        if let Some(out) = &mut out {
                            let mut defs = state[slot].clone();
                            if externally_written.contains(&symbol) {
                                defs.insert(Def::External);
                            }
                            out.insert(id, defs);
                        }
                    }
                    Access::Store(_) | Access::Delete => {
                        state[slot] = BTreeSet::from([Def::Op(id)])
                    }
                }
            }
        }
    };
    while let Some(i) = worklist.pop_first() {
        let block = order[i];
        let mut state = entries[block.index()]
            .clone()
            .expect("queued blocks have an entry");
        run(block.index(), &mut state, None);
        for t in r.blocks[block.index()].terminator.successors() {
            let changed = match &mut entries[t.index()] {
                None => {
                    entries[t.index()] = Some(state.clone());
                    true
                }
                Some(old) => {
                    let mut changed = false;
                    for (a, b) in old.iter_mut().zip(&state) {
                        for d in b {
                            changed |= a.insert(*d);
                        }
                    }
                    changed
                }
            };
            if changed {
                worklist.insert(rank[t.index()]);
            }
        }
    }
    for (block, entry) in entries.into_iter().enumerate() {
        if let Some(mut state) = entry {
            run(block, &mut state, Some(out));
        }
    }
}
