/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use itertools::Either;
use itertools::Itertools;
use ruff_python_ast::Stmt;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::Flow;
use crate::binding::scope::FlowInfo;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::Loop;
use crate::binding::scope::LoopExit;
use crate::config::error_kind::ErrorKind;
use crate::error::context::ErrorInfo;
use crate::graph::index::Idx;

// Represents a name we need to handle when merging flows.
struct MergeItem {
    // The key at which we will bind the result of the merge. Unlike all other keys
    // in our data structure, this one does not refer to a pre-existing binding coming
    // from upstream but rather the *output* of the merge.
    phi_key: Idx<Key>,
    // Key of the default binding. This is only used in loops, where it is used
    // to say that when in doubt, the loop recursive Phi should solve to either
    // the binding lives at the top of the loop (if any) or the first assignment
    // in the first branch we encountered.
    default: Idx<Key>,
    // The set of bindings live at the end of each branch. This will not include
    // `merged_key` itself (which might be live if a branch of a loop does not
    // modify anything).
    values: SmallSet<Idx<Key>>,
    // The flow styles from each branch in the merge
    flow_styles: Vec<FlowStyle>,
}

impl MergeItem {
    fn new(phi_key: Idx<Key>, info: FlowInfo, visible_branches_len: usize) -> Self {
        let mut myself = Self {
            phi_key,
            default: info.default,
            values: SmallSet::new(),
            flow_styles: Vec::with_capacity(visible_branches_len),
        };
        myself.add_branch(info);
        myself
    }

    /// Add the flow info at the end of a branch to our merge item.
    fn add_branch(&mut self, info: FlowInfo) {
        if info.key != self.phi_key {
            // Optimization: instead of x = phi(x, ...), we can skip the x.
            // Avoids a recursive solving step later.
            self.values.insert(info.key);
        }
        self.flow_styles.push(info.style);
    }
}

impl<'a> BindingsBuilder<'a> {
    fn merge_flow(&mut self, mut xs: Vec<Flow>, range: TextRange, is_loop: bool) -> Flow {
        if xs.len() == 1 && xs[0].has_terminated {
            return xs.pop().unwrap();
        }
        let (hidden_branches, mut visible_branches): (Vec<_>, Vec<_>) =
            xs.into_iter().partition(|x| x.has_terminated);

        // We normally go through the visible branches, but if nothing is visible no one is going to
        // fill in the Phi keys we promised. So just give up and use the hidden branches instead.
        let no_next = visible_branches.is_empty();
        if visible_branches.is_empty() {
            visible_branches = hidden_branches;
        }

        // Collect all the information that we care about from all branches
        let mut merge_items: SmallMap<Name, MergeItem> =
            SmallMap::with_capacity(visible_branches.first().map_or(0, |x| x.info.len()));
        let visible_branches_len = visible_branches.len();
        for flow in visible_branches {
            for (name, info) in flow.info.into_iter_hashed() {
                match merge_items.entry_hashed(name) {
                    Entry::Occupied(mut merge_item_entry) => {
                        merge_item_entry.get_mut().add_branch(info)
                    }
                    Entry::Vacant(e) => {
                        // The promise is that the next block will create a binding for all names in `names`.
                        //
                        // Note that in some cases (e.g. variables defined above a loop) we already promised
                        // a binding and this lookup will just give us back the same `Idx<Key::Phi(...)>` we
                        // created initially.
                        let phi_key = self.idx_for_promise(Key::Phi(e.key().clone(), range));
                        e.insert(MergeItem::new(phi_key, info, visible_branches_len));
                    }
                };
            }
        }

        let mut res = SmallMap::with_capacity(merge_items.len());
        for (name, merge_item) in merge_items.into_iter_hashed() {
            let style = FlowStyle::merged(merge_item.flow_styles);
            self.insert_binding_idx(
                merge_item.phi_key,
                match () {
                    _ if merge_item.values.len() == 1 => {
                        Binding::Forward(merge_item.values.into_iter().next().unwrap())
                    }
                    _ if is_loop => Binding::Default(
                        merge_item.default,
                        Box::new(Binding::Phi(merge_item.values)),
                    ),
                    _ => Binding::Phi(merge_item.values),
                },
            );
            res.insert_hashed(
                name,
                FlowInfo {
                    key: merge_item.phi_key,
                    default: if self.scopes.loop_depth() > 0 {
                        merge_item.default
                    } else {
                        merge_item.phi_key
                    },
                    style,
                },
            );
        }
        Flow {
            info: res,
            has_terminated: no_next,
        }
    }

    fn merge_into_current(&mut self, mut branches: Vec<Flow>, range: TextRange, is_loop: bool) {
        branches.push(mem::take(&mut self.scopes.current_mut().flow));
        self.scopes.current_mut().flow = self.merge_flow(branches, range, is_loop);
    }

    fn merge_loop_into_current(&mut self, branches: Vec<Flow>, range: TextRange) {
        self.merge_into_current(branches, range, true);
    }

    pub fn merge_branches_into_current(&mut self, branches: Vec<Flow>, range: TextRange) {
        self.merge_into_current(branches, range, false);
    }

    pub fn set_current_flow_to_merged_branches(&mut self, branches: Vec<Flow>, range: TextRange) {
        let flow = self.merge_flow(branches, range, false);
        self.scopes.replace_current_flow(flow);
    }

    /// Helper for loops, inserts a phi key for every name in the given flow.
    fn insert_phi_keys(&mut self, mut flow: Flow, range: TextRange) -> Flow {
        for (name, info) in flow.info.iter_mut() {
            // The promise is that we will insert a Phi binding when the control flow merges.
            info.key = self.idx_for_promise(Key::Phi(name.clone(), range));
        }
        flow
    }

    pub fn setup_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps) {
        let base = mem::take(&mut self.scopes.current_mut().flow);
        // To account for possible assignments to existing names in a loop, we
        // speculatively insert phi keys upfront.
        self.scopes.current_mut().flow = self.insert_phi_keys(base.clone(), range);
        self.scopes
            .current_mut()
            .loops
            .push(Loop(vec![(LoopExit::NeverRan, base)]));
        self.bind_narrow_ops(narrow_ops, range);
    }

    pub fn teardown_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps, orelse: Vec<Stmt>) {
        let done = self.scopes.finish_current_loop();
        let (breaks, other_exits): (Vec<Flow>, Vec<Flow>) =
            done.0.into_iter().partition_map(|(exit, flow)| match exit {
                LoopExit::Break => Either::Left(flow),
                LoopExit::NeverRan | LoopExit::Continue => Either::Right(flow),
            });
        // We associate a range to the non-`break` exits from the loop; it doesn't matter much what
        // it is as long as it's different from the loop's range.
        let other_range = TextRange::new(range.start(), range.start());
        if breaks.is_empty() {
            // When there are no `break`s, the loop condition is always false once the body has exited,
            // and any `orelse` always runs.
            self.merge_loop_into_current(other_exits, range);
            self.bind_narrow_ops(&narrow_ops.negate(), other_range);
            self.stmts(orelse);
        } else {
            // Otherwise, we negate the loop condition and run the `orelse` only when we don't `break`.
            self.merge_loop_into_current(other_exits, range);
            self.bind_narrow_ops(&narrow_ops.negate(), other_range);
            self.stmts(orelse);
            self.merge_loop_into_current(breaks, other_range);
        }
    }

    pub fn add_loop_exitpoint(&mut self, exit: LoopExit, range: TextRange) {
        let in_loop = self.scopes.add_loop_exitpoint(exit);
        if !in_loop {
            // Python treats break and continue outside of a loop as a syntax error.
            self.error(
                range,
                ErrorInfo::Kind(ErrorKind::ParseError),
                format!("Cannot `{exit}` outside loop"),
            );
        }
    }
}
