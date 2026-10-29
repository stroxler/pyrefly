/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Source-occurrence identities for the shared structured statement driver.
//!
//! An overlay must qualify these IDs with its module and exact source snapshot. They
//! are stable across parses and backend pruning of the same source, not across edits.
//! Repeated lowering of a `finally` body repeats its IDs intentionally: these identify
//! source occurrences, not runtime instances or allocated control-flow blocks.
//!
//! Points describe structural boundaries, not feasibility, normal return, narrowing,
//! or merge policy. The statement driver does not identify expression-level control
//! flow, lambda scopes, or comprehension scopes.

use ruff_text_size::TextRange;

use crate::ir::Program;

/// A boundary's role in the source structure. None of these roles implies reachability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FlowPointKind {
    SuiteEntry,
    SuiteExit,
    /// An envelope around traversal of a function/class definition, including headers.
    ScopeEntry,
    ScopeExit,
    IfFork,
    IfJoin,
    ArmTest,
    ArmEntry,
    ArmExit,
    LoopHeader,
    LoopBody,
    LoopBackedge,
    /// Normal exhaustion of the loop, before its `else` suite.
    LoopExit,
    LoopElse,
    /// The boundary after the loop and its `else`, including `break` paths.
    LoopJoin,
    Break,
    Continue,
}

/// A point within one source module/snapshot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FlowPointId {
    /// Lexical function/class/module suite span, independent of backend region allocation.
    pub region: TextRange,
    /// The source construct owning this boundary, not a visit ordinal.
    pub range: TextRange,
    pub kind: FlowPointKind,
}

/// A lowered program with an opt-in trace of source boundaries visited by its backend.
///
/// The trace is not a block-location map: a structural join callback can precede the
/// backend's construction of its join block. Consumers may attach source-qualified
/// overlays here, but applying them to the engine's blocks requires a further mapping.
pub struct ProgramWithFlowPoints {
    pub program: Program,
    pub flow_points: Vec<FlowPointId>,
}

/// An opt-in observer's ordered trace. An unobserved backend needs no collector.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FlowPointCollector {
    pub points: Vec<FlowPointId>,
}

impl FlowPointCollector {
    pub fn observe(&mut self, point: FlowPointId) {
        self.points.push(point);
    }
}
