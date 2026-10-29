/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Optional feedback from solved Pyrefly continuations to the neutral source driver.
//!
//! Inference keys stay here. Consumers receive source-qualified facts through the
//! core's small `FlowFacts` interface. This is not a complete typed CFG: only observed
//! `BranchEnd` arms can carry a no-normal-completion proof, under typing assumptions.

use std::collections::HashMap;
use std::sync::Arc;

use pyrefly_graph::index::Idx;
use pyrefly_python::module_name::ModuleName;
use pyrefly_static_analysis::facts::FlowFacts;
use pyrefly_static_analysis::facts::NormalCompletion;
use pyrefly_static_analysis::flow_point::FlowPointId;
use pyrefly_static_analysis::flow_point::FlowPointKind;

use crate::alt::answers::Answers;
use crate::binding::binding::Key;
use crate::binding::bindings::Bindings;
use crate::binding::driver::FlowObservation;
use crate::types::types::Type;

pub use pyrefly_static_analysis::facts::DiagnosticCheck;
pub use pyrefly_static_analysis::facts::FactAssumptions;
pub use pyrefly_static_analysis::facts::FeedbackMetadata;
pub use pyrefly_static_analysis::facts::ObservationCoverage;

/// A frozen facts table from one analyzed module/source. Recreate it after edits or
/// dependency/configuration changes; exact source matching alone is not a dependency
/// invalidation system. No global cache or live session is hidden in this value.
pub struct TypedFlowFacts {
    module: ModuleName,
    source: Arc<str>,
    facts: HashMap<FlowPointId, NormalCompletion>,
    metadata: FeedbackMetadata,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SourceMismatch {
    Module,
    Source,
}

/// A caller-validated view tied to the same exact source as its analyzed program.
pub struct SourceFacts<'a> {
    snapshot: &'a TypedFlowFacts,
}

impl TypedFlowFacts {
    pub fn module(&self) -> ModuleName {
        self.module
    }

    pub fn metadata(&self) -> FeedbackMetadata {
        self.metadata
    }

    /// Validate exact source before exposing facts. A same-length edit is stale too.
    pub fn for_source(
        &self,
        module: ModuleName,
        source: &str,
    ) -> Result<SourceFacts<'_>, SourceMismatch> {
        if self.module != module {
            return Err(SourceMismatch::Module);
        }
        if self.source.as_ref() != source {
            return Err(SourceMismatch::Source);
        }
        Ok(SourceFacts { snapshot: self })
    }
}

impl FlowFacts for SourceFacts<'_> {
    fn metadata(&self) -> FeedbackMetadata {
        self.snapshot.metadata()
    }

    fn normal_completion(&self, point: FlowPointId) -> NormalCompletion {
        self.snapshot
            .facts
            .get(&point)
            .copied()
            .unwrap_or(NormalCompletion::Unknown)
    }
}

impl TypedFlowFacts {
    pub(crate) fn collect(
        bindings: &Bindings,
        mut solved_type: impl FnMut(Idx<Key>) -> Option<Type>,
    ) -> Self {
        let mut facts = HashMap::new();
        let mut mapped_arms = 0;
        let mut eligible_arms = 0;
        if let Some(report) = bindings.driver_report() {
            for mapping in &report.flow_bindings {
                if mapping.point.kind != FlowPointKind::ArmExit
                    || !matches!(bindings.idx_to_key(mapping.key), Key::BranchEnd(_))
                {
                    continue;
                }
                mapped_arms += 1;
                eligible_arms += usize::from(mapping.completion_safe);
                let fact = match solved_type(mapping.key) {
                    Some(ty) if mapping.completion_safe && ty.is_never() => {
                        NormalCompletion::NoNormalCompletionIfTypesHold
                    }
                    _ => NormalCompletion::Unknown,
                };
                // Repeated source occurrences (for example a finally expansion) must
                // not turn conflicting evidence into a proof by last-write wins.
                facts
                    .entry(mapping.point)
                    .and_modify(|old| {
                        if *old != fact {
                            *old = NormalCompletion::Unknown;
                        }
                    })
                    .or_insert(fact);
            }
        }
        let observed = bindings
            .driver_report()
            .is_some_and(|report| report.observation != FlowObservation::Disabled);
        let conditional_facts = facts
            .values()
            .filter(|fact| **fact == NormalCompletion::NoNormalCompletionIfTypesHold)
            .count();
        TypedFlowFacts {
            module: bindings.module().name(),
            source: Arc::from(bindings.module().lined_buffer().contents().as_str()),
            facts,
            metadata: FeedbackMetadata {
                assumptions: FactAssumptions::DeclaredTypesAndTypingRulesHold,
                diagnostics: DiagnosticCheck::NotProvided,
                coverage: if observed {
                    ObservationCoverage::Observed {
                        mapped_arms,
                        eligible_arms,
                        conditional_facts,
                    }
                } else {
                    ObservationCoverage::Unobserved
                },
            },
        }
    }
}

impl Answers {
    /// Copy already-solved, observed arm-continuation facts. This does not walk
    /// source, solve missing keys, validate runtime types, or inspect diagnostics.
    /// Observation and trust conditions are explicit in the snapshot metadata.
    pub fn flow_facts(&self) -> TypedFlowFacts {
        TypedFlowFacts::collect(self.bindings(), |key| self.get_type_at(key))
    }
}
