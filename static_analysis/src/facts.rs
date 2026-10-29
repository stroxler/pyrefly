/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Optional facts attached by an external pre-check to structured source points.
//! Facts carry explicit typing assumptions and coverage; they are not unconditional
//! runtime proofs, and the frontend supplies no type-analysis or merge policy.

use crate::flow_point::FlowPointId;

/// Whether a source arm can complete normally, under the provider's assumptions.
/// This says nothing about divergence versus exceptions, nor about branch truth.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NormalCompletion {
    /// No proof is available. Missing facts must not prune a path.
    Unknown,
    /// The provider's types/rules establish no normal completion of this arm, but
    /// only if declared types and the provider's typing assumptions hold at runtime.
    NoNormalCompletionIfTypesHold,
}

/// These facts are conditional on the provider's typing model, not unconditional
/// execution proofs. Untyped calls, casts and incorrect annotations can violate it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FactAssumptions {
    DeclaredTypesAndTypingRulesHold,
}

/// The provider's status for pre-check diagnostics. Facts alone do not establish
/// that a program passed a diagnostic check; the consumer must account for this.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticCheck {
    NotProvided,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ObservationCoverage {
    Unobserved,
    /// Counts describe observed arm continuations, not the entire source CFG.
    Observed {
        /// Arm-exit mappings the provider observed, including repeated occurrences.
        mapped_arms: usize,
        /// Mappings the provider considers eligible to carry a completion fact.
        eligible_arms: usize,
        /// Source points carrying a conditional no-normal-completion fact.
        conditional_facts: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FeedbackMetadata {
    pub assumptions: FactAssumptions,
    pub diagnostics: DiagnosticCheck,
    pub coverage: ObservationCoverage,
}

/// An optional, source-bound facts view. Providers must reject stale source before
/// handing out the view. A downstream domain decides whether and how to use facts.
pub trait FlowFacts {
    fn metadata(&self) -> FeedbackMetadata;
    fn normal_completion(&self, point: FlowPointId) -> NormalCompletion;
}
