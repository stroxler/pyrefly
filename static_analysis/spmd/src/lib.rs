/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Experimental local gradient contracts and restricted rank-two sharding rules.
//! The Python frontend, lexical binding and fixpoint engine belong to the core;
//! this package supplies only a stateful domain and trusted contract descriptors.

mod domain;
pub mod sidecar;
mod types;

pub use domain::Ambient;
pub use domain::Contract;
pub use domain::Diagnostic;
pub use domain::Report;
pub use domain::Spmd;
pub use domain::State;
pub use domain::Value;
pub use domain::check;
pub use types::Collective;
pub use types::Compute;
pub use types::Grad;
pub use types::Local;
pub use types::Mesh;
pub use types::Placement;
pub use types::Tensor;
pub use types::compute;
pub use types::transition;
