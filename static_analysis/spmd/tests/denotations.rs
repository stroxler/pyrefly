/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Pure numerical counterexamples for the supported rank-two contract rule.

use std::collections::BTreeSet;

use pyrefly_static_analysis_spmd::Compute;
use pyrefly_static_analysis_spmd::Grad;
use pyrefly_static_analysis_spmd::Local;
use pyrefly_static_analysis_spmd::Placement;
use pyrefly_static_analysis_spmd::Tensor;
use pyrefly_static_analysis_spmd::compute;

#[test]
fn row_sharded_matmul_partial_sum_reconstructs_full_matmul() {
    // x=[2,3], w=[5,7]^T, with one K coordinate on each rank.
    let x = [2_i64, 3];
    let w = [5_i64, 7];
    let partials = [x[0] * w[0], x[1] * w[1]];
    let full = x.iter().zip(w).map(|(x, w)| x * w).sum::<i64>();
    assert_eq!(partials.iter().sum::<i64>(), full);
    let x = Tensor::new(2, Grad::Yes, [("tp", Placement::Shard(1))]).unwrap();
    let w = Tensor::new(2, Grad::Yes, [("tp", Placement::Shard(0))]).unwrap();
    let axes = BTreeSet::from([String::from("tp")]);
    let descriptor = compute(Compute::Matmul, &[x, w], None, &axes, true).unwrap();
    assert_eq!(descriptor.local()["tp"], Local::P);
}

#[test]
fn unsupported_output_partial_intent_never_leaves_invalid_partition_metadata() {
    let x = Tensor::new(2, Grad::Yes, [("tp", Placement::Shard(1))]).unwrap();
    let axes = BTreeSet::from([String::from("tp")]);
    assert!(compute(Compute::Relu, &[x], None, &axes, true).is_err());
}
