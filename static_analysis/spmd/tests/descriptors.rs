/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use pyrefly_static_analysis_spmd::Collective;
use pyrefly_static_analysis_spmd::Compute;
use pyrefly_static_analysis_spmd::Grad;
use pyrefly_static_analysis_spmd::Local;
use pyrefly_static_analysis_spmd::Mesh;
use pyrefly_static_analysis_spmd::Placement;
use pyrefly_static_analysis_spmd::Tensor;
use pyrefly_static_analysis_spmd::compute;
use pyrefly_static_analysis_spmd::transition;

fn tensor(placement: Placement) -> Tensor {
    Tensor::new(2, Grad::Yes, [("tp", placement)]).unwrap()
}

fn local(local: Local) -> Tensor {
    tensor(Placement::Local(local))
}

fn apply(op: Compute, values: &[Tensor]) -> Result<Tensor, String> {
    compute(op, values, None, &BTreeSet::new(), false)
}

#[test]
fn gradient_contracts_are_dual_but_forward_r_and_i_agree() {
    for (forward, backward) in [
        (Local::R, Local::P),
        (Local::P, Local::R),
        (Local::I, Local::I),
        (Local::V, Local::V),
    ] {
        assert_eq!(forward.gradient(), backward);
        assert_eq!(backward.gradient(), forward);
    }
    let replicated = [3.0_f64, 3.0];
    let invariant = [3.0_f64, 3.0];
    assert_eq!(replicated, invariant);
    assert_ne!(Local::R.gradient(), Local::I.gradient());
}

#[test]
fn local_rules_do_not_invent_partial_contractions() {
    assert_eq!(
        apply(Compute::Matmul, &[local(Local::V), local(Local::V)])
            .unwrap()
            .local()["tp"],
        Local::V
    );
    assert_eq!(
        apply(Compute::Matmul, &[local(Local::R), local(Local::V)])
            .unwrap()
            .local()["tp"],
        Local::V
    );
    assert!(
        apply(Compute::Matmul, &[local(Local::I), local(Local::V)])
            .unwrap_err()
            .contains("I->R")
    );
    assert_eq!(
        apply(Compute::Relu, &[local(Local::I)]).unwrap().local()["tp"],
        Local::I
    );
}

#[test]
fn linearity_rejects_affine_and_multilinear_partial_misuse() {
    assert_eq!(
        apply(Compute::Add, &[local(Local::P), local(Local::P)])
            .unwrap()
            .local()["tp"],
        Local::P
    );
    for op in [Compute::Multiply, Compute::Matmul] {
        assert_eq!(
            apply(op, &[local(Local::P), local(Local::R)])
                .unwrap()
                .local()["tp"],
            Local::P
        );
        assert!(apply(op, &[local(Local::P), local(Local::P)]).is_err());
        assert!(apply(op, &[local(Local::P), local(Local::V)]).is_err());
    }
    assert!(apply(Compute::Add, &[local(Local::P), local(Local::R)]).is_err());
    assert!(apply(Compute::Relu, &[local(Local::P)]).is_err());
    // Concrete denotations expose the missing cross terms and nonlinear error.
    let a = [1.0_f64, 1.0];
    let b = [1.0_f64, 1.0];
    assert_eq!(
        a.iter().zip(b).map(|(a, b)| a + b).sum::<f64>(),
        a.iter().sum::<f64>() + b.iter().sum::<f64>()
    );
    assert_ne!(
        a.iter().zip(b).map(|(a, b)| a * b).sum::<f64>(),
        a.iter().sum::<f64>() * b.iter().sum::<f64>()
    );
    let p = [-2.0_f64, 3.0];
    assert_ne!(
        p.iter().map(|x| x.max(0.0)).sum::<f64>(),
        p.iter().sum::<f64>().max(0.0)
    );
}

#[test]
fn no_grad_relaxation_is_explicit_and_does_not_fabricate_it() {
    let invariant = Tensor::new(2, Grad::No, [("tp", Placement::Local(Local::I))]).unwrap();
    assert_eq!(
        apply(Compute::Matmul, &[invariant, local(Local::V)])
            .unwrap()
            .local()["tp"],
        Local::V
    );
    let invariant = Tensor::new(2, Grad::Unknown, [("tp", Placement::Local(Local::I))]).unwrap();
    assert!(apply(Compute::Matmul, &[invariant, local(Local::V)]).is_err());
    let replicate = Tensor::new(2, Grad::No, [("tp", Placement::Local(Local::R))]).unwrap();
    assert_eq!(
        apply(Compute::Multiply, &[local(Local::I), replicate])
            .unwrap()
            .local()["tp"],
        Local::I
    );
}

#[test]
fn global_matmul_preserves_free_shards_and_requires_partial_intent() {
    let empty = BTreeSet::new();
    let output = compute(
        Compute::Matmul,
        &[local(Local::R), tensor(Placement::Shard(1))],
        None,
        &empty,
        true,
    )
    .unwrap();
    assert_eq!(output.partition(), &[vec![], vec![String::from("tp")]]);
    let inputs = [tensor(Placement::Shard(1)), tensor(Placement::Shard(0))];
    assert!(
        compute(Compute::Matmul, &inputs, None, &empty, true)
            .unwrap_err()
            .contains("explicit Partial")
    );
    let partial = BTreeSet::from([String::from("tp")]);
    let output = compute(Compute::Matmul, &inputs, None, &partial, true).unwrap();
    assert_eq!(output.local()["tp"], Local::P);
    assert!(output.partition().iter().all(Vec::is_empty));
    assert!(
        compute(
            Compute::Matmul,
            &[tensor(Placement::Shard(0)), tensor(Placement::Shard(1))],
            None,
            &empty,
            true
        )
        .is_err()
    );
    assert!(
        compute(
            Compute::Matmul,
            &[local(Local::R), tensor(Placement::Shard(1))],
            None,
            &partial,
            true
        )
        .is_err()
    );
}

#[test]
fn partition_and_dimension_invariants_are_checked() {
    assert!(Tensor::new(0, Grad::No, [("tp", Placement::Shard(0))]).is_err());
    assert!(Tensor::new(2, Grad::Yes, [("tp", Placement::Shard(-3))]).is_err());
    assert_eq!(tensor(Placement::Shard(-1)), tensor(Placement::Shard(1)));
    assert!(
        local(Local::R)
            .with_partition(vec![vec!["tp".into()], vec![]])
            .is_err()
    );
    assert!(
        tensor(Placement::Shard(0))
            .with_partition(vec![vec!["tp".into()], vec!["tp".into()]])
            .is_err()
    );
    assert!(Mesh::new(["tp", "tp"]).is_err());
}

#[test]
fn collective_source_and_destination_are_not_assumptions() {
    let src = Placement::Local(Local::P);
    let dst = Placement::Local(Local::I);
    assert_eq!(
        transition(
            &local(Local::P),
            Collective::AllReduce,
            "tp",
            src,
            dst,
            false
        )
        .unwrap()
        .local()["tp"],
        Local::I
    );
    assert!(
        transition(
            &local(Local::R),
            Collective::AllReduce,
            "tp",
            src,
            dst,
            false
        )
        .is_err()
    );
    assert!(
        transition(
            &local(Local::P),
            Collective::AllReduce,
            "wrong",
            src,
            dst,
            false
        )
        .is_err()
    );
    assert!(
        transition(
            &local(Local::P),
            Collective::AllReduce,
            "tp",
            src,
            Placement::Local(Local::V),
            false
        )
        .is_err()
    );
    assert!(
        transition(
            &tensor(Placement::Shard(0)),
            Collective::AllGather,
            "tp",
            Placement::Shard(1),
            Placement::Local(Local::R),
            true
        )
        .is_err()
    );
}

#[test]
fn stack_unbind_and_innermost_shard_collectives() {
    let output = transition(
        &local(Local::V),
        Collective::AllGather,
        "tp",
        Placement::Local(Local::V),
        Placement::Local(Local::R),
        false,
    )
    .unwrap();
    assert_eq!(output.rank(), 3);
    let output = transition(
        &local(Local::P),
        Collective::ReduceScatter,
        "tp",
        Placement::Local(Local::P),
        Placement::Local(Local::V),
        false,
    )
    .unwrap();
    assert_eq!(output.rank(), 1);
    let scalar = Tensor::new(0, Grad::Yes, [("tp", Placement::Local(Local::P))]).unwrap();
    assert!(
        transition(
            &scalar,
            Collective::ReduceScatter,
            "tp",
            Placement::Local(Local::P),
            Placement::Local(Local::V),
            false
        )
        .is_err()
    );
    let input = Tensor::new(
        2,
        Grad::Yes,
        [
            ("dp", Placement::Local(Local::V)),
            ("tp", Placement::Local(Local::V)),
        ],
    )
    .unwrap()
    .with_partition(vec![vec!["dp".into(), "tp".into()], vec![]])
    .unwrap();
    assert!(
        transition(
            &input,
            Collective::AllGather,
            "dp",
            Placement::Shard(0),
            Placement::Local(Local::R),
            true
        )
        .is_err()
    );
    let output = transition(
        &input,
        Collective::AllGather,
        "tp",
        Placement::Shard(0),
        Placement::Local(Local::R),
        true,
    )
    .unwrap();
    assert_eq!(output.partition()[0], vec![String::from("dp")]);
    assert!(
        transition(
            &tensor(Placement::Shard(0)),
            Collective::Convert,
            "tp",
            Placement::Shard(0),
            Placement::Shard(1),
            true
        )
        .is_err()
    );
}

#[test]
fn active_mesh_requires_every_operand_axis() {
    let mesh = Mesh::new(["dp", "tp"]).unwrap();
    assert!(
        compute(
            Compute::Relu,
            &[local(Local::P)],
            Some(&mesh),
            &BTreeSet::new(),
            false
        )
        .unwrap_err()
        .contains("missing active")
    );
}
