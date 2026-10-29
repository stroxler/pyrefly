/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::RegionId;
use pyrefly_static_analysis::SyntacticExports;
use pyrefly_static_analysis::analyze_python;
use pyrefly_static_analysis_spmd::Collective;
use pyrefly_static_analysis_spmd::Compute;
use pyrefly_static_analysis_spmd::Contract;
use pyrefly_static_analysis_spmd::Grad;
use pyrefly_static_analysis_spmd::Local;
use pyrefly_static_analysis_spmd::Mesh;
use pyrefly_static_analysis_spmd::Placement;
use pyrefly_static_analysis_spmd::Tensor;
use pyrefly_static_analysis_spmd::Value;
use pyrefly_static_analysis_spmd::check;
use pyrefly_static_analysis_spmd::compute;
use pyrefly_static_analysis_spmd::transition;

fn pointwise(inputs: &[Tensor]) -> Result<Tensor, String> {
    compute(
        Compute::Add,
        inputs,
        Some(&Mesh::new(["dp", "tp"]).unwrap()),
        &BTreeSet::new(),
        true,
    )
}

fn ordered(order: &[&str]) -> Tensor {
    Tensor::new(
        2,
        Grad::Yes,
        [
            ("dp", Placement::Local(Local::V)),
            ("tp", Placement::Local(Local::V)),
        ],
    )
    .unwrap()
    .with_partition(vec![
        order.iter().map(|axis| String::from(*axis)).collect(),
        vec![],
    ])
    .unwrap()
}

fn collisions() -> [Tensor; 2] {
    [
        Tensor::new(
            2,
            Grad::Yes,
            [
                ("dp", Placement::Local(Local::R)),
                ("tp", Placement::Shard(0)),
            ],
        )
        .unwrap(),
        Tensor::new(
            2,
            Grad::Yes,
            [
                ("dp", Placement::Shard(0)),
                ("tp", Placement::Local(Local::R)),
            ],
        )
        .unwrap(),
    ]
}

fn contract(inputs: [Tensor; 2]) -> Contract {
    let mut contract = Contract {
        mesh: Some(Mesh::new(["dp", "tp"]).unwrap()),
        global: true,
        // No expected return contract: representation validity is mandatory itself.
        ..Contract::default()
    };
    let [x, y] = inputs;
    contract.arguments.insert("x".into(), Value::Tensor(x));
    contract.arguments.insert("y".into(), Value::Tensor(y));
    contract
}

#[test]
fn coincident_pointwise_shards_need_a_shared_order() {
    for op in [Compute::Add, Compute::Multiply] {
        let result = compute(op, &collisions(), None, &BTreeSet::new(), true);
        assert!(result.unwrap_err().contains("coincident output shards"));
    }
    assert!(pointwise(&[ordered(&["dp", "tp"]), ordered(&["tp", "dp"])]).is_err());
}

#[test]
fn consistent_ordered_pointwise_shards_are_preserved() {
    let input = ordered(&["dp", "tp"]);
    let replicated = Tensor::new(
        2,
        Grad::Yes,
        [
            ("dp", Placement::Local(Local::R)),
            ("tp", Placement::Local(Local::R)),
        ],
    )
    .unwrap();
    for second in [input.clone(), replicated] {
        let output = pointwise(&[input.clone(), second]).unwrap();
        output.validate(true).unwrap();
        assert_eq!(output.partition(), input.partition());
        assert_eq!(output.local()["dp"], Local::V);
        assert_eq!(output.local()["tp"], Local::V);
    }
    let distinct_dims = Tensor::new(
        2,
        Grad::Yes,
        [("dp", Placement::Shard(0)), ("tp", Placement::Shard(1))],
    )
    .unwrap();
    assert_eq!(
        pointwise(&[distinct_dims.clone(), distinct_dims.clone()]).unwrap(),
        distinct_dims
    );
}

#[test]
fn consumer_rejects_output_collision_without_expected_return() {
    let program = analyze_python(
        "import torch\ndef f(x, y):\n    return torch.add(x, y)\n",
        ModuleName::from_str("fixture"),
        false,
    );
    let region = RegionId(
        program
            .regions
            .iter()
            .position(|region| region.name.as_str() == "f")
            .unwrap() as u32,
    );
    let invalid = contract(collisions());
    assert!(invalid.returns.is_none());
    let result = check(&program, region, &invalid, &SyntacticExports);
    assert!(!result.validated(), "{result:?}");
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("coincident output shards")),
        "{result:?}"
    );
    let valid = contract([ordered(&["dp", "tp"]), ordered(&["dp", "tp"])]);
    assert!(valid.returns.is_none());
    let result = check(&program, region, &valid, &SyntacticExports);
    assert!(result.validated(), "{result:?}");
    let Some(Value::Tensor(output)) = result.returned else {
        panic!("expected a tensor output")
    };
    output.validate(true).unwrap();
    assert_eq!(
        output.partition()[0],
        vec![String::from("dp"), String::from("tp")]
    );
}

#[test]
fn discarded_invalid_tensor_result_is_still_rejected() {
    let program = analyze_python(
        "import torch\ndef f(x, y):\n    torch.add(x, y)\n    return x\n",
        ModuleName::from_str("fixture"),
        false,
    );
    let region = RegionId(
        program
            .regions
            .iter()
            .position(|region| region.name.as_str() == "f")
            .unwrap() as u32,
    );
    let contract = contract(collisions());
    let result = check(&program, region, &contract, &SyntacticExports);
    assert_eq!(result.returned, Some(contract.arguments["x"].clone()));
    assert!(!result.validated(), "{result:?}");
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("coincident output shards")),
        "{result:?}"
    );
}

#[test]
fn output_tensor_invariants_are_checked_without_an_expected_contract() {
    let varying = Tensor::new(
        2,
        Grad::Yes,
        [
            ("dp", Placement::Local(Local::V)),
            ("tp", Placement::Local(Local::R)),
        ],
    )
    .unwrap();
    assert!(varying.validate(false).is_ok());
    assert!(
        varying
            .validate(true)
            .unwrap_err()
            .contains("missing from PartitionSpec")
    );
    let input = Tensor::new(32, Grad::Yes, [("tp", Placement::Local(Local::V))]).unwrap();
    assert!(
        transition(
            &input,
            Collective::AllGather,
            "tp",
            Placement::Local(Local::V),
            Placement::Local(Local::R),
            false
        )
        .is_err()
    );
    // Returning a trusted local-V descriptor does not certify global reconstruction,
    // including when that descriptor is nested inside a returned collection.
    for returned in ["x", "[x]"] {
        let source = format!("def f(x, y):\n    return {returned}\n");
        let program = analyze_python(&source, ModuleName::from_str("fixture"), false);
        let region = RegionId(
            program
                .regions
                .iter()
                .position(|region| region.name.as_str() == "f")
                .unwrap() as u32,
        );
        let result = check(
            &program,
            region,
            &contract([varying.clone(), varying.clone()]),
            &SyntacticExports,
        );
        assert!(!result.validated(), "{result:?}");
        assert!(
            result
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("missing from PartitionSpec")),
            "{result:?}"
        );
    }
}
