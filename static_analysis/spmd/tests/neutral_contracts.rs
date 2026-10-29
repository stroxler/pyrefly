/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::RegionId;
use pyrefly_static_analysis::SyntacticExports;
use pyrefly_static_analysis::analyze_python;
use pyrefly_static_analysis_spmd::Contract;
use pyrefly_static_analysis_spmd::Grad;
use pyrefly_static_analysis_spmd::Local;
use pyrefly_static_analysis_spmd::Mesh;
use pyrefly_static_analysis_spmd::Placement;
use pyrefly_static_analysis_spmd::Tensor;
use pyrefly_static_analysis_spmd::Value;
use pyrefly_static_analysis_spmd::check;

#[test]
fn qualified_shard_constructor_and_negative_dimension_use_neutral_operations() {
    let source = "from spmd_types import S as shard, all_gather, R\ndef f(x):\n    return all_gather(x, 'tp', src=shard(-1), dst=R)\n";
    let mut contract = Contract {
        mesh: Some(Mesh::new(["tp"]).unwrap()),
        global: true,
        ..Contract::default()
    };
    contract.arguments.insert(
        "x".into(),
        Value::Tensor(Tensor::new(2, Grad::Yes, [("tp", Placement::Shard(1))]).unwrap()),
    );
    contract.returns =
        Some(Tensor::new(2, Grad::Yes, [("tp", Placement::Local(Local::R))]).unwrap());
    for (source, valid) in [
        (source.into(), true),
        (source.replace("shard(-1)", "shard(0)"), false),
    ] {
        let program = analyze_python(&source, ModuleName::from_str("fixture"), false);
        let region = RegionId(
            program
                .regions
                .iter()
                .position(|region| region.name.as_str() == "f")
                .unwrap() as u32,
        );
        let result = check(&program, region, &contract, &SyntacticExports);
        assert_eq!(result.validated(), valid, "{result:?}");
    }
}

#[test]
fn variadic_entry_cannot_fabricate_tensor_parameter_shapes() {
    let mut contract = Contract::default();
    let tensor = Tensor::new(2, Grad::Yes, [("tp", Placement::Local(Local::I))]).unwrap();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor.clone()));
    contract.returns = Some(tensor);
    for source in ["def f(*x):\n    return x\n", "def f(**x):\n    return x\n"] {
        let program = analyze_python(source, ModuleName::from_str("fixture"), false);
        let region = RegionId(
            program
                .regions
                .iter()
                .position(|region| region.name.as_str() == "f")
                .unwrap() as u32,
        );
        let result = check(&program, region, &contract, &SyntacticExports);
        assert!(result.incomplete && !result.validated(), "{result:?}");
    }
}
