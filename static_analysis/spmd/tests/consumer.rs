/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::path::Path;
use std::process::Command;

use pyrefly_python::module_name::ModuleName;
use pyrefly_static_analysis::Domain;
use pyrefly_static_analysis::Engine;
use pyrefly_static_analysis::Entry;
use pyrefly_static_analysis::ExportProvider;
use pyrefly_static_analysis::ExportTarget;
use pyrefly_static_analysis::Limits;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::RegionId;
use pyrefly_static_analysis::analyze_python;
use pyrefly_static_analysis::exports::SyntacticExports;
use pyrefly_static_analysis_spmd::Ambient;
use pyrefly_static_analysis_spmd::Contract;
use pyrefly_static_analysis_spmd::Grad;
use pyrefly_static_analysis_spmd::Local;
use pyrefly_static_analysis_spmd::Mesh;
use pyrefly_static_analysis_spmd::Placement;
use pyrefly_static_analysis_spmd::Report;
use pyrefly_static_analysis_spmd::Spmd;
use pyrefly_static_analysis_spmd::State;
use pyrefly_static_analysis_spmd::Tensor;
use pyrefly_static_analysis_spmd::Value;
use pyrefly_static_analysis_spmd::check;
use pyrefly_static_analysis_spmd::sidecar;

const MLP: &str = include_str!("../examples/tp_mlp.py");
const MLP_CONTRACT: &str = include_str!("../examples/tp_mlp.json");

fn program(source: &str) -> Program {
    analyze_python(source, ModuleName::from_str("fixture"), false)
}

fn region(program: &Program, name: &str) -> RegionId {
    RegionId(
        program
            .regions
            .iter()
            .position(|r| r.name.as_str() == name)
            .unwrap() as u32,
    )
}

fn tensor(local: Local) -> Tensor {
    Tensor::new(2, Grad::Yes, [("tp", Placement::Local(local))]).unwrap()
}

fn contract() -> Contract {
    Contract {
        mesh: Some(Mesh::new(["tp"]).unwrap()),
        ..Contract::default()
    }
}

fn report(source: &str, contract: &Contract) -> Report {
    let program = program(source);
    check(&program, region(&program, "f"), contract, &SyntacticExports)
}

fn mlp(source: &str) -> Report {
    let program = program(source);
    let (region, contract) = sidecar::parse(&program, MLP_CONTRACT).unwrap();
    check(&program, region, &contract, &SyntacticExports)
}

#[test]
fn tp_mlp_checks_forward_and_backward_contracts() {
    let result = mlp(MLP);
    assert!(result.validated(), "{result:?}");
    assert_eq!(result.returned, Some(Value::Tensor(tensor(Local::I))));
    let result = mlp(&MLP.replace("x = s.invariant_to_replicate(x, \"tp\")", "x = x"));
    assert!(!result.validated());
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("I->R entry")),
        "{result:?}"
    );
    let result = mlp(&MLP.replace("dst=s.I", "dst=s.R"));
    assert!(!result.validated());
    assert!(
        result.diagnostics.iter().any(|d| d.code == "exit"),
        "{result:?}"
    );
    // R and I have equal forward denotation, but the exit's gradient contract differs.
    assert_eq!(result.returned, Some(Value::Tensor(tensor(Local::R))));
    let result = mlp(&MLP.replace(
        "return s.all_reduce(p, \"tp\", src=s.P, dst=s.I)",
        "return p",
    ));
    assert!(!result.validated());
    assert_eq!(result.returned, Some(Value::Tensor(tensor(Local::P))));
}

#[test]
fn metadata_is_transparent_but_nonlinear_partial_reads_are_not() {
    let result = mlp(&MLP.replace("p.stride()", "t.relu(p)"));
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Partial")),
        "{result:?}"
    );
    let mut contract = contract();
    contract
        .arguments
        .insert("p".into(), Value::Tensor(tensor(Local::P)));
    contract.returns = Some(tensor(Local::P));
    let result = report(
        "def f(p):\n    p.shape\n    p.dtype\n    p.device\n    p.ndim\n    p.size()\n    p.stride(0)\n    p.dim()\n    p.ndimension()\n    p.storage_offset()\n    len(p)\n    return p\n",
        &contract,
    );
    assert!(result.validated(), "{result:?}");
    let result = report(
        "import torch\ndef f(p):\n    p.stride()\n    return torch.relu(p)\n",
        &contract,
    );
    assert!(!result.validated());
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("Partial"))
    );
    let result = report("def f(p):\n    p.values()\n    return p\n", &contract);
    assert!(result.incomplete && !result.validated());
}

#[test]
fn renamed_imports_aliases_and_shadowing_use_reaching_identity() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::I)));
    contract.returns = Some(tensor(Local::R));
    let result = report(
        "from spmd_types import invariant_to_replicate as enter\ndef f(x):\n    renamed = enter\n    return renamed(x, 'tp')\n",
        &contract,
    );
    assert!(result.validated(), "{result:?}");
    let result = report(
        "from spmd_types import invariant_to_replicate as enter\ndef f(x):\n    enter = x\n    return enter(x, 'tp')\n",
        &contract,
    );
    assert!(result.incomplete && !result.validated());
    let result = report(
        "import torch as t\ndef f(x):\n    if False:\n        t = x\n    return t.relu(x)\n",
        &contract,
    );
    assert!(result.incomplete && !result.validated(), "{result:?}");
    let result = report(
        "from spmd_types import invariant_to_replicate as enter\ndef f(x, enter):\n    return enter(x, 'tp')\n",
        &contract,
    );
    assert!(result.incomplete && !result.validated());
}

#[test]
fn no_grad_entry_relaxation_is_not_guessed() {
    let source = "import torch\ndef f(x, w):\n    return torch.matmul(x, w)\n";
    let mut contract = contract();
    contract
        .arguments
        .insert("w".into(), Value::Tensor(tensor(Local::V)));
    contract.returns = Some(tensor(Local::V));
    for (grad, valid) in [(Grad::No, true), (Grad::Yes, false), (Grad::Unknown, false)] {
        contract.arguments.insert(
            "x".into(),
            Value::Tensor(Tensor::new(2, grad, [("tp", Placement::Local(Local::I))]).unwrap()),
        );
        let result = report(source, &contract);
        assert_eq!(result.validated(), valid, "{result:?}");
    }
}

#[test]
fn ambient_mesh_restores_on_normal_and_early_exits() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::I)));
    contract
        .arguments
        .insert("inner".into(), Value::Mesh(Mesh::new(["dp"]).unwrap()));
    contract.arguments.insert(
        "dp".into(),
        Value::Tensor(Tensor::new(2, Grad::Yes, [("dp", Placement::Local(Local::I))]).unwrap()),
    );
    contract.returns = Some(tensor(Local::I));
    let source = "import torch\nimport spmd_types as s\ndef f(x, dp, inner):\n    with s.set_current_mesh(inner):\n        dp = torch.relu(dp)\n    return torch.relu(x)\n";
    let result = report(source, &contract);
    assert!(result.validated(), "{result:?}");
    let result = report(
        &source.replace("dp = torch.relu(dp)", "torch.relu(x)"),
        &contract,
    );
    assert!(!result.validated(), "{result:?}");
    // Exercise the public full-state API directly and verify unwind restoration.
    let source = "import spmd_types as s\ndef f(x, inner):\n    with s.set_current_mesh(inner):\n        return x\n";
    let program = program(source);
    let empty = BTreeMap::new();
    let domain = Spmd {
        exports: &SyntacticExports,
        partial_outputs: &empty,
        global: false,
    };
    let engine = Engine::new(&program, &domain);
    let module = engine.analyze(Program::MODULE, Entry::new(State::new(None)));
    let mut entry = Entry::new(State::new(contract.mesh.clone()));
    entry.args = vec![
        Value::Tensor(tensor(Local::I)),
        contract.arguments["inner"].clone(),
    ];
    let s = program.global("s").unwrap();
    entry.outer.insert(
        s,
        module.exit.unwrap().binding(&program, s).unwrap().clone(),
    );
    let result = engine.analyze(region(&program, "f"), entry);
    let state = result.exit.unwrap().state;
    assert_eq!(state.ambient, Ambient::Known(contract.mesh));
    assert_eq!(state.contexts, Some(Vec::new()));
}

#[test]
fn active_mesh_completeness_is_not_inferred_from_operand_union() {
    let mut contract = contract();
    contract.mesh = Some(Mesh::new(["tp", "dp"]).unwrap());
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::R)));
    let result = report(
        "import torch\ndef f(x):\n    return torch.relu(x)\n",
        &contract,
    );
    assert!(!result.validated());
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("missing active mesh axis dp")),
        "{result:?}"
    );
    let result = report("def f(x):\n    x.stride()\n    return x\n", &contract);
    assert!(
        result.validated(),
        "metadata does not compute on tensor values: {result:?}"
    );
}

#[test]
fn branch_ambiguity_does_not_become_a_safe_contract() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::I)));
    contract.arguments.insert("flag".into(), Value::Predicate);
    let source = "import spmd_types as s\nimport torch\ndef f(x, flag):\n    if flag:\n        x = s.invariant_to_replicate(x, 'tp')\n    return torch.relu(x)\n";
    let result = report(source, &contract);
    assert!(!result.validated());
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.message.contains("ambiguous across branches")),
        "{result:?}"
    );
    let result = report(
        "def f(x, flag):\n    if flag:\n        y = x\n    return y\n",
        &contract,
    );
    assert!(result.incomplete && !result.validated(), "{result:?}");
    let empty = BTreeMap::new();
    let domain = Spmd {
        exports: &SyntacticExports,
        partial_outputs: &empty,
        global: false,
    };
    let left = State::new(Some(Mesh::new(["tp"]).unwrap()));
    let right = State::new(Some(Mesh::new(["dp"]).unwrap()));
    assert_eq!(domain.join_state(&left, &right).ambient, Ambient::Ambiguous);
    assert_eq!(
        domain.join(
            &Value::Tensor(tensor(Local::I)),
            &Value::Tensor(tensor(Local::R))
        ),
        Value::Ambiguous
    );
}

#[test]
fn unknown_effects_cannot_be_repaired_by_metadata_or_annotations() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::I)));
    contract.returns = Some(tensor(Local::I));
    for source in [
        "def f(x):\n    opaque(x)\n    return x\n",
        "def f(x):\n    x.foo = x\n    return x\n",
        "import torch\ndef f(x):\n    torch.relu(x, inplace=True)\n    return x\n",
        "def f(x):\n    with unknown_context():\n        x.stride()\n    return x\n",
        "import spmd_types as s\ndef f(x):\n    x = opaque(x)\n    s.assert_type(x, s.I)\n    return x\n",
        "def f(x):\n    try:\n        x.stride()\n    except Exception:\n        pass\n    return x\n",
        "def f(x):\n    missing\n    return x\n",
        "def f(x):\n    if x:\n        pass\n    return x\n",
    ] {
        let result = report(source, &contract);
        assert!(
            result.incomplete && !result.validated(),
            "{source}\n{result:?}"
        );
    }
    let result = report(
        "import torch\ndef f(x):\n    return torch.relu(*[x])\n",
        &contract,
    );
    assert!(result.incomplete && !result.validated());
}

#[test]
fn collective_src_dst_axis_and_type_only_strictness() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::P)));
    contract.returns = Some(tensor(Local::I));
    let source =
        "import spmd_types as s\ndef f(x):\n    return s.all_reduce(x, 'tp', src=s.P, dst=s.I)\n";
    assert!(report(source, &contract).validated());
    for wrong in [
        source.replace("src=s.P", "src=s.R"),
        source.replace("dst=s.I", "dst=s.V"),
        source.replace("'tp'", "'wrong'"),
    ] {
        assert!(!report(&wrong, &contract).validated());
    }
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::V)));
    assert!(
        report(source, &contract).validated(),
        "runtime reduction permits V reinterpret"
    );
    let strict = source.replace("s.all_reduce", "s.rules.all_reduce");
    assert!(!report(&strict, &contract).validated());
    assert!(report(&strict.replace("src=s.P", "src=s.V"), &contract).validated());
    contract.arguments.insert("x".into(), Value::Unknown);
    assert!(
        !report(source, &contract).validated(),
        "src cannot invent an input contract"
    );
}

#[test]
fn unreachable_effects_are_not_reported_and_loops_terminate() {
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::R)));
    contract.arguments.insert("flag".into(), Value::Predicate);
    contract.returns = Some(tensor(Local::R));
    let result = report(
        "import torch\ndef f(x):\n    if False:\n        opaque(x)\n    return torch.relu(x)\n",
        &contract,
    );
    // This source has no flag parameter; remove the extra entry contract.
    assert!(!result.validated());
    contract.arguments.remove("flag");
    assert!(
        report(
            "import torch\ndef f(x):\n    if False:\n        opaque(x)\n    return torch.relu(x)\n",
            &contract
        )
        .validated()
    );
    contract.arguments.insert("flag".into(), Value::Predicate);
    let source =
        "import torch\ndef f(x, flag):\n    while flag:\n        x = torch.relu(x)\n    return x\n";
    assert!(report(source, &contract).validated());
    contract.limits = Limits {
        max_block_visits: 0,
        widen_after: 0,
    };
    assert!(report(source, &contract).incomplete);
}

#[test]
fn sidecars_reject_ambiguous_lines_and_unknown_fields() {
    let parsed = program(MLP);
    assert!(
        sidecar::parse(
            &parsed,
            &MLP_CONTRACT.replace("\"global\": true", "\"globla\": true")
        )
        .is_err()
    );
    let mut invalid_line: serde_json::Value = serde_json::from_str(MLP_CONTRACT).unwrap();
    invalid_line["partial_outputs"][0]["line"] = serde_json::json!(1);
    assert!(sidecar::parse(&parsed, &invalid_line.to_string()).is_err());
    assert!(sidecar::parse(&parsed, &MLP_CONTRACT.replace("\"S(1)\"", "\"S(100)\"")).is_err());
    let source = MLP.replace("p = t.matmul(h, w2)", "p = t.matmul(t.relu(h), w2)");
    assert!(sidecar::parse(&program(&source), MLP_CONTRACT).is_err());
}

#[test]
fn runnable_cli_has_explicit_contract_status_and_nonzero_errors() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples");
    let output = Command::new(env!("CARGO_BIN_EXE_pyrefly_static_analysis_spmd"))
        .arg(path.join("tp_mlp.py"))
        .arg(path.join("tp_mlp.json"))
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("contract-validated"));
    let output = Command::new(env!("CARGO_BIN_EXE_pyrefly_static_analysis_spmd"))
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(2));
}

// Verify the consumer can use a genuine re-export resolver rather than only
// matching the syntactic import spelling. Name is inferred from the trait API.
struct Reexports;
impl ExportProvider for Reexports {
    fn resolve(&self, module: ModuleName, name: &pyrefly_static_analysis::Name) -> ExportTarget {
        if module.to_string() == "my_ops" && name.as_str() == "entry" {
            ExportTarget::Definition(
                ModuleName::from_str("spmd_types"),
                pyrefly_static_analysis::Name::new("invariant_to_replicate"),
            )
        } else {
            ExportTarget::Unknown
        }
    }
}

#[test]
fn generic_export_provider_controls_function_identity() {
    let program = program("from my_ops import entry\ndef f(x):\n    return entry(x, 'tp')\n");
    let mut contract = contract();
    contract
        .arguments
        .insert("x".into(), Value::Tensor(tensor(Local::I)));
    contract.returns = Some(tensor(Local::R));
    assert!(check(&program, region(&program, "f"), &contract, &Reexports).validated());
    assert!(
        !check(
            &program,
            region(&program, "f"),
            &contract,
            &SyntacticExports
        )
        .validated()
    );
}
