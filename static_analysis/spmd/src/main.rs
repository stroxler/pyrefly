/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::env;
use std::fs;
use std::process::ExitCode;

use pyrefly_python::module_name::ModuleName;
use pyrefly_static_analysis::analyze_python;
use pyrefly_static_analysis::exports::SyntacticExports;
use pyrefly_static_analysis_spmd::check;
use pyrefly_static_analysis_spmd::sidecar;

fn run() -> Result<bool, String> {
    let args: Vec<_> = env::args_os().skip(1).collect();
    if args.len() != 2 {
        return Err("usage: pyrefly_static_analysis_spmd <python-file> <contracts.json>".into());
    }
    let source = fs::read_to_string(&args[0]).map_err(|error| error.to_string())?;
    let contracts = fs::read_to_string(&args[1]).map_err(|error| error.to_string())?;
    let program = analyze_python(&source, ModuleName::from_str("spmd_input"), false);
    let (region, contract) = sidecar::parse(&program, &contracts)?;
    let report = check(&program, region, &contract, &SyntacticExports);
    for diagnostic in &report.diagnostics {
        let range = diagnostic
            .op
            .map(|op| program.op(op).range)
            .unwrap_or(program.region(region).range);
        let (line, column) = program.line_col(range);
        println!(
            "{}:{}:{}: {}: {}",
            args[0].to_string_lossy(),
            line,
            column,
            diagnostic.code,
            diagnostic.message
        );
    }
    println!(
        "status: {}; incomplete: {}",
        if report.validated() {
            "contract-validated"
        } else {
            "not-validated"
        },
        report.incomplete
    );
    println!(
        "Scope: supported SPMD rules under trusted sidecar assumptions; not full Python/global-SPMD soundness."
    );
    Ok(report.validated())
}

fn main() -> ExitCode {
    match run() {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(2)
        }
    }
}
