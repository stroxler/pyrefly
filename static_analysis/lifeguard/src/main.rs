/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! `lifeguard-spike NAME=PATH...`: analyze an explicit list of Python files as one
//! project and print lazy-import diagnostics and a verdict per module.
//!
//! Each argument names a module and its file. A path ending in `__init__.py` marks a
//! package `__init__`. Nothing is discovered: imports of modules not listed are treated
//! as external.
//!
//! Exit status: 0 when every module is safe, 1 when some module is unsafe, 2 when none
//! is unsafe but some module could not be fully analyzed, 64 for usage or I/O errors.

use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

use pyrefly_static_analysis::SyntacticExports;
use pyrefly_static_analysis_lifeguard::Project;
use pyrefly_static_analysis_lifeguard::Verdict;

const USAGE: &str = "usage: lifeguard-spike NAME=PATH [NAME=PATH ...]";

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() || args.iter().any(|a| a == "-h" || a == "--help") {
        eprintln!("{USAGE}");
        return ExitCode::from(64);
    }
    let mut project = Project::new();
    for arg in &args {
        let Some((name, path)) = arg.split_once('=') else {
            eprintln!("expected NAME=PATH, got `{arg}`\n{USAGE}");
            return ExitCode::from(64);
        };
        let source = match fs::read_to_string(path) {
            Ok(source) => source,
            Err(e) => {
                eprintln!("cannot read `{path}`: {e}");
                return ExitCode::from(64);
            }
        };
        let is_init = Path::new(path).file_stem().is_some_and(|s| s == "__init__");
        project.add(name, &source, is_init);
    }

    let reports = project.analyze(&SyntacticExports);
    for report in &reports {
        for d in &report.diagnostics {
            println!(
                "{}:{}:{}: {}: {}",
                report.name, d.line, d.column, d.kind, d.message
            );
        }
    }
    for report in &reports {
        let eager = if report.load_imports_eagerly() {
            " load-imports-eagerly"
        } else {
            ""
        };
        println!("{}: {}{eager}", report.name, report.verdict().as_str());
    }
    let verdicts: Vec<Verdict> = reports.iter().map(|r| r.verdict()).collect();
    if verdicts.contains(&Verdict::Unsafe) {
        ExitCode::from(1)
    } else if verdicts.contains(&Verdict::Unknown) {
        ExitCode::from(2)
    } else {
        ExitCode::SUCCESS
    }
}
