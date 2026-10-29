/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Fixture harness. Each module's source marks expected diagnostics with trailing
//! `# E: <kind>` comments, one per diagnostic kind expected on that line. The harness
//! compares the set of `(line, kind)` pairs; messages are shown on mismatch.

use pyrefly_static_analysis::SyntacticExports;
use pyrefly_static_analysis_lifeguard::ModuleReport;
use pyrefly_static_analysis_lifeguard::Project;

fn expected(source: &str) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    for (i, line) in source.lines().enumerate() {
        for part in line.split("# E:").skip(1) {
            let kind = part.split_whitespace().next().expect("a kind after `# E:`");
            out.push((i + 1, kind.to_owned()));
        }
    }
    out.sort();
    out.dedup();
    out
}

/// Analyze `modules` together and check every module's expectations.
pub fn check_all(modules: &[(&str, &str)]) -> Vec<ModuleReport> {
    let mut project = Project::new();
    for (name, source) in modules {
        project.add(name, source, false);
    }
    let reports = project.analyze(&SyntacticExports);
    for (name, source) in modules {
        let report = reports
            .iter()
            .find(|r| r.name.as_str() == *name)
            .expect("every module has a report");
        let mut actual: Vec<(usize, String)> = report
            .diagnostics
            .iter()
            .map(|d| (d.line, d.kind.as_str().to_owned()))
            .collect();
        actual.sort();
        actual.dedup();
        let details: Vec<String> = report
            .diagnostics
            .iter()
            .map(|d| format!("  {}:{}: {}: {}", d.line, d.column, d.kind, d.message))
            .collect();
        assert_eq!(
            expected(source),
            actual,
            "module `{name}` diagnostics:\n{}\nsource:\n{source}",
            details.join("\n"),
        );
    }
    reports
}

/// Analyze a single module named `test`.
pub fn check(source: &str) -> ModuleReport {
    check_all(&[("test", source)]).remove(0)
}
