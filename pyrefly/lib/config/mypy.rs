/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use serde::Deserialize;

use crate::ConfigFile;
use crate::PythonEnvironment;
use crate::PythonPlatform;
use crate::PythonVersion;
use crate::globs::Globs;
use crate::module::wildcard::ModuleWildcard;

#[derive(Clone, Debug, Deserialize)]

pub struct MypyConfig {
    files: Option<Vec<String>>,
    packages: Option<Vec<String>>,
    modules: Option<Vec<String>>,
    #[allow(dead_code)]
    #[serde(rename = "exclude")]
    exclude_regex: Option<String>,
    #[serde(rename = "mypy_path")]
    search_path: Option<Vec<PathBuf>>,
    #[serde(rename = "platform")]
    python_platform: Option<String>,
    python_version: Option<PythonVersion>,
    #[serde(rename = "python_executable")]
    python_interpreter: Option<PathBuf>,
}

#[derive(Deserialize)]
struct MypyOutput {
    mypy: MypyConfig,
    replace_imports: Vec<ModuleWildcard>,
}

impl MypyConfig {
    pub fn parse_mypy_config(ini_path: &Path) -> anyhow::Result<ConfigFile> {
        let script = "\
import configparser, json, sys
cp = configparser.ConfigParser()
with open(sys.argv[1]) as f:
    cp.read_file(f)
cfg = {}
replace_imports = []
for section in cp.sections():
    cfg[section] = {}
    for key, value in cp.items(section):
        if key == 'ignore_missing_imports' and section.startswith('mypy-') and value:
            replace_imports.append(section[5:])
            continue
        if key in ('files', 'packages', 'modules'):
            value = [x.strip() for x in value.split(',') if x.strip()]
        elif value in ('True', 'False'):
            value = value == 'True'
        cfg[section][key] = value
    if not cfg[section]:
        del cfg[section]
mypy = cfg.pop('mypy', {})
print(json.dumps({'mypy': mypy, 'per_module': cfg, 'replace_imports': replace_imports}))
";
        let mut cmd = Command::new(
            PythonEnvironment::get_default_interpreter()
                .ok_or_else(|| anyhow::anyhow!("Failed to find python interpreter"))?,
        );
        cmd.arg("-c");
        cmd.arg(script);
        cmd.arg(ini_path);
        let output = cmd.output()?;
        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "Failed to parse mypy config: {}",
                String::from_utf8_lossy(&output.stderr),
            ));
        }
        let raw_config = String::from_utf8(output.stdout)?;
        let MypyOutput {
            mypy,
            replace_imports,
        } = serde_json::from_str(&raw_config)?;

        let mut cfg = ConfigFile::default();

        let project_includes = Globs::new(
            [mypy.files, mypy.packages, mypy.modules]
                .into_iter()
                .flatten()
                .flatten()
                .collect::<Vec<_>>(),
        );
        cfg.project_includes = project_includes;

        if let Some(search_path) = mypy.search_path {
            cfg.search_path = search_path;
        }
        if let Some(platform) = mypy.python_platform {
            cfg.python_environment.python_platform = Some(PythonPlatform::new(&platform));
        }
        if mypy.python_version.is_some() {
            cfg.python_environment.python_version = mypy.python_version;
        }
        if mypy.python_interpreter.is_some() {
            cfg.python_interpreter = mypy.python_interpreter;
        }

        cfg.root.replace_imports_with_any = Some(replace_imports);

        Ok(cfg)
    }
}
