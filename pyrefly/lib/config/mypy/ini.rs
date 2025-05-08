/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;

use configparser::ini::Ini;
use configparser::ini::IniDefault;
use regex::Regex;
use regex_syntax::ast::print;
use serde::Deserialize;

use crate::config::config::ConfigFile;
use crate::config::environment::environment::PythonEnvironment;
use crate::config::mypy::regex_converter;
use crate::module::wildcard::ModuleWildcard;
use crate::sys_info::PythonPlatform;
use crate::sys_info::PythonVersion;
use crate::util::globs::Globs;
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
    #[serde(
        rename = "follow_untyped_imports",
        default = "ConfigFile::default_true"
    )]
    use_untyped_imports: bool,
    disable_error_code: Option<Vec<String>>,
    enable_error_code: Option<Vec<String>>,
}

impl MypyConfig {
    pub fn parse_mypy_config(ini_path: &Path) -> anyhow::Result<ConfigFile> {
        fn ini_string_to_array(value: &Option<String>) -> Vec<String> {
            match value {
                Some(value) => value
                    .split(',')
                    .map(|x| x.trim().to_owned())
                    .filter(|s| !s.is_empty())
                    .collect(),
                _ => Vec::new(),
            }
        }
        let mut default = IniDefault::default();
        // Need to set this to properly parse things like the PyTorch mypy.ini file,
        // Which has a multiline `files` comment that gets parsed incorrectly without this.
        default.multiline = true;
        let mut config = Ini::new_from_defaults(default);
        let map = config.load(ini_path);

        // https://mypy.readthedocs.io/en/latest/config_file.html#import-discovery
        // files, packages, modules, mypy_path, python_executable, python_version, and excludes can only be set in the top level `[mypy]` global section
        let files: Vec<String> = ini_string_to_array(&config.get("mypy", "files"));
        let packages: Vec<String> = ini_string_to_array(&config.get("mypy", "packages")); // list of strings
        let modules: Vec<String> = ini_string_to_array(&config.get("mypy", "modules")); // list of strings
        let excludes = config.get("mypy", "exclude"); // regex
        let mypy_path = config.get("mypy", "mypy_path"); // string
        let python_executable = config.get("mypy", "python_executable");
        let python_version = config.get("mypy", "python_version");
        let disable_error_code: Vec<String> =
            ini_string_to_array(&config.get("mypy", "disable_error_code"));
        let enable_error_code: Vec<String> =
            ini_string_to_array(&config.get("mypy", "enable_error_code"));

        let mut replace_imports: Vec<String> = Vec::new();
        let mut follow_untyped_imports = false;
        // let mut new_configuration = HashMap::new();
        if let Ok(mypy_config) = map {
            for (section_name, settings) in mypy_config {
                for (key, value) in settings {
                    if let Some(val) = value {
                        if val.as_str() == "True"
                            && key == "ignore_missing_imports"
                            && section_name.starts_with("mypy-")
                        {
                            replace_imports.push(section_name.clone());
                        }
                        if key == "follow_untyped_imports" {
                            follow_untyped_imports = follow_untyped_imports || (val == "True");
                        }
                    }
                }
            }
        }
        // Create new configuration
        let mut cfg = ConfigFile::default();

        let project_includes = [files, packages, modules]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        if !project_includes.is_empty() {
            cfg.project_includes = Globs::new(project_includes);
        }

        if let Some(exclude_regex) = excludes {
            let patterns = regex_converter::convert(&exclude_regex)?;
            if !patterns.is_empty() {
                cfg.project_excludes = Globs::new(patterns);
            }
        }

        if let Some(python_interpreter) = python_executable {
            // TODO: Add handling for when these are virtual environments
            // Is this something we can auto detect instead of hardcoding here.
            cfg.python_interpreter = PathBuf::from_str(&python_interpreter).ok();
        }

        if let Some(version) = python_version {
            cfg.python_environment.python_version = PythonVersion::from_str(&version).ok();
        }

        if let Some(search_paths) = mypy_path {
            let re = Regex::new(r"[,:]").unwrap();
            let value: Vec<PathBuf> = re
                .split(&search_paths)
                .map(|x| x.trim().to_owned())
                .filter(|x| !x.is_empty())
                .map(PathBuf::from)
                .collect();
            cfg.search_path = value;
        }
        cfg.use_untyped_imports = follow_untyped_imports;
        cfg.root.replace_imports_with_any = replace_imports
            .into_iter()
            .flat_map(|x| {
                if let Some(stripped) = x.strip_prefix("mypy-") {
                    stripped
                        .split(",")
                        .filter(|x| !x.is_empty())
                        .map(|x| ModuleWildcard::new(x).ok())
                        .collect()
                } else {
                    vec![ModuleWildcard::new(&x).ok()]
                }
            })
            .filter(|x| x.is_some())
            .collect();

        let mut errors = HashMap::new();
        for error_code in disable_error_code {
            errors.insert(error_code, false);
        }
        // enable_error_code overrides disable_error_code
        for error_code in enable_error_code {
            errors.insert(error_code, true);
        }
        cfg.root.errors = crate::config::mypy::code_to_kind(errors);

        Ok(cfg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::fs_anyhow;

    #[test]
    fn test_run_mypy() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        // This config is derived from the pytorch mypy.ini.
        let mypy = br#"[mypy]
files =
    src,
    other_src,
    test/some_test.py,
 
mypy_path = some_paths:comma,separated
 
unknown_option = True
 
exclude = src/include/|other_src/include/|src/specific/bad/file.py
 
[mypy-some.*.project]
ignore_missing_imports = True

[mypy-some.specific.project.subdir]
ignore_missing_imports = True

[mypy-stricter.on.this.*]
check_untyped_defs = True

[mypy-do.follow.*]
follow_untyped_imports = True

[mypy-comma,separated,projects]
ignore_missing_imports = True
 "#;
        fs_anyhow::write(&input_path, mypy)?;

        let cfg = MypyConfig::parse_mypy_config(&input_path)?;
        let project_includes = Globs::new(vec![
            "src".to_owned(),
            "other_src".to_owned(),
            "test/some_test.py".to_owned(),
        ]);
        assert_eq!(cfg.project_includes, project_includes);

        assert_eq!(
            cfg.search_path,
            vec![
                PathBuf::from("some_paths"),
                PathBuf::from("comma"),
                PathBuf::from("separated"),
            ]
        );

        let expected_excludes = Globs::new(vec![
            "**/src/include/".to_owned(),
            "**/other_src/include/".to_owned(),
            "**/src/specific/bad/file.py".to_owned(),
        ]);
        assert_eq!(cfg.project_excludes, expected_excludes);
        assert_eq!(cfg.replace_imports_with_any(None).len(), 5);
        assert!(cfg.use_untyped_imports);
        Ok(())
    }

    #[test]
    fn test_no_overwrite_missing_includes() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        // This config is derived from the pytorch mypy.ini.
        let mypy = br#"[mypy]
 unknown_option = True
 "#;
        fs_anyhow::write(&input_path, mypy)?;

        let cfg = MypyConfig::parse_mypy_config(&input_path)?;
        let default = ConfigFile::default();
        assert_eq!(cfg.project_includes, default.project_includes);
        assert_eq!(cfg.project_excludes, default.project_excludes);
        Ok(())
    }
    #[test]
    fn test_disable_errors() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        let src = br#"[mypy]
disable_error_code = union-attr
"#;
        fs_anyhow::write(&input_path, src)?;
        let mut cfg = MypyConfig::parse_mypy_config(&input_path)?;
        cfg.configure();
        let errors = cfg.errors(tmp.path());
        assert!(!errors.is_enabled(crate::error::kind::ErrorKind::MissingAttribute));
        Ok(())
    }
}
