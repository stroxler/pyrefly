/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context as _;
use configparser::ini::Ini;
use configparser::ini::IniDefault;
use pyrefly_python::sys_info::PythonVersion;
use pyrefly_util::globs::Glob;
use serde::Deserialize;

use crate::config::base::ConfigBase;
use crate::config::config::ConfigFile;
use crate::config::config::SubConfig;
use crate::config::error::ErrorDisplayConfig;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;
use crate::config::migration::project_excludes::ProjectExcludes;
use crate::config::migration::project_includes::ProjectIncludes;
use crate::config::migration::python_interpreter::PythonInterpreter;
use crate::config::migration::python_version::PythonVersionConfig;
use crate::config::migration::replace_imports::ReplaceImports;
use crate::config::migration::use_untyped_imports::UseUntypedImports;
use crate::config::migration::utils;
#[derive(Clone, Debug, Deserialize)]
#[allow(dead_code)]
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
        let mut default = IniDefault::default();
        // Need to set this to properly parse things like the PyTorch mypy.ini file,
        // Which has a multiline `files` comment that gets parsed incorrectly without this.
        default.multiline = true;
        let mut config = Ini::new_from_defaults(default);
        config
            .load(ini_path)
            .map_err(|e| anyhow::anyhow!(e))
            .with_context(|| {
                format!("While trying to read mypy config at {}", ini_path.display())
            })?;

        let mut cfg = ConfigFile::default();

        let config_options: Vec<Box<dyn ConfigOptionMigrater>> = vec![
            Box::new(ProjectIncludes),
            Box::new(ProjectExcludes),
            Box::new(PythonInterpreter),
            Box::new(PythonVersionConfig),
            Box::new(UseUntypedImports),
            Box::new(ReplaceImports),
        ];

        // Iterate through all config options and apply them to the config
        for option in config_options {
            // Ignore errors for now, we can use this in the future if we want to print out error messages or use for logging purpose
            let _ = option.migrate_from_mypy(&config, &mut cfg);
        }

        // Process search path (mypy_path)
        let mypy_path = config.get("mypy", "mypy_path"); // string
        if let Some(search_paths) = mypy_path {
            cfg.search_path_from_file = utils::string_to_paths(&search_paths);
        }

        // Process error codes
        let disable_error_code: Vec<String> =
            utils::string_to_array(&config.get("mypy", "disable_error_code"));
        let enable_error_code: Vec<String> =
            utils::string_to_array(&config.get("mypy", "enable_error_code"));
        cfg.root.errors = utils::make_error_config(disable_error_code, enable_error_code);

        // Process sub configs
        let mut sub_configs: Vec<(String, ErrorDisplayConfig)> = vec![];
        for section in &config.sections() {
            if !section.starts_with("mypy-") {
                continue;
            }

            // For subconfigs, the only config that needs to be extracted is enable/disable error codes.
            let disable_error_code: Vec<String> =
                utils::string_to_array(&config.get(section, "disable_error_code"));
            let enable_error_code: Vec<String> =
                utils::string_to_array(&config.get(section, "enable_error_code"));
            let errors = utils::make_error_config(disable_error_code, enable_error_code);
            if let Some(errors) = errors {
                sub_configs.push((section.strip_prefix("mypy-").unwrap().to_owned(), errors));
            }
        }

        let sub_configs = sub_configs
            .into_iter()
            .flat_map(|(section, errors)| {
                // Split the section headers into individual modules and pair them with the section's error config.
                // mypy uses module wildcards for its per-module sections, but we use globs.
                // A simple translation: turn `.` into `/` and `*` into `**`, e.g. `a.*.b` -> `a/**/b`.
                section
                    .split(",")
                    .map(|x| x.trim())
                    .filter(|x| !x.is_empty())
                    .map(|module| Glob::new(module.replace('.', "/").replace('*', "**")))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .zip(std::iter::repeat(Some(errors)))
            })
            .map(|(matches, errors)| SubConfig {
                matches,
                settings: ConfigBase {
                    errors,
                    ..Default::default()
                },
            })
            .collect::<Vec<_>>();
        cfg.sub_configs = sub_configs;

        Ok(cfg)
    }
}

#[cfg(test)]
mod tests {
    use pyrefly_util::fs_anyhow;
    use pyrefly_util::globs::Globs;

    use super::*;
    use crate::error::kind::ErrorKind;
    use crate::module::wildcard::ModuleWildcard;

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
            cfg.search_path_from_file,
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
        assert!(!errors.is_enabled(ErrorKind::MissingAttribute));
        Ok(())
    }

    #[test]
    fn test_replace_imports() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        let src = br#"[mypy]
test_flag = True

# replace some.*.project with Any.
[mypy-some.*.project]
ignore_missing_imports = True

# Don't replace this one, because it's not `follow_imports = skip`.
[mypy-another.project]
follow_imports = silent
"#;
        fs_anyhow::write(&input_path, src)?;
        let cfg = MypyConfig::parse_mypy_config(&input_path)?;
        assert_eq!(
            cfg.replace_imports_with_any(None),
            vec![ModuleWildcard::new("some.*.project").unwrap(),]
        );
        Ok(())
    }

    #[test]
    fn test_subconfigs() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        let src = br#"[mypy]
files = src

[mypy-src.*.linux]
disable_error_code = union-attr

[mypy-another.project]
follow_imports = silent
"#;
        fs_anyhow::write(&input_path, src)?;
        let mut cfg = MypyConfig::parse_mypy_config(&input_path)?;
        cfg.configure();

        assert!(
            cfg.errors(Path::new("src"))
                .is_enabled(ErrorKind::MissingAttribute)
        );
        assert!(
            !cfg.errors(Path::new("src/linux"))
                .is_enabled(ErrorKind::MissingAttribute)
        );
        Ok(())
    }

    #[test]
    fn test_no_use_untyped_imports() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let input_path = tmp.path().join("mypy.ini");
        let src = br#"[mypy]
follow_untyped_imports = False

[mypy-this.setting.ignored]
follow_untyped_imports = True
"#;
        fs_anyhow::write(&input_path, src)?;
        let mut cfg = MypyConfig::parse_mypy_config(&input_path)?;
        cfg.configure();

        assert!(!cfg.use_untyped_imports);
        Ok(())
    }
}
