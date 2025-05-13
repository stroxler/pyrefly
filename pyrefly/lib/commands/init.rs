/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context as _;
use clap::Parser;
use parse_display::Display;
use path_absolutize::Absolutize;
use tracing::error;

use crate::commands::config_migration;
use crate::commands::run::CommandExitStatus;
use crate::config::config::ConfigFile;
use crate::config::util::PyProject;
use crate::util::fs_anyhow;

// This should likely be moved into config.rs
#[derive(Clone, Debug, Parser, Copy, Display)]
pub enum ConfigFileKind {
    MyPy,
    Pyright,
    Pyrefly,
    Pyproject,
}

impl ConfigFileKind {
    fn file_name(&self) -> &str {
        match self {
            Self::MyPy => "mypy.ini",
            Self::Pyright => "pyrightconfig.json",
            Self::Pyrefly => "pyrefly.toml",
            Self::Pyproject => "pyproject.toml",
        }
    }

    fn toml_identifier(self) -> String {
        match self {
            // This makes me question if pyproject should be a part of the enum at all
            Self::Pyproject => "".to_owned(),
            _ => format!("[tool.{}]", self).to_lowercase(),
        }
    }
}

/// Initialize a new pyrefly config in the given directory. Can also be used to run pyrefly config-migration on a given project.
#[derive(Clone, Debug, Parser)]
#[command(after_help = "Examples:
   `pyrefly init`: Create a new pyrefly.toml config in the current directory
 ")]
pub struct Args {
    /// The path to the project to initialize. Optional. If not present, will create a new pyrefly.toml config in the current directory.
    /// If this is the path to a pyproject.toml, the config will be written as a `[tool.pyrefly]` entry in that file.
    #[arg(default_value_os_t = PathBuf::from("."))]
    path: PathBuf,
    /// If enabled, will run `pyrefly config-migration` on the given project instead of creating a new pyrefly.toml config.
    /// PATH can point to a project directory with an existing, or directly to a pyproject.toml, mypy.ini, or pyrightconfig.json file.
    #[arg(long, default_value_t = false)]
    migrate: bool,
    /// [Optional] With --migrate, the path to output the pyrefly config to.
    /// If not present, will output to the same directory as the input, to a pyrefly.toml file if no pyproject.toml exists.
    /// If output_path points to a pyproject.toml file or a directory with a pyproject.toml file, the config will be written as a `[tool.pyrefly]` entry in that file.
    /// Otherwise, the config will be written to a pyrefly.toml file.
    #[arg(long, requires = "migrate")]
    output_path: Option<PathBuf>,
}

impl Args {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            migrate: false,
            output_path: None,
        }
    }

    pub fn new_migration(path: PathBuf, output_path: Option<PathBuf>) -> Self {
        Self {
            path,
            migrate: true,
            output_path,
        }
    }

    fn check_for_pyproject_file(path: &Path) -> bool {
        if path.ends_with(ConfigFile::PYPROJECT_FILE_NAME) {
            return true;
        }
        let pyproject_path = &path.join(ConfigFile::PYPROJECT_FILE_NAME);
        pyproject_path.exists()
    }

    fn check_for_existing_config(path: &Path, kind: ConfigFileKind) -> anyhow::Result<bool> {
        let file_name = kind.file_name();
        if path.ends_with(file_name) && path.exists() {
            return Ok(true);
        }
        if path.ends_with(ConfigFile::PYPROJECT_FILE_NAME) && path.exists() {
            let raw_pyproject = fs_anyhow::read_to_string(path).with_context(|| {
                format!(
                    "While trying to check for an existing {} config in {}",
                    kind,
                    path.display()
                )
            })?;
            return Ok(raw_pyproject.contains(&kind.toml_identifier()));
        }
        if path.is_dir() {
            let custom_file = Args::check_for_existing_config(&path.join(file_name), kind);

            let pyproject =
                Args::check_for_existing_config(&path.join(ConfigFile::PYPROJECT_FILE_NAME), kind);
            return Ok(custom_file? || pyproject?);
        }
        Ok(false)
    }

    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        let path = self.path.absolutize()?.to_path_buf();

        if Args::check_for_existing_config(&path, ConfigFileKind::Pyrefly)? {
            error!(
                "The project at {} has already been initialized for pyrefly. Run `pyrefly check` to see type errors.",
                path.display()
            );
            return Ok(CommandExitStatus::UserError);
        }

        // 1. Check for mypy configuration
        let found_mypy = Args::check_for_existing_config(&path, ConfigFileKind::MyPy)?;
        let found_pyright = Args::check_for_existing_config(&path, ConfigFileKind::Pyright)?;
        // 2. Pyrefly configuration

        if found_mypy || found_pyright {
            println!("Found an existing type checking configuration - setting up pyrefly ...");
            let args = config_migration::Args {
                input_path: Some(path),
                output_path: self.output_path.clone(),
            };
            return args.run();
        }

        // Generate a basic config with a couple sensible defaults.
        // This prevents us from simply outputting an empty file, and gives the user somewhere to start if they want to customize.
        let cfg = ConfigFile {
            project_includes: ConfigFile::default_project_includes(),
            project_excludes: ConfigFile::default_project_excludes(),
            ..Default::default()
        };

        // 3. pyproject.toml configuration
        if Args::check_for_pyproject_file(&path) {
            let config = PyProject::new(cfg);
            let serialized = toml::to_string_pretty(&config)?;
            if path.ends_with(ConfigFile::PYPROJECT_FILE_NAME) {
                fs_anyhow::append(&path, serialized.as_bytes())?;
            } else {
                let config_path = path.join(ConfigFile::PYPROJECT_FILE_NAME);
                fs_anyhow::write(&config_path, serialized.as_bytes())?;
            }
            return Ok(CommandExitStatus::Success);
        }

        if path.is_dir() {
            let config_path = path.join(ConfigFile::PYREFLY_FILE_NAME);
            let serialized = toml::to_string_pretty(&cfg)?;
            fs_anyhow::write(&config_path, serialized.as_bytes())?;
        } else if path.ends_with(ConfigFile::PYREFLY_FILE_NAME) {
            let serialized = toml::to_string_pretty(&cfg)?;
            fs_anyhow::append(&path, serialized.as_bytes())?;
        } else {
            println!("{} is not a directory", path.display());
            error!(
                "Pyrefly configs must reside in `pyrefly.toml` or `pyproject.toml`, not {}",
                path.display()
            );
            return Ok(CommandExitStatus::UserError);
        }
        Ok(CommandExitStatus::Success)
    }
}

#[cfg(test)]
mod test {
    use tempfile;

    use super::*;

    // helper function for ConfigFile::from_file
    fn from_file(path: &Path) -> anyhow::Result<()> {
        let (_, errs) = ConfigFile::from_file(path);
        if errs.is_empty() {
            Ok(())
        } else {
            Err(anyhow::anyhow!(format!(
                "ConfigFile::from_file({}) failed: {:#?}",
                path.display(),
                ConfigFile::from_file(path).1
            )))
        }
    }

    #[test]
    fn test_dir_path() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let outpath = dir.join("pyrefly.toml");
        std::fs::create_dir(&dir)?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}");
        assert!(outpath.exists());

        from_file(&outpath)
    }

    #[test]
    fn test_config_file_kinds() -> anyhow::Result<()> {
        let kind = ConfigFileKind::MyPy;
        assert_eq!(kind.toml_identifier(), "[tool.mypy]".to_owned());
        Ok(())
    }

    #[test]
    fn test_dir_path_existing_pyproject() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let outpath = dir.join("pyproject.toml");
        std::fs::create_dir(&dir)?;
        fs_anyhow::write(
            &dir.join("pyproject.toml"),
            br#"[project]
 name = "test"
 "#,
        )?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}");
        assert!(outpath.exists());

        from_file(&outpath)
    }

    #[test]
    fn test_pyrefly_path() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyrefly.toml");
        std::fs::create_dir(&dir)?;
        let mut args = Args::new(cfgpath.clone());
        args.migrate = true;
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}");
        assert!(cfgpath.exists());
        from_file(&cfgpath)
    }

    #[test]
    fn test_pyproject_path() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyproject.toml");
        std::fs::create_dir(&dir)?;
        let args = Args::new(cfgpath.clone());
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}");
        assert!(cfgpath.exists());
        from_file(&cfgpath)
    }

    #[test]
    fn test_bad_path() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        std::fs::create_dir(&dir)?;
        let args = Args::new(dir.join("personal_configs.json"));
        let status = args.run()?;
        assert!(
            matches!(status, CommandExitStatus::UserError),
            "{status:#?}"
        );
        Ok(())
    }

    #[test]
    fn test_already_initialized_pyrefly() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyrefly.toml");
        std::fs::create_dir(&dir)?;
        fs_anyhow::write(
            &cfgpath,
            br#"[pyrefly]
 project_includes = ["."]
 "#,
        )?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(
            matches!(status, CommandExitStatus::UserError),
            "{status:#?}",
        );
        Ok(())
    }

    #[test]
    fn test_already_initialized_pyproject() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyproject.toml");
        std::fs::create_dir(&dir)?;
        fs_anyhow::write(
            &cfgpath,
            br#"[project]
 name = "test"

 [tool.pyrefly]
 project_includes = ["."]
 "#,
        )?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(
            matches!(status, CommandExitStatus::UserError),
            "{status:#?}",
        );
        Ok(())
    }

    #[test]
    fn test_not_initialized_pyproject() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyproject.toml");
        std::fs::create_dir(&dir)?;
        fs_anyhow::write(
            &cfgpath,
            br#"[project]
 name = "test"
 "#,
        )?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}",);
        Ok(())
    }

    #[test]
    fn test_not_initialized_no_config() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        std::fs::create_dir(&dir)?;
        let args = Args::new(dir);
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}",);
        Ok(())
    }

    #[test]
    fn test_non_empty_pyrefly_config() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let args = Args::new(tmp.path().to_path_buf());
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}",);
        let file = tmp.path().join(ConfigFile::PYREFLY_FILE_NAME);
        assert!(file.exists());
        let raw_cfg = fs_anyhow::read_to_string(&file)?;
        assert!(!raw_cfg.is_empty());
        from_file(&file)
    }

    #[test]
    fn test_non_empty_pyright_config() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let cfgpath = tmp.path().join(ConfigFile::PYPROJECT_FILE_NAME);
        fs_anyhow::write(
            &cfgpath,
            br#"[project]
name = "test"
"#,
        )?;
        let args = Args::new(tmp.path().to_path_buf());
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}",);
        let raw_cfg = fs_anyhow::read_to_string(&cfgpath)?;
        assert!(!raw_cfg.is_empty());
        from_file(&cfgpath)
    }
}
