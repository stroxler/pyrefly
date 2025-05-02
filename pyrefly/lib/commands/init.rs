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
use path_absolutize::Absolutize;
use tracing::error;

use crate::commands::config_migration;
use crate::commands::run::CommandExitStatus;
use crate::config::config::ConfigFile;
use crate::config::util::PyProject;
use crate::util::fs_anyhow;

/// Initialize a new pyrefly config in the given directory. Can also be used to run pyrefly config-migration on a given project.
#[derive(Clone, Debug, Parser)]
#[command(after_help = "Examples:
  `pyrefly init`: Create a new pyrefly.toml config in the current directory.
  `pyrefly init path/to/project`: Create a new pyrefly.toml config in the given directory.
  `pyrefly init --migrate`: Migrate a project with an existing mypy or pyright config to pyrefly.
  `pyrefly init path/to/project/mypy.ini --migrate`: Migrate the mypy config at the given path to a pyrefly.toml config in the same directory.
  `pyrefly init --migrate pyproject.toml`: Searches for a mypy or pyright config in the current directory, and writes the new pyrefly config to pyproject.toml file.
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

    fn check_for_existing_config(path: &Path) -> anyhow::Result<bool> {
        if path.ends_with(ConfigFile::PYREFLY_FILE_NAME) && path.exists() {
            return Ok(true);
        }
        if path.ends_with(ConfigFile::PYPROJECT_FILE_NAME) && path.exists() {
            let raw_pyproject = fs_anyhow::read_to_string(path).with_context(|| {
                format!(
                    "While trying to check for an existing pyrefly config in {}",
                    path.display()
                )
            })?;
            return Ok(raw_pyproject.contains("[tool.pyrefly]"));
        }
        if path.is_dir() {
            let pyrefly =
                Args::check_for_existing_config(&path.join(ConfigFile::PYREFLY_FILE_NAME));
            let pyproject =
                Args::check_for_existing_config(&path.join(ConfigFile::PYPROJECT_FILE_NAME));
            return Ok(pyrefly? || pyproject?);
        }
        Ok(false)
    }

    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        let path = self.path.absolutize()?.to_path_buf();

        if Args::check_for_existing_config(&path)? {
            error!(
                "The project at {} has already been initialized for pyrefly",
                path.display()
            );
            return Ok(CommandExitStatus::UserError);
        }

        if self.migrate {
            let args = config_migration::Args {
                input_path: Some(path),
                output_path: self.output_path.clone(),
            };
            return args.run();
        }
        let cfg = ConfigFile::default();

        if path.ends_with(ConfigFile::PYREFLY_FILE_NAME) {
            let serialized = toml::to_string_pretty(&cfg)?;
            fs_anyhow::write(&path, serialized.as_bytes())?;
        } else if path.ends_with(ConfigFile::PYPROJECT_FILE_NAME) {
            let config = PyProject::new(cfg);
            let serialized = toml::to_string_pretty(&config)?;
            fs_anyhow::append(&path, serialized.as_bytes())?;
        } else if path.is_dir() {
            let pyproject_path = path.join(ConfigFile::PYPROJECT_FILE_NAME);
            if pyproject_path.exists() {
                config_migration::write_pyproject(&pyproject_path, cfg)?;
            } else {
                let path = path.join(ConfigFile::PYREFLY_FILE_NAME);
                let serialized = toml::to_string_pretty(&cfg)?;
                fs_anyhow::write(&path, serialized.as_bytes())?;
            }
        } else {
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

        ConfigFile::from_file(&outpath, false).map(|_| ())
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

        ConfigFile::from_file(&outpath, false).map(|_| ())
    }

    #[test]
    fn test_pyrefly_path() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let dir = tmp.path().join("project");
        let cfgpath = dir.join("pyrefly.toml");
        std::fs::create_dir(&dir)?;
        let args = Args::new(cfgpath.clone());
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success), "{status:#?}");
        assert!(cfgpath.exists());
        ConfigFile::from_file(&cfgpath, false).map(|_| ())
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
        ConfigFile::from_file(&cfgpath, false).map(|_| ())
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
}
