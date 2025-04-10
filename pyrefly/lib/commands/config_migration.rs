/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The very basic version of this script takes the path to the config file
// as an argument, reads it, parses it, converts it, and writes it out.
// Future features:
// - find the configs to convert rather than taking a path
// - match up the error configurations (best-effort)
// - extract configs from pyproject.toml
// - write configs to pyproject.toml
// This script does not otherwise invoke pyrefly. This gives the user time to change anything by hand if needed.
use std::path::PathBuf;

use clap::Parser;
use tracing::info;

use crate::config::pyright::PyrightConfig;
use crate::run::CommandExitStatus;
use crate::util::fs_anyhow;
use crate::ConfigFile;

#[derive(Clone, Debug, Parser)]
pub struct Args {
    /// The path to the mypy or pyright config file to convert.
    input_path: PathBuf,
    /// Optional path to write the converted pyre.toml config file to. If not provided, the config will be written to the same directory as the input file.
    output_path: Option<PathBuf>,
}

impl Args {
    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        info!("Looking for {}", self.input_path.display());
        let raw_file = fs_anyhow::read_to_string(&self.input_path)?;
        if self.input_path.file_name() == Some("pyrightconfig.json".as_ref()) {
            info!("Detected pyright config file");
            let pyr = serde_json::from_str::<PyrightConfig>(&raw_file)?;
            let config = pyr.convert();
            let serialized = toml::to_string_pretty(&config)?;
            info!("Conversion finished");
            if let Some(output_path) = &self.output_path {
                fs_anyhow::write(output_path, serialized.as_bytes())?;
                info!("New config written to {}", output_path.display());
            } else {
                let output_path = self.input_path.with_file_name(ConfigFile::CONFIG_FILE_NAME);
                fs_anyhow::write(&output_path, serialized.as_bytes())?;
                info!("New config written to {}", output_path.display());
            }
            Ok(CommandExitStatus::Success)
        } else {
            eprintln!("Currently only migration form pyrightconfig.json is supported at this time");
            Ok(CommandExitStatus::UserError)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_pyright() -> anyhow::Result<()> {
        let tmp = tempfile::tempdir()?;
        let args = Args {
            input_path: tmp.path().join("pyrightconfig.json"),
            output_path: None,
        };
        let pyr = br#"{
    "include": ["src/**/*.py"]
}
"#;
        fs_anyhow::write(&args.input_path, pyr)?;
        let status = args.run()?;
        assert!(matches!(status, CommandExitStatus::Success));
        let output_path = args.input_path.with_file_name(ConfigFile::CONFIG_FILE_NAME);
        let output = fs_anyhow::read_to_string(&output_path)?;
        // We're not going to check the whole output because most of it will be default values, which may change.
        // We only actually care about the includes.
        let output_lines = output.lines().collect::<Vec<_>>();
        assert_eq!(output_lines[0], r#"project_includes = ["src/**/*.py"]"#);
        assert!(output_lines.len() > 1);
        Ok(())
    }
}
