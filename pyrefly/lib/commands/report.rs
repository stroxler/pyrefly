/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

use crate::commands::util::CommandExitStatus;

/// Generate reports from pyrefly type checking results.
#[deny(clippy::missing_docs_in_private_items)]
#[derive(Debug, Clone, Parser)]
pub struct ReportArgs {}

impl ReportArgs {
    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        println!("Pyrefly Report is under active development.");
        Ok(CommandExitStatus::Success)
    }
}
