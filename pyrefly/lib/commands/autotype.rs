/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use clap::Parser;

use crate::commands::run::CommandExitStatus;

#[derive(Debug, Parser, Clone)]
pub struct Args {}

impl Args {
    pub fn run(&self) -> anyhow::Result<CommandExitStatus> {
        Ok(CommandExitStatus::Success)
    }
}
