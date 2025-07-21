/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::env::args_os;
use std::process::ExitCode;

use clap::Parser;
use clap::Subcommand;
use library::AutotypeArgs;
use library::BuckCheckArgs;
use library::DumpConfigArgs;
use library::InitArgs;
use library::LspArgs;
use library::util::CommandExitStatus;
use library::util::CommonGlobalArgs;
use pyrefly::library::library::library::library;
use pyrefly::library::library::library::library::FullCheckArgs;
use pyrefly_util::args::get_args_expanded;
use pyrefly_util::panic::exit_on_panic;

// fbcode likes to set its own allocator in fbcode.default_allocator
// So when we set our own allocator, buck build buck2 or buck2 build buck2 often breaks.
// Making jemalloc the default only when we do a cargo build.
#[global_allocator]
#[cfg(all(any(target_os = "linux", target_os = "macos"), not(fbcode_build)))]
static ALLOC: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

#[global_allocator]
#[cfg(target_os = "windows")]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Main CLI entrypoint for Pyrefly.
#[deny(clippy::missing_docs_in_private_items)]
#[derive(Debug, Parser)]
#[command(name = "pyrefly")]
#[command(about = "A fast Python type checker", long_about = None)]
#[command(version)]
struct Args {
    /// Common global arguments shared across commands.
    #[command(flatten)]
    common: CommonGlobalArgs,

    /// Subcommand execution args.
    #[command(subcommand)]
    command: Command,
}

/// Subcommands to run Pyrefly with.
#[deny(clippy::missing_docs_in_private_items)]
#[derive(Debug, Clone, Subcommand)]
enum Command {
    /// Full type checking on a file or a project
    Check(FullCheckArgs),

    /// Dump info about pyrefly's configuration. Use by replacing `check` with `dump-config` in your pyrefly invocation.
    DumpConfig(DumpConfigArgs),

    /// Entry point for Buck integration
    BuckCheck(BuckCheckArgs),

    /// Initialize a new pyrefly config in the given directory,
    /// or migrate an existing mypy or pyright config to pyrefly.
    Init(InitArgs),

    /// Start an LSP server
    Lsp(LspArgs),

    /// Automatically add type annotations to a file or directory.
    Autotype(AutotypeArgs),
}

async fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<CommandExitStatus> {
    match command {
        Command::Check(args) => args.run(allow_forget).await,
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
        Command::Init(args) => args.run(),
        Command::Autotype(args) => args.run(),
        Command::DumpConfig(args) => args.run(),
    }
}

/// Run based on the command line arguments.
async fn run() -> anyhow::Result<ExitCode> {
    let args = Args::parse_from(get_args_expanded(args_os())?);
    args.common.init(false);
    Ok(run_command(args.command, true).await?.to_exit_code())
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> ExitCode {
    exit_on_panic();
    let res = run().await;
    match res {
        Ok(code) => code,
        Err(e) => {
            // If you return a Result from main, and RUST_BACKTRACE=1 is set, then
            // it will print a backtrace - which is not what we want.
            eprintln!("{:#}", e);
            ExitCode::FAILURE
        }
    }
}
