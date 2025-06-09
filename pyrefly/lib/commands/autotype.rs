/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::PathBuf;

use anyhow::Context as _;
use clap::Parser;
use dupe::Dupe;
use pyrefly_util::forgetter::Forgetter;
use pyrefly_util::globs::FilteredGlobs;

use crate::commands::check::Handles;
use crate::commands::check::checkpoint;
use crate::commands::run::CommandExitStatus;
use crate::config::finder::ConfigFinder;
use crate::state::lsp::AnnotationKind;
use crate::state::require::Require;
use crate::state::state::State;
use crate::types::literal::Lit;
use crate::types::types::Type;

#[derive(Debug, Parser, Clone)]
pub struct Args {}

fn format_hints(
    inlay_hints: Vec<(ruff_text_size::TextSize, Type, AnnotationKind)>,
) -> Vec<(ruff_text_size::TextSize, String)> {
    let mut qualified_hints = Vec::new();
    for (position, hint, kind) in inlay_hints {
        let formatted_hint = hint_to_string(hint);
        match kind {
            AnnotationKind::Parameter => {
                qualified_hints.push((position, format!(": {}", formatted_hint)));
            }
            AnnotationKind::Return => {
                qualified_hints.push((position, format!("-> {}", formatted_hint)));
            }
        }
    }
    qualified_hints
}

// Sort the hints by reverse order so we don't have to recalculate positions
fn sort_inlay_hints(
    inlay_hints: Vec<(ruff_text_size::TextSize, String)>,
) -> Vec<(ruff_text_size::TextSize, String)> {
    let mut sorted_inlay_hints = inlay_hints;
    sorted_inlay_hints.sort_by(|(a, _), (b, _)| b.cmp(a));
    sorted_inlay_hints
}

fn hint_to_string(hint: Type) -> String {
    match &hint {
        Type::Literal(literal) => match literal {
            Lit::Str(_) => "str".to_owned(),
            Lit::Int(_) => "int".to_owned(),
            Lit::Bool(_) => "bool".to_owned(),
            Lit::Bytes(_) => "bytes".to_owned(),
            Lit::Enum(_) => format!("{}", hint),
        },
        Type::Any(_) => "Any".to_owned(),
        _ => {
            format!("{}", hint)
        }
    }
}

impl Args {
    pub fn new() -> Self {
        Self {}
    }
    pub fn run(
        self,
        files_to_check: FilteredGlobs,
        config_finder: ConfigFinder,
        search_path: Option<Vec<PathBuf>>,
    ) -> anyhow::Result<CommandExitStatus> {
        let expanded_file_list = checkpoint(files_to_check.files(), &config_finder)?;
        let holder = Forgetter::new(State::new(config_finder), false);
        let handles = Handles::new(
            expanded_file_list,
            search_path.as_deref().unwrap_or_default(),
            holder.as_ref().config_finder(),
        );
        let mut forgetter = Forgetter::new(
            holder.as_ref().new_transaction(Require::Everything, None),
            true,
        );
        let transaction = forgetter.as_mut();
        for (handle, _) in handles.all(Require::Everything) {
            transaction.run(&[(handle.dupe(), Require::Everything)]);
            let inferred_types: Option<Vec<(ruff_text_size::TextSize, Type, AnnotationKind)>> =
                transaction.inferred_types(&handle);
            if let Some(i_types) = inferred_types {
                let formatted = format_hints(i_types);
                let sorted = sort_inlay_hints(formatted);

                let file_path = handle.path().as_path();
                let file_content = fs::read_to_string(file_path)
                    .with_context(|| format!("Failed to read file: {}", file_path.display()))?;
                let mut result = file_content;
                for inlay_hint in sorted {
                    let (position, hint) = inlay_hint;
                    if hint.contains("@") {
                        continue;
                    }
                    // TODO: Put this behind a flag
                    if hint.contains("Any") {
                        continue;
                    }
                    // Convert the TextSize to a byte offset
                    let offset = (position).into();
                    if offset <= result.len() {
                        result.insert_str(offset, &hint);
                    }
                }
                fs::write(file_path, result)
                    .with_context(|| format!("Failed to write to file: {}", file_path.display()))?;
            }
        }
        Ok(CommandExitStatus::Success)
    }
}
