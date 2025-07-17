/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::Itertools;
use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::MarkupContent;
use lsp_types::MarkupKind;
use lsp_types::Url;
use ruff_text_size::TextSize;
use starlark_map::small_set::SmallSet;

use crate::state::handle::Handle;
use crate::state::lsp::FindDefinitionItem;
use crate::state::state::Transaction;

pub fn get_hover(
    transaction: &Transaction<'_>,
    handle: &Handle,
    position: TextSize,
) -> Option<Hover> {
    let t = transaction.get_type_at(handle, position)?;
    let mut kind_formatted: String = "".to_owned();
    let mut docstring_formatted: String = "".to_owned();
    if let Some(FindDefinitionItem {
        metadata,
        location,
        docstring,
    }) = transaction
        .find_definition(handle, position, true)
        // TODO: handle more than 1 definition
        .into_iter()
        .next()
    {
        if let Some(symbol_kind) = metadata.symbol_kind() {
            kind_formatted = format!(
                "{} {}: ",
                &symbol_kind.display_for_hover(),
                location.module.code_at(location.range)
            );
        }
        if let Some(docstring) = docstring {
            docstring_formatted = format!("\n---\n{}", docstring.as_string().trim());
        }
    }
    let type_formatted = t.to_string();
    let symbol_def_loc_formatted = {
        let mut tracked_def_locs = SmallSet::new();
        t.universe(&mut |t| tracked_def_locs.extend(t.qname()));
        let linked_names = tracked_def_locs
            .into_iter()
            .filter_map(|qname| {
                if let Ok(mut url) = Url::from_file_path(qname.module_path().as_path()) {
                    let start_pos = qname.module().display_range(qname.range()).start;
                    url.set_fragment(Some(&format!(
                        "L{},{}",
                        start_pos.line.get(),
                        start_pos.column
                    )));
                    Some(format!("[{}]({})", qname.id(), url))
                } else {
                    None
                }
            })
            .join(" | ");
        if linked_names.is_empty() {
            "".to_owned()
        } else {
            format!("\n---\nGo to {linked_names}")
        }
    };
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!(
                "```python\n{kind_formatted}{type_formatted}\n```{docstring_formatted}{symbol_def_loc_formatted}",
            ),
        }),
        range: None,
    })
}
