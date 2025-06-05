/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use lsp_types::CompletionItemKind;

/// The kind of symbol of a binding.
/// It will be displayed in IDEs with different icons.
/// https://adamcoster.com/blog/vscode-workspace-symbol-provider-purpose might give you an idea of
/// how it will look in VSCode.
#[derive(Debug, Clone, Copy, Dupe)]
pub enum SymbolKind {
    Module,
    Attribute,
    Variable,
    Parameter,
    TypeParameter,
    TypeAlias,
    Function,
    Class,
    Bool,
    Str,
}

impl SymbolKind {
    pub fn to_lsp_completion_item_kind(self) -> CompletionItemKind {
        match self {
            SymbolKind::Module => CompletionItemKind::MODULE,
            SymbolKind::Attribute => CompletionItemKind::FIELD,
            SymbolKind::Variable => CompletionItemKind::VARIABLE,
            SymbolKind::Parameter => CompletionItemKind::VARIABLE,
            SymbolKind::TypeParameter => CompletionItemKind::TYPE_PARAMETER,
            SymbolKind::TypeAlias => CompletionItemKind::INTERFACE,
            SymbolKind::Function => CompletionItemKind::FUNCTION,
            SymbolKind::Class => CompletionItemKind::CLASS,
            SymbolKind::Bool => CompletionItemKind::VALUE,
            SymbolKind::Str => CompletionItemKind::VALUE,
        }
    }
}
