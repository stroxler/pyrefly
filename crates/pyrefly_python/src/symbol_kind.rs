/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use lsp_types::CompletionItemKind;
use lsp_types::SemanticTokenModifier;
use lsp_types::SemanticTokenType;

/// The kind of symbol of a binding.
/// It will be displayed in IDEs with different icons.
/// https://adamcoster.com/blog/vscode-workspace-symbol-provider-purpose might give you an idea of
/// how it will look in VSCode.
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolKind {
    Module,
    Attribute,
    Variable,
    Constant,
    Parameter,
    TypeParameter,
    TypeAlias,
    Function,
    Class,
}

impl SymbolKind {
    pub fn to_lsp_symbol_kind(self) -> lsp_types::SymbolKind {
        match self {
            SymbolKind::Module => lsp_types::SymbolKind::MODULE,
            SymbolKind::Attribute => lsp_types::SymbolKind::FIELD,
            SymbolKind::Variable => lsp_types::SymbolKind::VARIABLE,
            SymbolKind::Constant => lsp_types::SymbolKind::CONSTANT,
            SymbolKind::Parameter => lsp_types::SymbolKind::VARIABLE,
            SymbolKind::TypeParameter => lsp_types::SymbolKind::TYPE_PARAMETER,
            SymbolKind::TypeAlias => lsp_types::SymbolKind::INTERFACE,
            SymbolKind::Function => lsp_types::SymbolKind::FUNCTION,
            SymbolKind::Class => lsp_types::SymbolKind::CLASS,
        }
    }

    pub fn to_lsp_completion_item_kind(self) -> CompletionItemKind {
        match self {
            SymbolKind::Module => CompletionItemKind::MODULE,
            SymbolKind::Attribute => CompletionItemKind::FIELD,
            SymbolKind::Variable => CompletionItemKind::VARIABLE,
            SymbolKind::Constant => CompletionItemKind::CONSTANT,
            SymbolKind::Parameter => CompletionItemKind::VARIABLE,
            SymbolKind::TypeParameter => CompletionItemKind::TYPE_PARAMETER,
            SymbolKind::TypeAlias => CompletionItemKind::INTERFACE,
            SymbolKind::Function => CompletionItemKind::FUNCTION,
            SymbolKind::Class => CompletionItemKind::CLASS,
        }
    }

    pub fn display_for_hover(self) -> String {
        match self {
            SymbolKind::Module => "(module)".to_owned(),
            SymbolKind::Attribute => "(attribute)".to_owned(),
            SymbolKind::Variable => "(variable)".to_owned(),
            SymbolKind::Constant => "(constant)".to_owned(),
            SymbolKind::Parameter => "(parameter)".to_owned(),
            SymbolKind::TypeParameter => "(type parameter)".to_owned(),
            SymbolKind::TypeAlias => "(type alias)".to_owned(),
            SymbolKind::Function => "(function)".to_owned(),
            SymbolKind::Class => "(class)".to_owned(),
        }
    }

    pub fn to_lsp_semantic_token_type_with_modifiers(
        self,
    ) -> (SemanticTokenType, Vec<SemanticTokenModifier>) {
        match self {
            SymbolKind::Module => (SemanticTokenType::NAMESPACE, vec![]),
            SymbolKind::Attribute => (SemanticTokenType::PROPERTY, vec![]),
            SymbolKind::Variable => (SemanticTokenType::VARIABLE, vec![]),
            SymbolKind::Constant => (
                SemanticTokenType::VARIABLE,
                vec![SemanticTokenModifier::READONLY],
            ),
            SymbolKind::Parameter => (SemanticTokenType::PARAMETER, vec![]),
            SymbolKind::TypeParameter => (SemanticTokenType::TYPE_PARAMETER, vec![]),
            SymbolKind::TypeAlias => (SemanticTokenType::INTERFACE, vec![]),
            // todo(samzhou19815): modifier for async
            SymbolKind::Function => (SemanticTokenType::FUNCTION, vec![]),
            SymbolKind::Class => (SemanticTokenType::CLASS, vec![]),
        }
    }
}
