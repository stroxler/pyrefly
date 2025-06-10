/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use lsp_types::SemanticToken;
use lsp_types::SemanticTokenModifier;
use lsp_types::SemanticTokenType;
use lsp_types::SemanticTokensLegend;
use ruff_text_size::TextRange;

use crate::module::module_info::ModuleInfo;

pub struct SemanticTokensLegends {
    token_types_index: HashMap<SemanticTokenType, u32>,
}

impl SemanticTokensLegends {
    pub fn lsp_semantic_token_legends() -> SemanticTokensLegend {
        SemanticTokensLegend {
            token_types: vec![
                SemanticTokenType::NAMESPACE,
                SemanticTokenType::TYPE,
                SemanticTokenType::CLASS,
                SemanticTokenType::ENUM,
                SemanticTokenType::INTERFACE,
                SemanticTokenType::STRUCT,
                SemanticTokenType::TYPE_PARAMETER,
                SemanticTokenType::PARAMETER,
                SemanticTokenType::VARIABLE,
                SemanticTokenType::PROPERTY,
                SemanticTokenType::ENUM_MEMBER,
                SemanticTokenType::EVENT,
                SemanticTokenType::FUNCTION,
                SemanticTokenType::METHOD,
                SemanticTokenType::MACRO,
                SemanticTokenType::KEYWORD,
                SemanticTokenType::MODIFIER,
                SemanticTokenType::COMMENT,
                SemanticTokenType::STRING,
                SemanticTokenType::NUMBER,
                SemanticTokenType::REGEXP,
                SemanticTokenType::OPERATOR,
                SemanticTokenType::DECORATOR,
            ],
            token_modifiers: vec![
                SemanticTokenModifier::DECLARATION,
                SemanticTokenModifier::DEFINITION,
                SemanticTokenModifier::READONLY,
                SemanticTokenModifier::STATIC,
                SemanticTokenModifier::DEPRECATED,
                SemanticTokenModifier::ABSTRACT,
                SemanticTokenModifier::ASYNC,
                SemanticTokenModifier::MODIFICATION,
                SemanticTokenModifier::DOCUMENTATION,
                SemanticTokenModifier::DEFAULT_LIBRARY,
            ],
        }
    }

    pub fn new() -> Self {
        let lsp_legend = Self::lsp_semantic_token_legends();
        let mut token_types_index = HashMap::new();
        for (i, token_type) in lsp_legend.token_types.iter().enumerate() {
            token_types_index.insert(token_type.clone(), i as u32);
        }
        Self { token_types_index }
    }

    pub fn convert_tokens_into_lsp_semantic_tokens(
        &self,
        mut tokens: Vec<SemanticTokenWithFullRange>,
        module_info: ModuleInfo,
    ) -> Vec<SemanticToken> {
        tokens.sort_by(|a, b| a.range.start().cmp(&b.range.start()));
        let mut previous_line = 0;
        let mut previous_col = 0;
        let mut lsp_semantic_tokens = Vec::new();
        for token in tokens {
            let source_range = module_info.source_range(token.range);
            let length = token.range.len().to_u32();
            let current_line = source_range.start.line.to_zero_indexed();
            let current_col = source_range.start.column.to_zero_indexed();
            let (delta_line, delta_start) = if previous_line == current_line {
                let delta_start = current_col - previous_col;
                previous_col = current_col;
                (0, delta_start as u32)
            } else {
                let delta_line = current_line - previous_line;
                previous_line = current_line;
                previous_col = current_col;
                (delta_line as u32, current_col as u32)
            };
            lsp_semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: *self.token_types_index.get(&token.token_type).unwrap(),
                // todo(samzhou19815): add support for token modifiers
                token_modifiers_bitset: 0,
            });
        }
        lsp_semantic_tokens
    }
}

pub struct SemanticTokenWithFullRange {
    pub range: TextRange,
    pub token_type: SemanticTokenType,
}
