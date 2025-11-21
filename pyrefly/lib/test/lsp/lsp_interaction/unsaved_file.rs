/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_server::RequestId;
use lsp_types::SemanticTokensResult;
use lsp_types::Url;
use lsp_types::request::Completion;
use lsp_types::request::SemanticTokensFullRequest;
use serde_json::json;

use crate::test::lsp::lsp_interaction::object_model::InitializeSettings;
use crate::test::lsp::lsp_interaction::object_model::LspInteraction;

#[test]
fn test_semantic_tokens_for_unsaved_file() {
    let mut interaction = LspInteraction::new();
    interaction.initialize(InitializeSettings::default());

    let uri = Url::parse("untitled:Untitled-1").unwrap();
    let text = r#"def foo():
    return 1

foo()
"#;
    interaction.client.did_open_uri(&uri, "python", text);

    interaction
        .client
        .send_request::<SemanticTokensFullRequest>(
            RequestId::from(2),
            json!({
                "textDocument": { "uri": uri.to_string() }
            }),
        );

    interaction
        .client
        .expect_response_with::<SemanticTokensFullRequest>(RequestId::from(2), |response| {
            match response {
                Some(SemanticTokensResult::Tokens(xs)) => !xs.data.is_empty(),
                _ => false,
            }
        });

    interaction.shutdown();
}

#[test]
fn test_completion_for_unsaved_file() {
    let mut interaction = LspInteraction::new();
    interaction.initialize(InitializeSettings::default());

    let uri = Url::parse("untitled:Untitled-2").unwrap();
    let text = r#"import math
math.
"#;
    interaction.client.did_open_uri(&uri, "python", text);

    interaction.client.send_request::<Completion>(
        RequestId::from(2),
        json!({
            "textDocument": {"uri": uri.to_string()},
            "position": {"line": 1, "character": 5}
        }),
    );

    interaction
        .client
        .expect_completion_response_with(RequestId::from(2), |list| !list.items.is_empty());

    interaction.shutdown();
}
