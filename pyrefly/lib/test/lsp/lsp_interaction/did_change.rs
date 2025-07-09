/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types::Url;

use crate::test::lsp::lsp_interaction::util::TestCase;
use crate::test::lsp::lsp_interaction::util::build_did_open_notification;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;
use crate::test::lsp::lsp_interaction::util::run_test_lsp;

#[test]
fn test_text_document_did_change() {
    let root = get_test_files_root();
    let filepath = root.path().join("text_document.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(filepath.clone())),
            Message::from(Notification {
                method: "textDocument/didChange".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(&filepath).unwrap().to_string(),
                        "languageId": "python",
                        "version": 2
                    },
                    "contentChanges": [{
                        "range": {
                            "start": {"line": 6, "character": 0},
                            "end": {"line": 7, "character": 0}
                        },
                        "text": format!("{}\n", "rint(\"another change\")")
                    }],
                }),
            }),
            Message::from(Notification {
                method: "textDocument/didChange".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(&filepath).unwrap().to_string(),
                        "languageId": "python",
                        "version": 3
                    },
                    "contentChanges": [{
                        "range": {
                            "start": {"line": 6, "character": 0},
                            "end": {"line": 6, "character": 0},
                        },
                        "text": format!("{}", "p")
                    }],
                }),
            }),
            Message::from(Request {
                id: RequestId::from(1),
                method: "textDocument/diagnostic".to_owned(),
                params: serde_json::json!({
                "textDocument": {
                    "uri": Url::from_file_path(&filepath).unwrap().to_string()
                }}),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!({"items": [], "kind": "full"})),
            error: None,
        })],
        ..Default::default()
    });
}

#[test]
fn test_text_document_did_change_unicode() {
    let root = get_test_files_root();
    let filepath = root.path().join("utf.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(filepath.clone())),
            Message::from(Notification {
                method: "textDocument/didChange".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(&filepath).unwrap().to_string(),
                        "languageId": "python",
                        "version": 2
                    },
                    "contentChanges": [{
                        "range": {
                            "start": { "line": 7, "character": 8 },
                            "end": { "line": 8, "character": 2 }
                        },
                        "rangeLength": 3,
                        "text": ""
                    }]
                }),
            }),
            Message::from(Notification {
                method: "textDocument/didChange".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(&filepath).unwrap().to_string(),
                        "languageId": "python",
                        "version": 3
                    },
                    "contentChanges": [{
                        "range": {
                            "start": { "line": 7, "character": 8 },
                            "end": { "line": 7, "character": 8 }
                        },
                        "rangeLength": 0,
                        "text": format!("\n{}", "print(\"")
                    }]
                }),
            }),
            Message::from(Request {
                id: RequestId::from(1),
                method: "textDocument/diagnostic".to_owned(),
                params: serde_json::json!({
                "textDocument": {
                    "uri": Url::from_file_path(&filepath).unwrap().to_string()
                }}),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!({"items": [], "kind": "full"})),
            error: None,
        })],
        ..Default::default()
    });
}
