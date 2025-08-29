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
use lsp_types::ConfigurationItem;
use lsp_types::ConfigurationParams;
use lsp_types::Url;
use lsp_types::notification::DidChangeConfiguration;
use lsp_types::notification::Notification as _;
use lsp_types::request::Request as _;
use lsp_types::request::WorkspaceConfiguration;

use crate::test::lsp::lsp_interaction::util::TestCase;
use crate::test::lsp::lsp_interaction::util::build_did_open_notification;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;
#[allow(deprecated)]
use crate::test::lsp::lsp_interaction::util::run_test_lsp;

#[test]
#[allow(deprecated)]
fn test_inlay_hint_default_config() {
    let root = get_test_files_root();
    let request_file_name = root.path().join("inlay_hint_test.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(
                root.path().join(request_file_name.clone()),
            )),
            Message::from(Request {
                id: RequestId::from(2),
                method: "textDocument/inlayHint".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join(request_file_name)).unwrap().to_string()
                    },
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 100, "character": 0}
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
            id: RequestId::from(2),
            result: Some(serde_json::json!([
                {
                    "label":" -> tuple[Literal[1], Literal[2]]",
                    "position":{"character":21,"line":6},
                    "textEdits":[{
                        "newText":" -> tuple[Literal[1], Literal[2]]",
                        "range":{"end":{"character":21,"line":6},"start":{"character":21,"line":6}}
                    }]
                },
                {
                    "label":": tuple[Literal[1], Literal[2]]",
                    "position":{"character":6,"line":11},
                    "textEdits":[{
                        "newText":": tuple[Literal[1], Literal[2]]",
                        "range":{"end":{"character":6,"line":11},"start":{"character":6,"line":11}}
                    }]
                },
                {
                    "label":" -> Literal[0]",
                    "position":{"character":15,"line":14},
                    "textEdits":[{
                        "newText":" -> Literal[0]",
                        "range":{"end":{"character":15,"line":14},"start":{"character":15,"line":14}}
                    }]
                }
            ])),
            error: None,
        })],
        ..Default::default()
    });
}

#[test]
#[allow(deprecated)]
fn test_inlay_hint_disable_all() {
    let root = get_test_files_root();
    let request_file_name = root.path().join("inlay_hint_test.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::Response(Response {
                id: RequestId::from(1),
                result: Some(serde_json::json!([])),
                error: None,
            }),
            Message::from(build_did_open_notification(
                root.path().join(request_file_name.clone()),
            )),
            Message::Notification(Notification {
                method: DidChangeConfiguration::METHOD.to_owned(),
                params: serde_json::json!({"settings": {}}),
            }),
            Message::Response(Response {
                id: RequestId::from(2),
                result: Some(serde_json::json!([
                    {
                        "analysis": {
                            "inlayHints": {
                                "callArgumentNames": "on",
                                "functionReturnTypes": false,
                                "pytestParameters": false,
                                "variableTypes": false
                            },
                        }
                    },
                ])),
                error: None,
            }),
            Message::from(Request {
                id: RequestId::from(3),
                method: "textDocument/inlayHint".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join(request_file_name)).unwrap().to_string()
                    },
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 100, "character": 0}
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![
            Message::Request(Request {
                id: RequestId::from(1),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Request(Request {
                id: RequestId::from(2),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Response(Response {
                id: RequestId::from(3),
                result: Some(serde_json::json!([])),
                error: None,
            }),
        ],
        configuration: true,
        ..Default::default()
    });
}

#[test]
#[allow(deprecated)]
fn test_inlay_hint_disable_variables() {
    let root = get_test_files_root();
    let request_file_name = root.path().join("inlay_hint_test.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::Response(Response {
                id: RequestId::from(1),
                result: Some(serde_json::json!([])),
                error: None,
            }),
            Message::from(build_did_open_notification(
                root.path().join(request_file_name.clone()),
            )),
            Message::Notification(Notification {
                method: DidChangeConfiguration::METHOD.to_owned(),
                params: serde_json::json!({"settings": {}}),
            }),
            Message::Response(Response {
                id: RequestId::from(2),
                result: Some(serde_json::json!([
                    {
                        "analysis": {
                            "inlayHints": {
                                "variableTypes": false
                            },
                        }
                    },
                ])),
                error: None,
            }),
            Message::from(Request {
                id: RequestId::from(3),
                method: "textDocument/inlayHint".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join(request_file_name)).unwrap().to_string()
                    },
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 100, "character": 0}
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![
            Message::Request(Request {
                id: RequestId::from(1),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Request(Request {
                id: RequestId::from(2),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Response(Response {
                id: RequestId::from(3),
                result: Some(serde_json::json!([{
                    "label":" -> tuple[Literal[1], Literal[2]]",
                    "position":{"character":21,"line":6},
                    "textEdits":[{
                        "newText":" -> tuple[Literal[1], Literal[2]]",
                        "range":{"end":{"character":21,"line":6},"start":{"character":21,"line":6}}
                    }]
                },
                {
                    "label":" -> Literal[0]",
                    "position":{"character":15,"line":14},
                    "textEdits":[{
                        "newText":" -> Literal[0]",
                        "range":{"end":{"character":15,"line":14},"start":{"character":15,"line":14}}
                    }]
                }])),
                error: None,
            }),
        ],
        configuration: true,
        ..Default::default()
    });
}

#[test]
#[allow(deprecated)]
fn test_inlay_hint_disable_returns() {
    let root = get_test_files_root();
    let request_file_name = root.path().join("inlay_hint_test.py");
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::Response(Response {
                id: RequestId::from(1),
                result: Some(serde_json::json!([])),
                error: None,
            }),
            Message::from(build_did_open_notification(
                root.path().join(request_file_name.clone()),
            )),
            Message::Notification(Notification {
                method: DidChangeConfiguration::METHOD.to_owned(),
                params: serde_json::json!({"settings": {}}),
            }),
            Message::Response(Response {
                id: RequestId::from(2),
                result: Some(serde_json::json!([
                    {
                        "analysis": {
                            "inlayHints": {
                                "functionReturnTypes": false
                            },
                        }
                    },
                ])),
                error: None,
            }),
            Message::from(Request {
                id: RequestId::from(3),
                method: "textDocument/inlayHint".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join(request_file_name)).unwrap().to_string()
                    },
                    "range": {
                        "start": {"line": 0, "character": 0},
                        "end": {"line": 100, "character": 0}
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![
            Message::Request(Request {
                id: RequestId::from(1),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Request(Request {
                id: RequestId::from(2),
                method: WorkspaceConfiguration::METHOD.to_owned(),
                params: serde_json::json!(ConfigurationParams {
                    items: Vec::from([ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }]),
                }),
            }),
            Message::Response(Response {
                id: RequestId::from(3),
                result: Some(serde_json::json!([{
                    "label":": tuple[Literal[1], Literal[2]]",
                    "position":{"character":6,"line":11},
                    "textEdits":[{
                        "newText":": tuple[Literal[1], Literal[2]]",
                        "range":{"end":{"character":6,"line":11},"start":{"character":6,"line":11}}
                    }]
                }])),
                error: None,
            }),
        ],
        configuration: true,
        ..Default::default()
    });
}
