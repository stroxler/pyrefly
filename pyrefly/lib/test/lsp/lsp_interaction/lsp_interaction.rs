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
use lsp_types::notification::DidChangeWorkspaceFolders;
use lsp_types::notification::Notification as _;
use lsp_types::request::Request as _;
use lsp_types::request::WorkspaceConfiguration;

use crate::test::lsp::lsp_interaction::util::TestCase;
use crate::test::lsp::lsp_interaction::util::build_did_open_notification;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;
use crate::test::lsp::lsp_interaction::util::run_test_lsp;

#[test]
fn test_did_change_configuration() {
    let root = get_test_files_root();
    let scope_uri = Url::from_file_path(root.path()).unwrap();
    let mut messages_from_language_client = Vec::new();
    messages_from_language_client.push(Message::Notification(Notification {
        method: DidChangeConfiguration::METHOD.to_owned(),
        params: serde_json::json!({"settings": {}}),
    }));
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!([{}])),
        error: None,
    }));
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!([{}])),
        error: None,
    }));
    let params = serde_json::json!(ConfigurationParams {
        items: Vec::from([
            ConfigurationItem {
                scope_uri: Some(scope_uri.clone()),
                section: Some("python".to_owned()),
            },
            ConfigurationItem {
                scope_uri: None,
                section: Some("python".to_owned()),
            }
        ]),
    });
    let expected_messages_from_language_server = vec![
        Message::Request(Request {
            id: RequestId::from(1),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: params.clone(),
        }),
        Message::Request(Request {
            id: RequestId::from(2),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params,
        }),
    ];
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
        ..Default::default()
    });
}

#[test]
fn test_disable_language_services() {
    let test_files_root = get_test_files_root();
    let scope_uri = Url::from_file_path(test_files_root.path()).unwrap();
    let file_path = test_files_root.path().join("foo.py");
    let mut messages_from_language_client = Vec::new();
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!([{}])),
        error: None,
    }));
    messages_from_language_client.push(Message::from(build_did_open_notification(
        file_path.clone(),
    )));
    let go_to_definition_params = serde_json::json!({
        "textDocument": {
            "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
        },
        "position": {
            "line": 6,
            "character": 16
        }
    });
    messages_from_language_client.push(Message::from(Request {
        id: RequestId::from(2),
        method: "textDocument/definition".to_owned(),
        params: go_to_definition_params.clone(),
    }));
    messages_from_language_client.push(Message::Notification(Notification {
        method: DidChangeConfiguration::METHOD.to_owned(),
        params: serde_json::json!({"settings": {}}),
    }));
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!([{"pyrefly": {"disableLanguageServices": true}}, {"pyrefly": {"disableLanguageServices": true}}])),
        error: None,
    }));
    messages_from_language_client.push(Message::from(Request {
        id: RequestId::from(3),
        method: "textDocument/definition".to_owned(),
        params: go_to_definition_params.clone(),
    }));
    let mut expected_messages_from_language_server = Vec::new();
    let configuration_params = serde_json::json!(ConfigurationParams {
        items: Vec::from([
            ConfigurationItem {
                scope_uri: Some(scope_uri.clone()),
                section: Some("python".to_owned()),
            },
            ConfigurationItem {
                scope_uri: None,
                section: Some("python".to_owned()),
            }
        ]),
    });
    expected_messages_from_language_server.push(Message::Request(Request {
        id: RequestId::from(1),
        method: WorkspaceConfiguration::METHOD.to_owned(),
        params: configuration_params.clone(),
    }));
    expected_messages_from_language_server.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!({
            "uri": Url::from_file_path(test_files_root.path().join("bar.py")).unwrap().to_string(),
            "range": {
                "start": {
                    "line": 6,
                    "character": 6
                },
                "end": {
                    "line": 6,
                    "character": 9
                }
            }
        })),
        error: None,
    }));
    expected_messages_from_language_server.push(Message::Request(Request {
        id: RequestId::from(2),
        method: WorkspaceConfiguration::METHOD.to_owned(),
        params: configuration_params.clone(),
    }));
    expected_messages_from_language_server.push(Message::Response(Response {
        id: RequestId::from(3),
        result: Some(serde_json::json!([])),
        error: None,
    }));
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
        ..Default::default()
    });
}

#[test]
fn test_disable_language_services_default_workspace() {
    let test_files_root = get_test_files_root();
    let file_path = test_files_root.path().join("foo.py");
    let mut messages_from_language_client = Vec::new();
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!([{}])),
        error: None,
    }));
    messages_from_language_client.push(Message::from(build_did_open_notification(
        file_path.clone(),
    )));
    let go_to_definition_params = serde_json::json!({
        "textDocument": {
            "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
        },
        "position": {
            "line": 6,
            "character": 16
        }
    });
    messages_from_language_client.push(Message::from(Request {
        id: RequestId::from(2),
        method: "textDocument/definition".to_owned(),
        params: go_to_definition_params.clone(),
    }));
    messages_from_language_client.push(Message::Notification(Notification {
        method: DidChangeConfiguration::METHOD.to_owned(),
        params: serde_json::json!({"settings": {}}),
    }));
    messages_from_language_client.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!([{"pyrefly": {"disableLanguageServices": true}}, {"pyrefly": {"disableLanguageServices": true}}])),
        error: None,
    }));
    messages_from_language_client.push(Message::from(Request {
        id: RequestId::from(3),
        method: "textDocument/definition".to_owned(),
        params: go_to_definition_params.clone(),
    }));
    let mut expected_messages_from_language_server = Vec::new();
    let configuration_params = serde_json::json!(ConfigurationParams {
        items: Vec::from([ConfigurationItem {
            scope_uri: None,
            section: Some("python".to_owned()),
        }]),
    });
    expected_messages_from_language_server.push(Message::Request(Request {
        id: RequestId::from(1),
        method: WorkspaceConfiguration::METHOD.to_owned(),
        params: configuration_params.clone(),
    }));
    expected_messages_from_language_server.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!({
            "uri": Url::from_file_path(test_files_root.path().join("bar.py")).unwrap().to_string(),
            "range": {
                "start": {
                    "line": 6,
                    "character": 6
                },
                "end": {
                    "line": 6,
                    "character": 9
                }
            }
        })),
        error: None,
    }));
    expected_messages_from_language_server.push(Message::Request(Request {
        id: RequestId::from(2),
        method: WorkspaceConfiguration::METHOD.to_owned(),
        params: configuration_params.clone(),
    }));
    expected_messages_from_language_server.push(Message::Response(Response {
        id: RequestId::from(3),
        result: Some(serde_json::json!([])),
        error: None,
    }));
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        configuration: true,
        ..Default::default()
    });
}

#[test]
fn test_edits_while_recheck() {
    let mut test_messages = Vec::new();
    let mut expected_responses = Vec::new();
    let root = get_test_files_root();

    let path = root.path().join("foo.py");
    test_messages.push(Message::from(build_did_open_notification(path.clone())));
    // In this test, we trigger didSave and didChange to try to exercise the behavior
    // where we have concurrent in-memory recheck and on-disk recheck.
    test_messages.push(Message::from(Notification {
        method: "textDocument/didSave".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(&path).unwrap().to_string(),
                "languageId": "python",
                "version": 1,
                "text": std::fs::read_to_string(path.clone()).unwrap()
            }
        }),
    }));
    test_messages.push(Message::from(Notification {
        method: "textDocument/didChange".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(&path).unwrap().to_string(),
                "languageId": "python",
                "version": 2
            },
            "contentChanges": [
                {"text": format!("{}\n\nextra_stuff", std::fs::read_to_string(path).unwrap())}
            ],
        }),
    }));

    test_messages.push(Message::from(Request {
        id: RequestId::from(2),
        method: "textDocument/definition".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
            "position": {
                "line": 6,
                "character": 18
            }
        }),
    }));

    expected_responses.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!({
            "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string(),
            "range": {
                "start": {
                    "line": 6,
                    "character": 6
                },
                "end": {
                    "line": 6,
                    "character": 9
                }
            }
        })),
        error: None,
    }));

    run_test_lsp(TestCase {
        messages_from_language_client: test_messages,
        expected_messages_from_language_server: expected_responses,
        ..Default::default()
    });
}

#[test]
fn test_file_watcher() {
    let root = get_test_files_root();
    run_test_lsp(TestCase {
        messages_from_language_client: vec![Message::Response(Response {
            id: RequestId::from(1),
            result: None,
            error: None,
        })],
        expected_messages_from_language_server: vec![Message::Request(Request {
            id: RequestId::from(1),
            method: "client/registerCapability".to_owned(),
            params: serde_json::json!({
            "registrations": [{"id": "FILEWATCHER", "method": "workspace/didChangeWatchedFiles", "registerOptions": {"watchers": [
                {"globPattern": root.path().join("**/*.py").to_str().unwrap(), "kind": 7},
                {"globPattern": root.path().join("**/*.pyi").to_str().unwrap(), "kind": 7},
                {"globPattern": root.path().join("**/pyrefly.toml"), "kind": 7},
                {"globPattern": root.path().join("**/pyproject.toml"), "kind": 7}
            ]}}]}),
        })],
        workspace_folders: Some(vec![(
            "test".to_owned(),
            Url::from_file_path(root).unwrap(),
        )]),
        file_watch: true,
        ..Default::default()
    });
}

#[test]
fn test_did_change_workspace_folder() {
    let root = get_test_files_root();
    let scope_uri = Url::from_file_path(root.path()).unwrap();
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::Response(Response {
                id: RequestId::from(1),
                result: Some(serde_json::json!([{}])),
                error: None,
            }),
            Message::Notification(Notification {
                method: DidChangeWorkspaceFolders::METHOD.to_owned(),
                params: serde_json::json!({
                    "event": {
                    "added": [{"uri": Url::from_file_path(&root).unwrap(), "name": "test"}],
                    "removed": [],
                    }
                }),
            }),
            Message::Response(Response {
                id: RequestId::from(2),
                result: Some(serde_json::json!([{}, {}])),
                error: None,
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
                    items: Vec::from([
                        ConfigurationItem {
                            scope_uri: Some(scope_uri.clone()),
                            section: Some("python".to_owned()),
                        },
                        ConfigurationItem {
                            scope_uri: None,
                            section: Some("python".to_owned()),
                        }
                    ]),
                }),
            }),
        ],
        configuration: true,
        ..Default::default()
    });
}

fn get_diagnostics_result() -> serde_json::Value {
    serde_json::json!({"items": [
            {"code":"bad-argument-type","message":"`+` is not supported between `Literal[1]` and `Literal['']`\n  Argument `Literal['']` is not assignable to parameter `value` with type `int` in function `int.__add__`",
            "range":{"end":{"character":6,"line":5},"start":{"character":0,"line":5}},"severity":1,"source":"Pyrefly"}],"kind":"full"
    })
}

#[test]
fn test_diagnostics_default_workspace() {
    let root = get_test_files_root();
    let file_path = root.path().join("type_errors.py");
    let messages_from_language_client = vec![
        Message::from(build_did_open_notification(file_path.clone())),
        Message::from(Request {
            id: RequestId::from(1),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
    ];

    let expected_messages_from_language_server = vec![Message::Response(Response {
        id: RequestId::from(1),
        result: Some(get_diagnostics_result()),
        error: None,
    })];

    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        ..Default::default()
    });
}

#[test]
fn test_diagnostics_in_workspace() {
    let root = get_test_files_root();
    let file_path = root.path().join("type_errors.py");
    let messages_from_language_client = vec![
        Message::from(build_did_open_notification(file_path.clone())),
        Message::from(Request {
            id: RequestId::from(1),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
    ];

    let expected_messages_from_language_server = vec![Message::Response(Response {
        id: RequestId::from(1),
        result: Some(get_diagnostics_result()),
        error: None,
    })];

    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![(
            "test".to_owned(),
            Url::from_file_path(root).unwrap(),
        )]),
        ..Default::default()
    });
}

#[test]
fn test_unexpected_keyword_range() {
    let root = get_test_files_root();
    let file_path = root.path().join("unexpected_keyword.py");
    let messages_from_language_client = vec![
        Message::from(build_did_open_notification(file_path.clone())),
        Message::from(Request {
            id: RequestId::from(1),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
    ];

    let expected_messages_from_language_server = vec![Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!({
            "items": [
                {
                    "code": "unexpected-keyword",
                    "message": "Unexpected keyword argument `foo` in function `test`",
                    "range": {
                        "end": {"character": 8, "line": 4},
                        "start": {"character": 5, "line": 4}
                    },
                    "severity": 1,
                    "source": "Pyrefly"
                }
            ],
            "kind": "full"
        })),
        error: None,
    })];

    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![(
            "test".to_owned(),
            Url::from_file_path(root).unwrap(),
        )]),
        ..Default::default()
    });
}

#[test]
fn test_disable_type_errors_language_services_still_work() {
    let test_files_root = get_test_files_root();
    let scope_uri = Url::from_file_path(test_files_root.path()).unwrap();
    let file_path = test_files_root.path().join("foo.py");
    let messages_from_language_client = vec![
        Message::Response(Response {
            id: RequestId::from(1),
            result: Some(
                serde_json::json!([{"pyrefly": {"disableTypeErrors": true}}, {"pyrefly": {"disableTypeErrors": true}}]),
            ),
            error: None,
        }),
        Message::from(build_did_open_notification(file_path.clone())),
        Message::from(Request {
            id: RequestId::from(2),
            method: "textDocument/hover".to_owned(),
            params: serde_json::json!({
                "textDocument": {
                    "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
                },
                "position": {
                    "line": 6,
                    "character": 17
                }
            }),
        }),
    ];
    let expected_messages_from_language_server = vec![
        Message::Request(Request {
            id: RequestId::from(1),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: serde_json::json!(ConfigurationParams {
                items: Vec::from([
                    ConfigurationItem {
                        scope_uri: Some(scope_uri.clone()),
                        section: Some("python".to_owned()),
                    },
                    ConfigurationItem {
                        scope_uri: None,
                        section: Some("python".to_owned()),
                    }
                ]),
            }),
        }),
        Message::Response(Response {
            id: RequestId::from(2),
            result: Some(serde_json::json!({
                "contents": {"kind":"markdown","value":"```python\n(class) Bar: type[Bar]\n```"}
            })),
            error: None,
        }),
    ];
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
        ..Default::default()
    });
}

#[test]
fn test_disable_type_errors_workspace_folder() {
    let test_files_root = get_test_files_root();
    let scope_uri = Url::from_file_path(test_files_root.path()).unwrap();
    let file_path = test_files_root.path().join("type_errors.py");
    let configuration_request_params = serde_json::json!(ConfigurationParams {
        items: Vec::from([
            ConfigurationItem {
                scope_uri: Some(scope_uri.clone()),
                section: Some("python".to_owned()),
            },
            ConfigurationItem {
                scope_uri: None,
                section: Some("python".to_owned()),
            }
        ]),
    });

    let messages_from_language_client = vec![
        Message::from(build_did_open_notification(file_path.clone())),
        Message::Response(Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!([])),
            error: None,
        }),
        Message::from(Request {
            id: RequestId::from(2),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
        Message::Notification(Notification {
            method: DidChangeConfiguration::METHOD.to_owned(),
            params: serde_json::json!({"settings": {}}),
        }),
        Message::Response(Response {
            id: RequestId::from(2),
            result: Some(
                serde_json::json!([{"pyrefly": {"disableTypeErrors": true}}, {"pyrefly": {"disableTypeErrors": true}}]),
            ),
            error: None,
        }),
        Message::from(Request {
            id: RequestId::from(3),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
    ];
    let expected_messages_from_language_server = vec![
        Message::Request(Request {
            id: RequestId::from(1),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: configuration_request_params.clone(),
        }),
        Message::Response(Response {
            id: RequestId::from(2),
            result: Some(get_diagnostics_result()),
            error: None,
        }),
        Message::Request(Request {
            id: RequestId::from(2),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: configuration_request_params,
        }),
        Message::Response(Response {
            id: RequestId::from(3),
            result: Some(serde_json::json!({"items": [], "kind": "full"})),
            error: None,
        }),
    ];
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
        ..Default::default()
    });
}

#[test]
fn test_disable_type_errors_default_workspace() {
    let test_files_root = get_test_files_root();
    let file_path = test_files_root.path().join("type_errors.py");
    let messages_from_language_client = vec![
        Message::Response(Response {
            id: RequestId::from(1),
            result: Some(serde_json::json!([])),
            error: None,
        }),
        Message::from(build_did_open_notification(file_path.clone())),
        Message::from(Request {
            id: RequestId::from(2),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
        Message::Notification(Notification {
            method: DidChangeConfiguration::METHOD.to_owned(),
            params: serde_json::json!({"settings": {}}),
        }),
        Message::Response(Response {
            id: RequestId::from(2),
            result: Some(
                serde_json::json!([{"pyrefly": {"disableTypeErrors": true}}, {"pyrefly": {"disableTypeErrors": true}}]),
            ),
            error: None,
        }),
        Message::from(Request {
            id: RequestId::from(3),
            method: "textDocument/diagnostic".to_owned(),
            params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(file_path.clone()).unwrap().to_string()
            }}),
        }),
    ];
    let expected_messages_from_language_server = vec![
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
        Message::Response(Response {
            id: RequestId::from(2),
            result: Some(get_diagnostics_result()),
            error: None,
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
            result: Some(serde_json::json!({"items": [], "kind": "full"})),
            error: None,
        }),
    ];
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        configuration: true,
        ..Default::default()
    });
}

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
