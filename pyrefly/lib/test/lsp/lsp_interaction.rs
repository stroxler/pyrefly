/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use core::panic;
use std::path::PathBuf;
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use crossbeam_channel::RecvTimeoutError;
use crossbeam_channel::bounded;
use lsp_server::Connection;
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
use pretty_assertions::assert_eq;
use tempfile::TempDir;

use crate::commands::lsp::Args;
use crate::commands::lsp::run_lsp;
use crate::test::util::init_test;
use crate::util::fs_anyhow;

struct TestCase {
    messages_from_language_client: Vec<Message>,
    expected_messages_from_language_server: Vec<Message>,
    search_path: Vec<PathBuf>,
    experimental_project_path: Vec<PathBuf>,
    /// workspace folders open in the client
    workspace_folders: Option<Vec<(String, Url)>>,
    /// if client has configuration capability
    configuration: bool,
}

fn run_test_lsp(test_case: TestCase) {
    init_test();
    let timeout = Duration::from_secs(25);
    let args = Args {
        search_path: test_case.search_path,
        site_package_path: Vec::new(),
        experimental_project_path: test_case.experimental_project_path.clone(),
    };
    // language_client_sender is used to send messages to the language client
    // language_client_receiver sees messages sent to the language client
    let (language_client_sender, language_client_receiver) = bounded::<Message>(0);
    // language_server_sender is used to send messages to the language server
    // language_server_receiver sees messages sent to the language server
    let (language_server_sender, language_server_receiver) = bounded::<Message>(0);

    // was there a response after a client -> server request
    let (server_response_received_sender, server_response_received_receiver) =
        bounded::<RequestId>(0);
    // was there a response after a server -> client request
    let (client_request_received_sender, client_request_received_receiver) =
        bounded::<RequestId>(0);

    let connection = Connection {
        sender: language_client_sender,
        receiver: language_server_receiver,
    };

    thread::scope(|scope| {
        // this thread runs the language server
        scope.spawn(move || {
            run_lsp(Arc::new(connection), || Ok(()), args)
                .map(|_| ())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))
        });
        // this thread sends messages to the language server (from test case)
        scope.spawn(move || {
            for msg in
                get_initialize_messages(&test_case.workspace_folders, test_case.configuration)
                    .into_iter()
                    .chain(test_case.messages_from_language_client)
            {
                let send = || {
                    eprintln!("client--->server {}", serde_json::to_string(&msg).unwrap());
                    if let Err(err) = language_server_sender.send_timeout(msg.clone(), timeout) {
                        panic!("Failed to send message to language server: {:?}", err);
                    }
                };
                match &msg {
                    Message::Request(Request {
                        id,
                        method: _,
                        params: _,
                    }) => {
                        send();
                        if let Ok(response) =
                            server_response_received_receiver.recv_timeout(timeout)
                            && response == *id
                        {
                            // continue
                        } else {
                            panic!("Did not receive response for request {:?}", id);
                        }
                    }
                    Message::Notification(_) => send(),
                    // Language client responses need to ensure the request was sent first
                    Message::Response(Response {
                        id,
                        result: _,
                        error: _,
                    }) => {
                        if let Ok(response) = client_request_received_receiver.recv_timeout(timeout)
                            && response == *id
                        {
                            send();
                        } else {
                            panic!("Did not receive request for intended response {:?}", &msg);
                        }
                    }
                }
            }
        });
        // this thread receives messages from the language server and validates responses
        scope.spawn(move || {
            let initialize_responses = if test_case.experimental_project_path.is_empty() {
                get_initialize_responses()
            } else {
                get_initialize_responses_with_find_refs_enabled()
            };
            let mut responses = initialize_responses
                .into_iter()
                .chain(test_case.expected_messages_from_language_server)
                .collect::<Vec<_>>();

            loop {
                if responses.is_empty() {
                    break;
                }
                match language_client_receiver.recv_timeout(timeout) {
                    Ok(msg) => {
                        eprintln!("client<---server {}", serde_json::to_string(&msg).unwrap());
                        let mut assert = || {
                            assert_eq!(
                                serde_json::to_string(&msg).unwrap(),
                                serde_json::to_string(&responses.remove(0)).unwrap(),
                                "Response mismatch"
                            );
                        };
                        match &msg {
                            Message::Response(Response {
                                id,
                                result: _,
                                error: _,
                            }) => {
                                assert();
                                server_response_received_sender.send(id.clone()).unwrap();
                            }
                            Message::Notification(notification) => {
                                eprintln!("Received notification: {:?}", notification);
                            }
                            Message::Request(Request {
                                id,
                                method: _,
                                params: _,
                            }) => {
                                assert();
                                client_request_received_sender.send(id.clone()).unwrap();
                            }
                        };
                    }
                    Err(RecvTimeoutError::Timeout) => {
                        panic!("Timeout waiting for response. Expected ${:?}.", responses,);
                    }
                    Err(RecvTimeoutError::Disconnected) => {
                        panic!("Channel disconnected. Expected ${:?}.", responses);
                    }
                }
            }
        });
    });
}

fn get_initialize_params(
    workspace_folders: &Option<Vec<(String, Url)>>,
    configuration: bool,
) -> serde_json::Value {
    let mut params = serde_json::json!({
        "rootPath": "/",
        "processId": std::process::id(),
        "trace": "verbose",
        "clientInfo": { "name": "debug" },
        "capabilities": {
            "textDocument": {
                "publishDiagnostics": {
                    "relatedInformation": true,
                    "versionSupport": false,
                    "tagSupport": {
                        "valueSet": [1, 2],
                    },
                    "codeDescriptionSupport": true,
                    "dataSupport": true,
                },
            },
        },
    });

    if let Some(folders) = workspace_folders {
        params["capabilities"]["workspace"]["workspaceFolders"] = serde_json::json!(true);
        params["workspaceFolders"] = serde_json::json!(
            folders
                .iter()
                .map(|(name, path)| serde_json::json!({"name": name, "uri": path.to_string()}))
                .collect::<Vec<_>>()
        );
    }
    if configuration {
        params["capabilities"]["workspace"]["configuration"] = serde_json::json!(true);
    }

    params
}

fn get_initialize_messages(
    workspace_folders: &Option<Vec<(String, Url)>>,
    configuration: bool,
) -> std::vec::Vec<lsp_server::Message> {
    vec![
        Message::from(Request {
            id: RequestId::from(1),
            method: "initialize".to_owned(),
            params: get_initialize_params(workspace_folders, configuration),
        }),
        Message::from(Notification {
            method: "initialized".to_owned(),
            params: serde_json::json!({}),
        }),
    ]
}

fn get_initialize_responses() -> Vec<Message> {
    vec![Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!({
            "capabilities": {
                "completionProvider": { "triggerCharacters": ["."]},
                "definitionProvider": true,
                "documentHighlightProvider":true,
                "documentSymbolProvider":true,
                "hoverProvider": true,
                "inlayHintProvider": true,
                "textDocumentSync": 1
            }
        }
        )),
        error: None,
    })]
}

fn get_initialize_responses_with_find_refs_enabled() -> Vec<Message> {
    vec![Message::Response(Response {
        id: RequestId::from(1),
        result: Some(serde_json::json!({
            "capabilities": {
                "completionProvider": { "triggerCharacters": ["."]},
                "definitionProvider": true,
                "documentHighlightProvider":true,
                "documentSymbolProvider":true,
                "hoverProvider": true,
                "referencesProvider": true,
                "inlayHintProvider": true,
                "textDocumentSync": 1
            }
        }
        )),
        error: None,
    })]
}

fn build_did_open_notification(path: PathBuf) -> lsp_server::Notification {
    Notification {
        method: "textDocument/didOpen".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(&path).unwrap().to_string(),
                "languageId": "python",
                "version": 1,
                "text": std::fs::read_to_string(path).unwrap()
            }
        }),
    }
}

fn get_test_files_root() -> TempDir {
    let mut source_files =
        std::env::current_dir().expect("std:env::current_dir() unavailable for test");
    let test_files_path = std::env::var("TEST_FILES_PATH")
        .expect("TEST_FILES_PATH env var not set: cargo or buck should set this automatically");
    source_files.push(test_files_path);

    // We copy all files over to a separate temp directory so we are consistent between Cargo and Buck.
    // In particular, given the current directory, Cargo is likely to find a pyproject.toml, but Buck won't.
    let t = TempDir::new().unwrap();
    for x in fs_anyhow::read_dir(&source_files).unwrap() {
        let name = x.unwrap().file_name();
        std::fs::copy(source_files.join(&name), t.path().join(&name)).unwrap();
    }
    t
}

#[test]
fn test_initialize() {
    run_test_lsp(TestCase {
        messages_from_language_client: Vec::new(),
        expected_messages_from_language_server: Vec::new(),
        search_path: Vec::new(),
        experimental_project_path: Vec::new(),
        workspace_folders: None,
        configuration: false,
    });
}

#[test]
fn test_initialize_with_python_path() {
    let scope_uri = Url::from_file_path(get_test_files_root()).unwrap();
    let python_path = "/path/to/python/interpreter";
    let id = RequestId::from(1);
    run_test_lsp(TestCase {
        messages_from_language_client: vec![Message::Response(Response {
            id: id.clone(),
            result: Some(serde_json::json!([{"pythonPath": python_path}])),
            error: None,
        })],
        expected_messages_from_language_server: vec![Message::Request(Request {
            id,
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: serde_json::json!(ConfigurationParams {
                items: Vec::from([ConfigurationItem {
                    scope_uri: Some(scope_uri.clone()),
                    section: Some("python".to_owned()),
                }]),
            }),
        })],
        search_path: Vec::new(),
        experimental_project_path: Vec::new(),
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
    });
}

fn test_go_to_def(
    root: &TempDir,
    workspace_folders: Option<Vec<(String, Url)>>,
    search_path: Vec<PathBuf>,
) {
    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(root.path().join("foo.py"))),
            Message::from(Request {
                id: RequestId::from(2),
                method: "textDocument/definition".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
                    },
                    "position": {
                        "line": 5,
                        "character": 16
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
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
        })],
        search_path,
        experimental_project_path: Vec::new(),
        workspace_folders,
        configuration: false,
    });
}

#[test]
fn test_go_to_def_single_root() {
    let root = get_test_files_root();
    test_go_to_def(
        &root,
        Some(vec![(
            "test".to_owned(),
            Url::from_file_path(root.path()).unwrap(),
        )]),
        Vec::new(), // should use search_path from workspace root
    );
}

#[test]
fn test_go_to_def_no_root() {
    let root = get_test_files_root();
    test_go_to_def(&root, Some(vec![]), vec![root.path().to_owned()]);
}

#[test]
fn test_go_to_def_no_folder_capability() {
    let root = get_test_files_root();
    test_go_to_def(&root, None, vec![root.path().to_owned()]);
}

#[test]
fn test_hover() {
    let root = get_test_files_root();

    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(root.path().join("foo.py"))),
            Message::from(Request {
                id: RequestId::from(2),
                method: "textDocument/hover".to_owned(),
                params: serde_json::json!({
                    "textDocument": {
                        "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
                    },
                    "position": {
                        "line": 5,
                        "character": 16
                    }
                }),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
            id: RequestId::from(2),
            result: Some(serde_json::json!({"contents": {
                "kind": "markdown",
                "value": "```python\ntype[Bar]\n```",
            }})),
            error: None,
        })],
        search_path: Vec::new(),
        experimental_project_path: Vec::new(),
        workspace_folders: None,
        configuration: false,
    });
}

#[test]
fn test_references() {
    let root = get_test_files_root();
    let mut test_messages = Vec::new();
    let mut expected_responses = Vec::new();
    test_messages.push(Message::from(build_did_open_notification(
        root.path().join("bar.py"),
    )));

    // Find reference from a reference location in the same in-memory file
    test_messages.push(Message::from(Request {
        id: RequestId::from(2),
        method: "textDocument/references".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            "position": {
                "line": 9,
                "character": 1
            },
            "context": {
                "includeDeclaration": true
            },
        }),
    }));

    expected_responses.push(Message::Response(Response {
        id: RequestId::from(2),
        result: Some(serde_json::json!([
            {
                "range": {"start":{"line":5,"character":16},"end":{"line":5, "character":19}},
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":6,"character":6},"end":{"character":9,"line":6}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":9,"character":0},"end":{"character":3,"line":9}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
        ])),
        error: None,
    }));

    // Find reference from a definition location in the same in-memory file
    test_messages.push(Message::from(Request {
        id: RequestId::from(3),
        method: "textDocument/references".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            "position": {
                "line": 6,
                "character": 7
            },
            "context": {
                "includeDeclaration": true
            },
        }),
    }));

    expected_responses.push(Message::Response(Response {
        id: RequestId::from(3),
        result: Some(serde_json::json!([
            {
                "range": {"start":{"line":5,"character":16},"end":{"line":5, "character":19}},
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":6,"character":6},"end":{"character":9,"line":6}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":9,"character":0},"end":{"character":3,"line":9}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
        ])),
        error: None,
    }));

    test_messages.push(Message::from(build_did_open_notification(
        root.path().join("foo.py"),
    )));

    // Find reference from a reference location in a different file
    test_messages.push(Message::from(Request {
        id: RequestId::from(4),
        method: "textDocument/references".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
            "position": {
                "line": 5,
                "character": 17
            },
            "context": {
                "includeDeclaration": true
            },
        }),
    }));

    expected_responses.push(Message::Response(Response {
        id: RequestId::from(4),
        result: Some(serde_json::json!([
            {
                "range": {"start":{"line":6,"character":6},"end":{"character":9,"line":6}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":9,"character":0},"end":{"character":3,"line":9}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":5,"character":16},"end":{"line":5, "character":19}},
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
        ])),
        error: None,
    }));

    // Change the definition file in memory.
    // However, find ref still reports the stale result based on the filesystem content.
    test_messages.push(Message::from(Notification {
        method: "textDocument/didChange".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string(),
                "languageId": "python",
                "version": 2
            },
            "contentChanges": [{
                "text": format!("\n\n{}", std::fs::read_to_string(root.path().join("bar.py")).unwrap())
            }],
        }),
    }));
    test_messages.push(Message::from(Request {
        id: RequestId::from(5),
        method: "textDocument/references".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
            "position": {
                "line": 5,
                "character": 17
            },
            "context": {
                "includeDeclaration": true
            },
        }),
    }));
    expected_responses.push(Message::Response(Response {
        id: RequestId::from(5),
        result: Some(serde_json::json!([
            {
                "range": {"start":{"line":6,"character":6},"end":{"character":9,"line":6}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":9,"character":0},"end":{"character":3,"line":9}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":5,"character":16},"end":{"line":5, "character":19}},
                "uri": Url::from_file_path(root.path().join("foo.py")).unwrap().to_string()
            },
        ])),
        error: None,
    }));

    // When we do a find-ref in an in-memory file with changed content,
    // it will cause us to fail to find references in other files.
    test_messages.push(Message::from(Request {
        id: RequestId::from(6),
        method: "textDocument/references".to_owned(),
        params: serde_json::json!({
            "textDocument": {
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            "position": {
                "line": 8,
                "character": 7
            },
            "context": {
                "includeDeclaration": true
            },
        }),
    }));

    expected_responses.push(Message::Response(Response {
        id: RequestId::from(6),
        result: Some(serde_json::json!([
            {
                "range": {"start":{"line":8,"character":6},"end":{"character":9,"line":8}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
            {
                "range": {"start":{"line":11,"character":0},"end":{"character":3,"line":11}},
                "uri": Url::from_file_path(root.path().join("bar.py")).unwrap().to_string()
            },
        ])),
        error: None,
    }));

    run_test_lsp(TestCase {
        messages_from_language_client: test_messages,
        expected_messages_from_language_server: expected_responses,
        search_path: vec![root.path().to_path_buf()],
        experimental_project_path: vec![root.path().to_path_buf()],
        workspace_folders: None,
        configuration: false,
    });
}

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
    let expected_messages_from_language_server = vec![
        Message::Request(Request {
            id: RequestId::from(1),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: serde_json::json!(ConfigurationParams {
                items: Vec::from([ConfigurationItem {
                    scope_uri: Some(scope_uri.clone()),
                    section: Some("python".to_owned()),
                }]),
            }),
        }),
        Message::Request(Request {
            id: RequestId::from(2),
            method: WorkspaceConfiguration::METHOD.to_owned(),
            params: serde_json::json!(ConfigurationParams {
                items: Vec::from([ConfigurationItem {
                    scope_uri: Some(scope_uri.clone()),
                    section: Some("python".to_owned()),
                }]),
            }),
        }),
    ];
    run_test_lsp(TestCase {
        messages_from_language_client,
        expected_messages_from_language_server,
        search_path: Vec::new(),
        experimental_project_path: Vec::new(),
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
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
            "line": 5,
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
        result: Some(serde_json::json!([{"pyrefly": {"disableLanguageServices": true}}])),
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
            scope_uri: Some(scope_uri.clone()),
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
        search_path: Vec::new(),
        experimental_project_path: Vec::new(),
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        configuration: true,
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
                "line": 5,
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
        search_path: vec![root.path().to_path_buf()],
        experimental_project_path: Vec::new(),
        workspace_folders: None,
        configuration: false,
    });
}
