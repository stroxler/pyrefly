/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// This file contains a new implementation of the lsp_interaction test suite. Soon it will replace the old one.
use std::sync::Arc;
use std::thread::spawn;
use std::time::Duration;

use crossbeam_channel::RecvTimeoutError;
use crossbeam_channel::bounded;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types::Url;
use lsp_types::notification::Exit;
use lsp_types::notification::Notification as _;
use lsp_types::request::Request as _;
use pretty_assertions::assert_eq;
use serde_json::Value;

use crate::commands::lsp::IndexingMode;
use crate::commands::lsp::LspArgs;
use crate::commands::lsp::run_lsp;
use crate::test::util::init_test;

pub struct TestServer {
    sender: crossbeam_channel::Sender<Message>,
    timeout: Duration,
}

impl TestServer {
    pub fn new(sender: crossbeam_channel::Sender<Message>) -> Self {
        Self {
            sender,
            timeout: Duration::from_secs(25),
        }
    }

    /// Send a message to this server
    pub fn send_message(&self, message: Message) {
        eprintln!(
            "client--->server {}",
            serde_json::to_string(&message).unwrap()
        );
        if let Err(err) = self.sender.send_timeout(message.clone(), self.timeout) {
            panic!("Failed to send message to language server: {err:?}");
        }
    }
    pub fn send_initialize(&self, params: Value) {
        self.send_message(Message::Request(Request {
            id: RequestId::from(1),
            method: "initialize".to_owned(),
            params,
        }))
    }

    pub fn send_initialized(&self) {
        self.send_message(Message::Notification(Notification {
            method: "initialized".to_owned(),
            params: serde_json::json!({}),
        }));
    }

    pub fn send_shutdown(&self, id: RequestId) {
        self.send_message(Message::Request(Request {
            id,
            method: lsp_types::request::Shutdown::METHOD.to_owned(),
            params: serde_json::json!(null),
        }));
    }

    pub fn send_exit(&self) {
        self.send_message(Message::Notification(Notification {
            method: Exit::METHOD.to_owned(),
            params: serde_json::json!(null),
        }));
    }

    pub fn get_initialize_params(
        &self,
        workspace_folders: Option<Vec<(String, Url)>>,
        configuration: bool,
        file_watch: bool,
    ) -> Value {
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
        if file_watch {
            params["capabilities"]["workspace"]["didChangeWatchedFiles"] =
                serde_json::json!({"dynamicRegistration": true});
        }
        if configuration {
            params["capabilities"]["workspace"]["configuration"] = serde_json::json!(true);
        }

        params
    }
}

pub struct TestClient {
    receiver: crossbeam_channel::Receiver<Message>,
    timeout: Duration,
}

impl TestClient {
    pub fn new(receiver: crossbeam_channel::Receiver<Message>) -> Self {
        Self {
            receiver,
            timeout: Duration::from_secs(25),
        }
    }

    pub fn expect_message(&self, expected_message: Message) {
        match self.receiver.recv_timeout(self.timeout) {
            Ok(msg) => {
                eprintln!("client<---server {}", serde_json::to_string(&msg).unwrap());

                let assert_match = |expected: &str, actual: &str| {
                    assert_eq!(actual, expected, "Response mismatch");
                };

                let expected_str = serde_json::to_string(&expected_message).unwrap();
                let actual_str = serde_json::to_string(&msg).unwrap();

                assert_match(&expected_str, &actual_str);
            }
            Err(RecvTimeoutError::Timeout) => {
                panic!("Timeout waiting for response. Expected: {expected_message:?}");
            }
            Err(RecvTimeoutError::Disconnected) => {
                panic!("Channel disconnected. Expected: {expected_message:?}");
            }
        }
    }
}

pub struct LspInteraction {
    pub server: TestServer,
    pub client: TestClient,
}

impl LspInteraction {
    pub fn new() -> Self {
        init_test();

        let (language_client_sender, language_client_receiver) = bounded::<Message>(0);
        let (language_server_sender, language_server_receiver) = bounded::<Message>(0);

        let args = LspArgs {
            indexing_mode: IndexingMode::None,
        };
        let connection = Connection {
            sender: language_client_sender,
            receiver: language_server_receiver,
        };

        let connection = Arc::new(connection);
        let args = args.clone();

        spawn(move || {
            run_lsp(connection, args)
                .map(|_| ())
                .map_err(|e| std::io::Error::other(e.to_string()))
        });
        Self {
            server: TestServer::new(language_server_sender),
            client: TestClient::new(language_client_receiver),
        }
    }

    pub fn shutdown(&self) {
        let shutdown_id = RequestId::from(999);
        self.server.send_shutdown(shutdown_id.clone());

        self.client.expect_message(Message::Response(Response {
            id: shutdown_id,
            result: Some(serde_json::json!(null)),
            error: None,
        }));

        self.server.send_exit();
    }
}
