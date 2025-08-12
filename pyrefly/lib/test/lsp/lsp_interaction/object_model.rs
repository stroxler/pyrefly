/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// This file contains a new implementation of the lsp_interaction test suite. Soon it will replace the old one.
use std::io;
use std::sync::Arc;
use std::thread::JoinHandle;
use std::thread::{self};
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
    /// Handle to the spawned server thread
    server_thread: Option<JoinHandle<Result<(), io::Error>>>,
}

impl TestServer {
    pub fn new(sender: crossbeam_channel::Sender<Message>) -> Self {
        Self {
            sender,
            timeout: Duration::from_secs(25),
            server_thread: None,
        }
    }

    pub fn expect_stop(&self) {
        let start = std::time::Instant::now();
        while let Some(thread) = &self.server_thread
            && !thread.is_finished()
        {
            if start.elapsed() > Duration::from_secs(10) {
                panic!("Server did not shutdown in time");
            }
            thread::sleep(Duration::from_millis(100));
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

    pub fn expect_any_message(&self) {
        match self.receiver.recv_timeout(self.timeout) {
            Ok(msg) => {
                eprintln!("client<---server {}", serde_json::to_string(&msg).unwrap());
            }
            Err(RecvTimeoutError::Timeout) => {
                panic!("Timeout waiting for response");
            }
            Err(RecvTimeoutError::Disconnected) => {
                panic!("Channel disconnected");
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

        let mut server = TestServer::new(language_server_sender);

        // Spawn the server thread and store its handle
        let thread_handle = thread::spawn(move || {
            run_lsp(connection, args)
                .map(|_| ())
                .map_err(|e| std::io::Error::other(e.to_string()))
        });

        server.server_thread = Some(thread_handle);

        Self {
            server,
            client: TestClient::new(language_client_receiver),
        }
    }

    pub fn initialize(&self) {
        self.server
            .send_initialize(self.server.get_initialize_params(None, false, false));
        self.client.expect_any_message();
        self.server.send_initialized();
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
