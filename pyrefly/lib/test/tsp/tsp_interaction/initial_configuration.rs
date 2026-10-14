/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Integration tests for requests that arrive before the initial `workspace/configuration` response.

use lsp_types::Uri;
use serde_json::json;
use tempfile::TempDir;

use crate::lsp::non_wasm::protocol::Message;
use crate::lsp::non_wasm::protocol::Response;
use crate::test::python_env::TestVenv;
use crate::test::tsp::tsp_interaction::object_model::InitializeSettings;
use crate::test::tsp::tsp_interaction::object_model::TspInteraction;

// bug = "A TSP request sent before the initial settings response ignores its interpreter."
#[cfg(unix)]
#[test]
fn test_initial_configuration_interpreter_ignored_by_earlier_tsp_requests() {
    let temp_dir = TempDir::new().unwrap();
    let workspace = temp_dir.path().join("workspace");
    std::fs::create_dir_all(&workspace).unwrap();
    std::fs::write(workspace.join("main.py"), "x = 1\n").unwrap();
    let venv = TestVenv::new_synthetic_excluded_from_discovery(temp_dir.path().join("interpreter"));
    let interpreter = venv.create_mock_interpreter();

    let mut tsp = TspInteraction::new();
    tsp.set_root(workspace.clone());
    let settings = InitializeSettings {
        configuration: Some(None),
        ..Default::default()
    };
    tsp.server
        .send_initialize(tsp.server.get_initialize_params(&settings));
    tsp.client.expect_any_message();
    tsp.server.send_initialized();
    let Message::Request(configuration) = tsp.client.receive_any_message() else {
        panic!("Expected the initial workspace/configuration request");
    };

    let main_uri = Uri::from_file_path(workspace.join("main.py")).unwrap();
    tsp.server.get_python_search_paths(main_uri.as_str(), 0);
    tsp.server.send_message(Message::Response(Response {
        id: configuration.id,
        result: Some(json!([{"pythonPath": interpreter}])),
        error: None,
    }));

    let response = tsp.client.receive_response_skip_notifications();
    assert!(
        response.error.is_none(),
        "Expected success, got error: {:?}",
        response.error
    );
    let paths: Vec<String> = serde_json::from_value(response.result.unwrap()).unwrap();
    let site_packages = Uri::from_file_path(venv.site_packages()).unwrap();
    assert!(
        !paths.contains(&site_packages.to_string()),
        "The request should answer with the default interpreter, got {paths:?}"
    );

    tsp.shutdown();
}
