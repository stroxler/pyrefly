/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_server::Message;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types::Url;

use crate::commands::lsp::IndexingMode;
use crate::test::lsp::lsp_interaction::util::TestCase;
use crate::test::lsp::lsp_interaction::util::build_did_open_notification;
use crate::test::lsp::lsp_interaction::util::get_test_files_root;
use crate::test::lsp::lsp_interaction::util::run_test_lsp;

#[test]
fn test_workspace_symbol() {
    let root = get_test_files_root();
    let root_path = root.path().join("tests_requiring_config");
    let scope_uri = Url::from_file_path(root_path.clone()).unwrap();

    run_test_lsp(TestCase {
        messages_from_language_client: vec![
            Message::from(build_did_open_notification(root_path.join("foo.py"))),
            Message::from(Request {
                id: RequestId::from(2),
                method: "workspace/symbol".to_owned(),
                params: serde_json::json!({
                    "query": "this_is_a_very_long_function_name_so_we_can"
                }),
            }),
        ],
        expected_messages_from_language_server: vec![Message::Response(Response {
            id: RequestId::from(2),
            result: Some(serde_json::json!([
                {
                    "kind":12,
                    "location":{
                        "range":{"start":{"line":6,"character":4},"end":{"line":6,"character":99}},
                        "uri":Url::from_file_path(root_path.join("autoimport_provider.py")).unwrap().to_string()
                    },
                    "name":"this_is_a_very_long_function_name_so_we_can_deterministically_test_autoimport_with_fuzzy_search"
                }
            ])),
            error: None,
        })],
        indexing_mode: IndexingMode::LazyBlocking,
        workspace_folders: Some(vec![("test".to_owned(), scope_uri)]),
        ..Default::default()
    });
}
