/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use crate::report::pysa::ModuleContext;
use crate::report::pysa::ModuleIds;
use crate::report::pysa::call_graph::build_call_graphs_for_module;
use crate::state::state::State;
use crate::test::util::TestEnv;

fn create_state(module_name: &str, python_code: &str) -> State {
    let mut test_env = TestEnv::new();
    test_env.add(module_name, python_code);
    let (state, _) = test_env.to_state();
    state
}

fn test_building_call_graph_for_module(test_module_name: &str, code: &str) {
    let state = create_state(test_module_name, code);
    let transaction = state.transaction();

    let handles = transaction.handles();
    let module_ids = ModuleIds::new(&handles);
    // TODO: Collect all function definitions
    let function_definitions = HashMap::new();

    handles
        .iter()
        .find(|handle| {
            let module_info = &transaction.get_module_info(handle).unwrap();
            let module_name = module_info.name().to_string();
            test_module_name == module_name
        })
        .map(|test_module_handle| {
            let context = ModuleContext::create(test_module_handle, &transaction, &module_ids);
            build_call_graphs_for_module(&context, true, &function_definitions)
        });
}

#[test]
fn test_building_call_graphs() {
    let module_name = "test";
    test_building_call_graph_for_module(
        module_name,
        r#"
def helper():
    pass

def main():
    helper()
"#,
    );
}
