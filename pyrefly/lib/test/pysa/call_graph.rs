/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use pyrefly_util::lined_buffer::DisplayRange;
use pyrefly_util::prelude::SliceExt;

use crate::report::pysa::DefinitionRef;
use crate::report::pysa::ModuleContext;
use crate::report::pysa::ModuleIds;
use crate::report::pysa::call_graph::CallCallees;
use crate::report::pysa::call_graph::CallGraphs;
use crate::report::pysa::call_graph::build_call_graphs_for_module;
use crate::report::pysa::collect_function_names;
use crate::report::pysa::location_key;
use crate::state::state::State;
use crate::test::util::TestEnv;

// Omit fields from `DefinitionRef` so that we can easily write the expected results
#[derive(Debug, Hash, Eq, PartialEq)]
struct DefinitionRefForTest {
    module_name: String,
    identifier: String,
}

impl DefinitionRefForTest {
    fn from_definition_ref(definition_ref: &DefinitionRef) -> Self {
        Self {
            module_name: definition_ref.module_name.clone(),
            identifier: definition_ref.identifier.clone(),
        }
    }

    fn from_string(string: &str) -> Self {
        let parts: Vec<&str> = string.split('.').collect();
        if let Some((last, rest)) = parts.split_last() {
            Self {
                module_name: rest.join("."),
                identifier: (*last).to_owned(),
            }
        } else {
            panic!("Invalid string: {}", string);
        }
    }
}

impl std::fmt::Display for DefinitionRefForTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.module_name, self.identifier)
    }
}

fn call_graph_for_test_from_actual(
    call_graph: &CallGraphs<DefinitionRef, DisplayRange>,
) -> CallGraphs<DefinitionRefForTest, String> {
    let call_graph_for_test = call_graph
        .0
        .iter()
        .map(|(caller, callees)| {
            let caller = DefinitionRefForTest::from_definition_ref(caller);
            let callees_for_test = callees
                .iter()
                .map(|(location, call_callees)| {
                    let call_callees_for_test = call_callees
                        .call_targets
                        .map(DefinitionRefForTest::from_definition_ref);
                    (
                        location_key(location),
                        CallCallees {
                            call_targets: call_callees_for_test,
                        },
                    )
                })
                .collect::<HashMap<_, _>>();
            (caller, callees_for_test)
        })
        .collect::<HashMap<_, _>>();
    CallGraphs::new_with_map(call_graph_for_test)
}

fn call_graph_for_test_from_expected(
    call_graph: &Vec<(&str, Vec<(&str, Vec<&str>)>)>,
) -> CallGraphs<DefinitionRefForTest, String> {
    let call_graph_for_test = call_graph
        .iter()
        .map(|(caller, callees)| {
            let callees_for_test = callees
                .iter()
                .map(|(location, call_callees)| {
                    let call_callees_for_test =
                        call_callees.map(|s| DefinitionRefForTest::from_string(s));
                    (
                        (*location).to_owned(),
                        CallCallees {
                            call_targets: call_callees_for_test,
                        },
                    )
                })
                .collect::<HashMap<_, _>>();
            (DefinitionRefForTest::from_string(caller), callees_for_test)
        })
        .collect::<HashMap<_, _>>();
    CallGraphs::new_with_map(call_graph_for_test)
}

fn create_state(module_name: &str, python_code: &str) -> State {
    let mut test_env = TestEnv::new();
    test_env.add(module_name, python_code);
    let (state, _) = test_env.to_state();
    state
}

fn test_building_call_graph_for_module(
    test_module_name: &str,
    code: &str,
    expected: &Vec<(&str, Vec<(&str, Vec<&str>)>)>,
) {
    let state = create_state(test_module_name, code);
    let transaction = state.transaction();

    let handles = transaction.handles();
    let module_ids = ModuleIds::new(&handles);
    let all_function_names = collect_function_names(&handles, &transaction, &module_ids);

    let test_module_handle = handles
        .iter()
        .find(|handle| {
            let module_info = &transaction.get_module_info(handle).unwrap();
            let module_name = module_info.name().to_string();
            test_module_name == module_name
        })
        .unwrap();
    let context = ModuleContext::create(test_module_handle, &transaction, &module_ids).unwrap();
    let actual_call_graph = build_call_graphs_for_module(&context, true, &all_function_names);
    let expected_call_graph = call_graph_for_test_from_expected(expected);
    assert_eq!(
        expected_call_graph,
        call_graph_for_test_from_actual(&actual_call_graph),
    );
}

#[test]
fn test_building_call_graphs() {
    let module_name = "test";

    test_building_call_graph_for_module(
        module_name,
        r#"
def foo():
  bar()
def bar():
  pass
"#,
        &vec![("test.foo", vec![("3:3-3:8", vec!["test.bar"])])],
    );

    test_building_call_graph_for_module(
        module_name,
        r#"
def foo(c: C):
  c.m()

class C:
  def m(self):
    pass
"#,
        &vec![("test.foo", vec![("3:3-3:8", vec!["test.m"])])],
    );
}
