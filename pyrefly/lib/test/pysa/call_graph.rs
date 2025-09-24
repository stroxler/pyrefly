/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use pyrefly_util::lined_buffer::DisplayRange;

use crate::report::pysa::DefinitionRef;
use crate::report::pysa::ModuleContext;
use crate::report::pysa::ModuleIds;
use crate::report::pysa::build_reversed_override_graph;
use crate::report::pysa::call_graph::AttributeAccessCallees;
use crate::report::pysa::call_graph::CallCallees;
use crate::report::pysa::call_graph::CallGraphs;
use crate::report::pysa::call_graph::CallTarget;
use crate::report::pysa::call_graph::ExpressionCallees;
use crate::report::pysa::call_graph::IdentifierCallees;
use crate::report::pysa::call_graph::build_call_graphs_for_module;
use crate::report::pysa::collect_function_definitions;
use crate::report::pysa::location_key;
use crate::test::pysa::utils::create_state;
use crate::test::pysa::utils::get_handle_for_module_name;

// Omit fields from `DefinitionRef` so that we can easily write the expected results
#[derive(Debug, Hash, Eq, PartialEq, Clone)]
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

fn create_call_target(target: &str) -> CallTarget<DefinitionRefForTest> {
    CallTarget {
        target: DefinitionRefForTest::from_string(target),
        implicit_receiver: false,
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
                .map(|(location, expression_callees)| {
                    (
                        location_key(location),
                        expression_callees.map_target(DefinitionRefForTest::from_definition_ref),
                    )
                })
                .collect::<HashMap<_, _>>();
            (caller, callees_for_test)
        })
        .collect::<HashMap<_, _>>();
    CallGraphs::from_map(call_graph_for_test)
}

fn call_graph_for_test_from_expected(
    call_graph: Vec<(&str, Vec<(&str, ExpressionCallees<DefinitionRefForTest>)>)>,
) -> CallGraphs<DefinitionRefForTest, String> {
    let call_graph_for_test = call_graph
        .into_iter()
        .map(|(caller, callees)| {
            let callees_for_test = callees
                .into_iter()
                .map(|(location, expression_callees_for_test)| {
                    ((*location).to_owned(), expression_callees_for_test)
                })
                .collect::<HashMap<_, _>>();
            (DefinitionRefForTest::from_string(caller), callees_for_test)
        })
        .collect::<HashMap<_, _>>();
    CallGraphs::from_map(call_graph_for_test)
}

fn test_building_call_graph_for_module(
    test_module_name: &str,
    code: &str,
    expected: Vec<(&str, Vec<(&str, ExpressionCallees<DefinitionRefForTest>)>)>,
) {
    let state = create_state(test_module_name, code);
    let transaction = state.transaction();

    let handles = transaction.handles();
    let module_ids = ModuleIds::new(&handles);

    let reversed_override_graph =
        build_reversed_override_graph(&handles, &transaction, &module_ids);
    let all_function_definitions = collect_function_definitions(
        &handles,
        &transaction,
        &module_ids,
        &reversed_override_graph,
    );

    let test_module_handle = get_handle_for_module_name(test_module_name, &transaction);
    let context = ModuleContext::create(&test_module_handle, &transaction, &module_ids).unwrap();
    let actual_call_graph = call_graph_for_test_from_actual(&build_call_graphs_for_module(
        &context,
        &all_function_definitions,
    ));
    let expected_call_graph = call_graph_for_test_from_expected(expected);
    assert_eq!(
        expected_call_graph, actual_call_graph,
        "Expected: {:#?}. Actual: {:#?}",
        expected_call_graph, actual_call_graph,
    );
}

fn call_callees_from_expected(
    expected: Vec<CallTarget<DefinitionRefForTest>>,
) -> ExpressionCallees<DefinitionRefForTest> {
    ExpressionCallees::Call(CallCallees {
        call_targets: expected.to_vec(),
    })
}

fn attribute_access_callees_from_expected(
    expected: Vec<CallTarget<DefinitionRefForTest>>,
) -> ExpressionCallees<DefinitionRefForTest> {
    ExpressionCallees::AttributeAccess(AttributeAccessCallees {
        callable_targets: expected.to_vec(),
    })
}

fn identifier_callees_from_expected(
    expected: Vec<CallTarget<DefinitionRefForTest>>,
) -> ExpressionCallees<DefinitionRefForTest> {
    ExpressionCallees::Identifier(IdentifierCallees {
        callable_targets: expected.to_vec(),
    })
}

static TEST_MODULE_NAME: &str = "test";
static TEST_DEFINITION_NAME: &str = "test.foo";

#[macro_export]
macro_rules! call_graph_testcase {
    ($name:ident, $module_name:ident, $code:literal, $expected:expr,) => {
        #[test]
        fn $name() {
            $crate::test::pysa::call_graph::test_building_call_graph_for_module(
                $module_name,
                $code,
                $expected,
            );
        }
    };
}

call_graph_testcase! {
    test_simple_function_call,
    TEST_MODULE_NAME,
    r#"
def foo():
  bar()
def bar():
  pass
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![
            ("3:3-3:6", identifier_callees_from_expected(vec![create_call_target("test.bar")])),
            ("3:3-3:8", call_callees_from_expected(vec![create_call_target("test.bar")])),
        ],
    )],
}

call_graph_testcase! {
    test_method_call_on_class,
    TEST_MODULE_NAME,
    r#"
class C:
  def m(self):
    pass
def foo(c: C):
  c.m()
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![
            (
                "6:3-6:6",
                attribute_access_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)]),
            ),
            ("6:3-6:8", call_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)])),
        ],
    )],
}

call_graph_testcase! {
    test_conditional_function_assignment,
    TEST_MODULE_NAME,
    r#"
def baz() -> int: ...
def bar() -> bool: ...
def foo(b: bool):
  if b:
    f = bar
  else:
    f = baz
  f()
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![
            ("6:9-6:12", identifier_callees_from_expected(vec![create_call_target("test.bar")])),
            ("8:9-8:12", identifier_callees_from_expected(vec![create_call_target("test.baz")])),
        ],
    )],
}

call_graph_testcase! {
    test_conditional_function_with_none,
    TEST_MODULE_NAME,
    r#"
def bar() -> bool: ...
def foo(b: bool):
  if b:
    f = bar
  else:
    f = False
  f()
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![("5:9-5:12", identifier_callees_from_expected(vec![create_call_target("test.bar")]))],
    )],
}

call_graph_testcase! {
    test_method_call_on_optional_class,
    TEST_MODULE_NAME,
    r#"
from typing import Optional
class C:
  def m():
    ...
def foo(c: Optional[C]):
  c.m()
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![
            ("7:3-7:8", call_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)])),
            (
                "7:3-7:6",
                attribute_access_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)]),
            )
        ],
    )],
}

call_graph_testcase! {
    test_method_call_on_class_with_inheritance,
    TEST_MODULE_NAME,
    r#"
class C:
  def m():
    ...
class D(C):
  def m():
    ...
class E(D):
  def m():
    ...
def foo(c: C):
  c.m()
"#,
    vec![(
        TEST_DEFINITION_NAME,
        vec![
            ("12:3-12:8", call_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)])),
            (
                "12:3-12:6",
                attribute_access_callees_from_expected(vec![create_call_target("test.m").with_implicit_receiver(true)]),
            )
        ],
    )],
}
