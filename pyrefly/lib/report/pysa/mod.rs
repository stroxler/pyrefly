/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod ast_visitor;
pub mod call_graph;
pub mod captured_variable;
pub mod class;
pub mod context;
pub mod function;
pub mod global_variable;
pub mod is_test_module;
pub mod location;
pub mod module;
pub mod override_graph;
pub mod scope;
pub mod type_of_expression;
pub mod types;

use core::panic;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufWriter;
use std::ops::Not;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Instant;

use itertools::Itertools;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePathDetails;
use pyrefly_util::fs_anyhow;
use pyrefly_util::thread_pool::ThreadPool;
use rayon::prelude::*;
use ruff_python_ast::name::Name;
use serde::Serialize;
use tracing::info;

use crate::module::typeshed::typeshed;
use crate::report::pysa::call_graph::CallGraph;
use crate::report::pysa::call_graph::export_call_graphs;
use crate::report::pysa::captured_variable::export_captured_variables;
use crate::report::pysa::class::ClassDefinition;
use crate::report::pysa::class::ClassId;
use crate::report::pysa::class::export_all_classes;
use crate::report::pysa::context::ModuleContext;
use crate::report::pysa::function::FunctionBaseDefinition;
use crate::report::pysa::function::FunctionDefinition;
use crate::report::pysa::function::FunctionId;
use crate::report::pysa::function::FunctionRef;
use crate::report::pysa::function::ModuleFunctionDefinitions;
use crate::report::pysa::function::WholeProgramFunctionDefinitions;
use crate::report::pysa::function::collect_function_base_definitions;
use crate::report::pysa::function::export_function_definitions;
use crate::report::pysa::global_variable::GlobalVariable;
use crate::report::pysa::global_variable::export_global_variables;
use crate::report::pysa::location::PysaLocation;
use crate::report::pysa::module::ModuleId;
use crate::report::pysa::module::ModuleIds;
use crate::report::pysa::module::ModuleKey;
use crate::report::pysa::override_graph::OverrideGraph;
use crate::report::pysa::override_graph::build_reversed_override_graph;
use crate::report::pysa::type_of_expression::export_type_of_expressions;
use crate::report::pysa::types::PysaType;
use crate::state::state::Transaction;

#[derive(Debug, Clone, Serialize)]
struct PysaProjectModule {
    module_id: ModuleId,
    module_name: ModuleName,        // e.g, `foo.bar`
    source_path: ModulePathDetails, // Path to the source code
    info_filename: Option<PathBuf>, // Filename for info files
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_test: bool, // Uses a set of heuristics to determine if the module is a test file.
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_interface: bool, // Is this a .pyi file?
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_init: bool, // Is this a __init__.py(i) file?
}

/// Format of the index file `pyrefly.pysa.json`
#[derive(Debug, Clone, Serialize)]
struct PysaProjectFile {
    format_version: u32,
    modules: HashMap<ModuleId, PysaProjectModule>,
    builtin_module_id: ModuleId,
    object_class_id: ClassId,
}

/// Format of the file `definitions/my.module:id.json` containing all definitions
#[derive(Debug, Clone, Serialize)]
pub struct PysaModuleDefinitions {
    format_version: u32,
    module_id: ModuleId,
    module_name: ModuleName,
    source_path: ModulePathDetails,
    function_definitions: ModuleFunctionDefinitions<FunctionDefinition>,
    class_definitions: HashMap<PysaLocation, ClassDefinition>,
    global_variables: HashMap<Name, GlobalVariable>,
}

/// Format of the file `type_of_expressions/my.module:id.json` containing type of expressions
#[derive(Debug, Clone, Serialize)]
pub struct PysaModuleTypeOfExpressions {
    format_version: u32,
    module_id: ModuleId,
    module_name: ModuleName,
    source_path: ModulePathDetails,
    type_of_expression: HashMap<PysaLocation, PysaType>,
}

/// Format of the file `call_graphs/my.module:id.json` containing module call graphs
#[derive(Debug, Clone, Serialize)]
pub struct PysaModuleCallGraphs {
    format_version: u32,
    module_id: ModuleId,
    module_name: ModuleName,
    source_path: ModulePathDetails,
    call_graphs: HashMap<FunctionId, CallGraph<FunctionRef>>,
}

pub fn export_module_definitions(
    context: &ModuleContext,
    function_base_definitions: &WholeProgramFunctionDefinitions<FunctionBaseDefinition>,
) -> PysaModuleDefinitions {
    let global_variables = export_global_variables(context);
    let captured_variables = export_captured_variables(context);
    let class_definitions = export_all_classes(function_base_definitions, context);

    let function_definitions =
        export_function_definitions(function_base_definitions, &captured_variables, context);
    PysaModuleDefinitions {
        format_version: 1,
        module_id: context.module_id,
        module_name: context.module_info.name(),
        source_path: context.module_info.path().details().clone(),
        function_definitions,
        class_definitions,
        global_variables,
    }
}

pub fn export_module_type_of_expressions(context: &ModuleContext) -> PysaModuleTypeOfExpressions {
    let type_of_expression = export_type_of_expressions(context);
    PysaModuleTypeOfExpressions {
        format_version: 1,
        module_id: context.module_id,
        module_name: context.module_info.name(),
        source_path: context.module_info.path().details().clone(),
        type_of_expression,
    }
}

pub fn export_module_call_graphs(
    context: &ModuleContext,
    function_base_definitions: &WholeProgramFunctionDefinitions<FunctionBaseDefinition>,
) -> PysaModuleCallGraphs {
    let call_graphs = export_call_graphs(context, function_base_definitions)
        .into_iter()
        .map(|(function_ref, call_graph)| (function_ref.function_id, call_graph))
        .collect();
    PysaModuleCallGraphs {
        format_version: 1,
        module_id: context.module_id,
        module_name: context.module_info.name(),
        source_path: context.module_info.path().details().clone(),
        call_graphs,
    }
}

pub fn write_results(results_directory: &Path, transaction: &Transaction) -> anyhow::Result<()> {
    let start = Instant::now();
    info!("Writing results to `{}`", results_directory.display());
    fs_anyhow::create_dir_all(results_directory)?;
    let definitions_directory = results_directory.join("definitions");
    let type_of_expressions_directory = results_directory.join("type_of_expressions");
    let call_graphs_directory = results_directory.join("call_graphs");
    fs_anyhow::create_dir_all(&definitions_directory)?;
    fs_anyhow::create_dir_all(&type_of_expressions_directory)?;
    fs_anyhow::create_dir_all(&call_graphs_directory)?;

    let handles = transaction.handles();
    let module_ids = ModuleIds::new(&handles);
    let mut project_modules = HashMap::new();

    let mut module_info_tasks = Vec::new();
    for handle in &handles {
        let module_id = module_ids.get(ModuleKey::from_handle(handle)).unwrap();

        // Path where we will store the information on the module.
        let info_filename = match handle.path().details() {
            ModulePathDetails::Namespace(_) => {
                // Indicates a directory that contains a `__init__.py` file.
                None
            }
            _ => {
                Some(PathBuf::from(format!(
                    "{}:{}.json",
                    // Filename must be less than 255 bytes
                    String::from_iter(
                        handle
                            .module()
                            .to_string()
                            .chars()
                            .filter(|c| c.is_ascii())
                            .take(220)
                    ),
                    module_id.to_int()
                )))
            }
        };

        assert!(
            project_modules
                .insert(
                    module_id,
                    PysaProjectModule {
                        module_id,
                        module_name: handle.module(),
                        source_path: handle.path().details().clone(),
                        info_filename: info_filename.clone(),
                        is_test: false,
                        is_interface: handle.path().is_interface(),
                        is_init: handle.path().is_init(),
                    }
                )
                .is_none(),
            "Found multiple handles with the same module id"
        );

        if let Some(info_filename) = info_filename {
            module_info_tasks.push((handle, module_id, info_filename));
        }
    }

    let project_modules = Arc::new(Mutex::new(project_modules));

    let reversed_override_graph = build_reversed_override_graph(&handles, transaction, &module_ids);
    let function_base_definitions = collect_function_base_definitions(
        &handles,
        transaction,
        &module_ids,
        &reversed_override_graph,
    );

    let _override_graph = OverrideGraph::from_reversed(&reversed_override_graph);

    // Retrieve and dump information about each module, in parallel.
    ThreadPool::new().install(|| -> anyhow::Result<()> {
        module_info_tasks.into_par_iter().try_for_each(
            |(handle, module_id, info_filename)| -> anyhow::Result<()> {
                let context = ModuleContext::create(handle, transaction, &module_ids).unwrap();

                {
                    let writer =
                        BufWriter::new(File::create(definitions_directory.join(&info_filename))?);
                    serde_json::to_writer(
                        writer,
                        &export_module_definitions(&context, &function_base_definitions),
                    )?;
                }

                {
                    let writer = BufWriter::new(File::create(
                        type_of_expressions_directory.join(&info_filename),
                    )?);
                    serde_json::to_writer(writer, &export_module_type_of_expressions(&context))?;
                }

                {
                    let writer =
                        BufWriter::new(File::create(call_graphs_directory.join(&info_filename))?);
                    serde_json::to_writer(
                        writer,
                        &export_module_call_graphs(&context, &function_base_definitions),
                    )?;
                }

                if is_test_module::is_test_module(&context) {
                    project_modules
                        .lock()
                        .unwrap()
                        .get_mut(&module_id)
                        .unwrap()
                        .is_test = true;
                }

                Ok(())
            },
        )
    })?;

    // Dump all typeshed files, so we can parse them.
    let typeshed = typeshed()?;
    for typeshed_module in typeshed.modules() {
        let module_path = typeshed.find(typeshed_module).unwrap();
        let relative_path = match module_path.details() {
            ModulePathDetails::BundledTypeshed(path) => path,
            _ => panic!("unexpected module path for typeshed module"),
        };
        let content = typeshed.load(relative_path).unwrap();
        let target_path = results_directory.join("typeshed").join(relative_path);
        fs_anyhow::create_dir_all(target_path.parent().unwrap())?;
        fs_anyhow::write(&target_path, content.as_bytes())?;
    }

    let builtin_module = handles
        .iter()
        .filter(|handle| handle.module().as_str() == "builtins")
        .exactly_one()
        .expect("expected exactly one builtins module");
    let object_class_id = ClassId::from_class(
        transaction
            .get_stdlib(builtin_module)
            .object()
            .class_object(),
    );

    let project_modules = Arc::into_inner(project_modules)
        .unwrap()
        .into_inner()
        .unwrap();

    let writer = BufWriter::new(File::create(results_directory.join("pyrefly.pysa.json"))?);
    serde_json::to_writer(
        writer,
        &PysaProjectFile {
            format_version: 1,
            modules: project_modules,
            builtin_module_id: module_ids
                .get(ModuleKey::from_handle(builtin_module))
                .unwrap(),
            object_class_id,
        },
    )?;

    let elapsed = start.elapsed();
    info!(
        "Wrote results to `{}` in {:.2}s",
        results_directory.display(),
        elapsed.as_secs_f32()
    );

    Ok(())
}
