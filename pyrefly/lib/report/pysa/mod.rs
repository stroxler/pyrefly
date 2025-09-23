/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod call_graph;
mod override_graph;

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

use dashmap::DashMap;
use itertools::Itertools;
use pyrefly_build::handle::Handle;
use pyrefly_python::ast::Ast;
use pyrefly_python::module::Module;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use pyrefly_python::module_path::ModulePathDetails;
use pyrefly_python::short_identifier::ShortIdentifier;
use pyrefly_types::callable::Callable;
use pyrefly_types::callable::Param;
use pyrefly_types::callable::Params;
use pyrefly_types::class::Class;
use pyrefly_types::types::Overload;
use pyrefly_types::types::Type;
use pyrefly_util::fs_anyhow;
use pyrefly_util::lined_buffer::DisplayRange;
use pyrefly_util::thread_pool::ThreadPool;
use pyrefly_util::visit::Visit;
use rayon::prelude::*;
use ruff_python_ast::AnyNodeRef;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprName;
use ruff_python_ast::ModModule;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtImportFrom;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use serde::Serialize;
use starlark_map::Hashed;
use tracing::info;

use crate::alt::answers::Answers;
use crate::alt::class::class_field::ClassField;
use crate::alt::types::class_metadata::ClassMro;
use crate::alt::types::decorated_function::DecoratedFunction;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::ClassFieldDefinition;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassMro;
use crate::binding::binding::KeyDecoratedFunction;
use crate::binding::bindings::Bindings;
use crate::module::typeshed::typeshed;
use crate::report::pysa::override_graph::OverrideGraph;
use crate::report::pysa::override_graph::create_reversed_override_graph_for_module;
use crate::state::lsp::FindDefinitionItemWithDocstring;
use crate::state::state::Transaction;
use crate::types::display::TypeDisplayContext;
use crate::types::stdlib::Stdlib;

/// Represents a unique identifier for a module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ModuleId(u32);

/// Represents a unique identifier for a class, inside a module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ClassId(u32);

impl ClassId {
    fn from_class(class: &Class) -> ClassId {
        ClassId(class.index().0)
    }
}

/// Represents a unique identifier for a function, inside a module
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FunctionId {
    Function {
        location: DisplayRange,
    },
    ModuleTopLevel,
    #[expect(dead_code)]
    ClassTopLevel {
        class_id: ClassId,
    },
}

impl FunctionId {
    fn serialize_to_string(&self) -> String {
        match self {
            FunctionId::Function { location } => format!("F:{}", location_key(location)),
            FunctionId::ModuleTopLevel => "MTL".to_owned(),
            FunctionId::ClassTopLevel { class_id } => format!("CTL:{}", class_id.0),
        }
    }
}

impl Serialize for FunctionId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.serialize_to_string())
    }
}

#[derive(Debug, Clone, Serialize)]
struct PysaProjectModule {
    module_id: ModuleId,
    module_name: String,            // e.g, `foo.bar`
    source_path: ModulePathDetails, // Path to the source code
    info_path: Option<PathBuf>,     // Path to the PysaModuleFile
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

#[derive(Debug, Clone, Serialize)]
enum ScopeParent {
    Function { location: String },
    Class { location: String },
    TopLevel,
}

// List of class names that a type refers to, after stripping Optional and Awaitable.
#[derive(Debug, Clone, Serialize)]
struct ClassNamesFromType {
    class_names: Vec<ClassRef>,
    #[serde(skip_serializing_if = "<&bool>::not")]
    stripped_coroutine: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    stripped_optional: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    stripped_readonly: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    unbound_type_variable: bool,
    // Is there an element (after stripping) that isn't a class name?
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_exhaustive: bool,
}

/// Information needed from Pysa about a type.
#[derive(Debug, Clone, Serialize)]
struct PysaType {
    // Pretty string representation of the type. Usually meant for the user.
    string: String,

    // Whether the type is a bool/int/float/enum, after stripping Optional and Awaitable.
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_bool: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_int: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_float: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_enum: bool,

    #[serde(skip_serializing_if = "ClassNamesFromType::skip_serializing")]
    class_names: ClassNamesFromType,
}

#[derive(Debug, Clone, Serialize)]
enum FunctionParameter {
    PosOnly {
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        annotation: PysaType,
        #[serde(skip_serializing_if = "<&bool>::not")]
        required: bool,
    },
    Pos {
        name: String,
        annotation: PysaType,
        #[serde(skip_serializing_if = "<&bool>::not")]
        required: bool,
    },
    VarArg {
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        annotation: PysaType,
    },
    KwOnly {
        name: String,
        annotation: PysaType,
        #[serde(skip_serializing_if = "<&bool>::not")]
        required: bool,
    },
    Kwargs {
        #[serde(skip_serializing_if = "Option::is_none")]
        name: Option<String>,
        annotation: PysaType,
    },
}

#[derive(Debug, Clone, Serialize)]
enum FunctionParameters {
    List(Vec<FunctionParameter>),
    Ellipsis,
    ParamSpec,
}

#[derive(Debug, Clone, Serialize)]
struct FunctionSignature {
    parameters: FunctionParameters,
    return_annotation: PysaType,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassRef {
    module_id: ModuleId,
    module_name: String, // For debugging purposes only. Reader should use the module id.
    class_id: ClassId,
    class_name: String, // For debugging purposes only. Reader should use the class id.
}

impl ClassRef {
    fn from_class(class: &Class, module_ids: &ModuleIds) -> ClassRef {
        ClassRef {
            module_id: module_ids
                .get(ModuleKey::from_module(class.module()))
                .unwrap(),
            module_name: class.module_name().to_string(),
            class_id: ClassId::from_class(class),
            class_name: class.qname().id().to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionDefinition {
    name: String,
    parent: ScopeParent,
    undecorated_signatures: Vec<FunctionSignature>,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_overload: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_staticmethod: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_classmethod: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_property_getter: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_property_setter: bool,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_stub: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// If this is a method, record the class it is defined in.
    defining_class: Option<ClassRef>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// If the method directly overrides a method in a parent class, we record that class.
    /// This is used for building overriding graphs.
    overridden_base_method: Option<DefinitionRef>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq, Hash)]
pub struct DefinitionRef {
    module_id: ModuleId,
    pub(crate) module_name: String, // For debugging purposes only. Reader should use the module id.
    function_id: FunctionId,
    pub(crate) identifier: String, // For debugging purposes only
}

impl DefinitionRef {
    fn from_decorated_function(function: &DecoratedFunction, context: &ModuleContext) -> Self {
        let name = function.metadata().kind.as_func_id().func;
        let display_range = context.module_info.display_range(function.id_range());
        DefinitionRef {
            module_id: context
                .module_ids
                .get(ModuleKey::from_module(&context.module_info))
                .unwrap(),
            module_name: context.module_info.name().to_string(),
            function_id: FunctionId::Function {
                location: display_range,
            },
            identifier: name.to_string(),
        }
    }

    fn from_find_definition_item_with_docstring(
        item: &FindDefinitionItemWithDocstring,
        function_names: &DashMap<ModuleId, HashMap<FunctionId, String>>,
        context: &ModuleContext,
    ) -> Option<Self> {
        // TODO: For overloads, return the last definition instead of the one from go-to-definitions.
        let display_range = item.module.display_range(item.definition_range);
        let function_id = FunctionId::Function {
            location: display_range,
        };
        let module_id = context
            .module_ids
            .get(ModuleKey::from_module(&item.module))
            .unwrap();
        function_names
            .get(&module_id)
            .and_then(|function_names| function_names.get(&function_id).cloned())
            .map(|function_name| DefinitionRef {
                module_id,
                module_name: item.module.name().to_string(),
                function_id,
                identifier: function_name,
            })
    }
}

#[derive(Debug, Clone, Serialize)]
enum PysaClassMro {
    Resolved(Vec<ClassRef>),
    Cyclic,
}

#[derive(Debug, Clone, Serialize)]
struct PysaClassField {
    #[serde(rename = "type")]
    type_: PysaType,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct GlobalVariable {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    type_: Option<PysaType>,
    location: String,
}

#[derive(Debug, Clone, Serialize)]
struct ClassDefinition {
    class_id: ClassId,
    name: String,
    bases: Vec<ClassRef>,
    mro: PysaClassMro,
    parent: ScopeParent,
    #[serde(skip_serializing_if = "<&bool>::not")]
    is_synthesized: bool, // True if this class was synthesized (e.g., from namedtuple), false if from actual `class X:` statement
    fields: HashMap<String, PysaClassField>,
}

/// Format of a module file `my.module:id.json`
/// Represents all the information Pysa needs about a given module.
#[derive(Debug, Clone, Serialize)]
struct PysaModuleFile {
    format_version: u32,
    module_id: ModuleId,
    module_name: String,
    source_path: ModulePathDetails,
    type_of_expression: HashMap<String, PysaType>,
    function_definitions: HashMap<FunctionId, FunctionDefinition>,
    class_definitions: HashMap<String, ClassDefinition>,
    global_variables: HashMap<String, GlobalVariable>,
}

/// Represents what makes a module unique
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleKey {
    name: ModuleName,
    path: ModulePath,
}

impl ModuleKey {
    pub fn from_handle(handle: &Handle) -> ModuleKey {
        ModuleKey {
            name: handle.module(),
            path: handle.path().clone(),
        }
    }

    fn from_module(module: &Module) -> ModuleKey {
        ModuleKey {
            name: module.name(),
            path: module.path().clone(),
        }
    }
}

pub struct ModuleIds(HashMap<ModuleKey, ModuleId>);

impl ModuleIds {
    /// Multiple python files can map to the same module name (e.g, `foo.bar`).
    /// This creates a unique and deterministic identifier for each handle.
    pub fn new(handles: &[Handle]) -> ModuleIds {
        let mut modules = handles
            .iter()
            .map(ModuleKey::from_handle)
            .collect::<Vec<_>>();
        modules.sort();

        let mut result = HashMap::new();
        let mut current_id = 1;
        for module in modules {
            assert!(
                result.insert(module, ModuleId(current_id)).is_none(),
                "Found multiple handles with the same module name and path"
            );
            current_id += 1;
        }
        ModuleIds(result)
    }

    pub fn get(&self, key: ModuleKey) -> Option<ModuleId> {
        self.0.get(&key).copied()
    }
}

pub fn location_key(range: &DisplayRange) -> String {
    format!(
        "{}:{}-{}:{}",
        range.start.line, range.start.column, range.end.line, range.end.column
    )
}

pub struct ModuleContext<'a> {
    handle: &'a Handle,
    transaction: &'a Transaction<'a>,
    bindings: Bindings,
    answers: Arc<Answers>,
    stdlib: Arc<Stdlib>,
    ast: Arc<ModModule>,
    module_info: Module,
    module_id: ModuleId,
    module_ids: &'a ModuleIds,
}

impl ModuleContext<'_> {
    pub fn create<'a>(
        handle: &'a Handle,
        transaction: &'a Transaction<'a>,
        module_ids: &'a ModuleIds,
    ) -> Option<ModuleContext<'a>> {
        Some(ModuleContext {
            handle,
            transaction,
            bindings: transaction.get_bindings(handle)?,
            answers: transaction.get_answers(handle)?,
            stdlib: transaction.get_stdlib(handle),
            ast: transaction.get_ast(handle)?,
            module_info: transaction.get_module_info(handle)?,
            module_id: module_ids.get(ModuleKey::from_handle(handle))?,
            module_ids,
        })
    }
}

fn string_for_type(type_: &Type) -> String {
    let mut ctx = TypeDisplayContext::new(&[type_]);
    ctx.always_display_module_name_except_builtins();
    ctx.display(type_).to_string()
}

fn strip_self_type(mut ty: Type) -> Type {
    ty.transform_mut(&mut |t| {
        if let Type::SelfType(cls) = t {
            *t = Type::ClassType(cls.clone());
        }
    });
    ty
}

fn has_superclass(class: &Class, want: &Class, context: &ModuleContext) -> bool {
    context
        .transaction
        .ad_hoc_solve(context.handle, |solver| {
            solver.type_order().has_superclass(class, want)
        })
        .unwrap()
}

impl ClassNamesFromType {
    fn from_class(class: Class, context: &ModuleContext) -> ClassNamesFromType {
        ClassNamesFromType {
            class_names: vec![ClassRef::from_class(&class, context.module_ids)],
            stripped_coroutine: false,
            stripped_optional: false,
            stripped_readonly: false,
            unbound_type_variable: false,
            is_exhaustive: true,
        }
    }

    fn not_a_class() -> ClassNamesFromType {
        ClassNamesFromType {
            class_names: vec![],
            stripped_coroutine: false,
            stripped_optional: false,
            stripped_readonly: false,
            unbound_type_variable: false,
            is_exhaustive: false,
        }
    }

    fn skip_serializing(&self) -> bool {
        self.class_names.is_empty()
    }

    fn with_strip_optional(mut self) -> ClassNamesFromType {
        self.stripped_optional = true;
        self
    }

    fn with_strip_coroutine(mut self) -> ClassNamesFromType {
        self.stripped_coroutine = true;
        self
    }

    fn join_with(mut self, other: ClassNamesFromType) -> ClassNamesFromType {
        self.class_names.extend(other.class_names);
        self.stripped_coroutine |= other.stripped_coroutine;
        self.stripped_optional |= other.stripped_optional;
        self.stripped_readonly |= other.stripped_readonly;
        self.unbound_type_variable |= other.unbound_type_variable;
        self.is_exhaustive &= other.is_exhaustive;
        self
    }

    fn sort_and_dedup(mut self) -> ClassNamesFromType {
        self.class_names.sort();
        self.class_names.dedup();
        self
    }
}

fn strip_optional(type_: &Type) -> Option<&Type> {
    match type_ {
        Type::Union(elements) if elements.len() == 2 && elements[0] == Type::None => {
            Some(&elements[1])
        }
        Type::Union(elements) if elements.len() == 2 && elements[1] == Type::None => {
            Some(&elements[0])
        }
        _ => None,
    }
}

fn strip_awaitable<'a>(type_: &'a Type, context: &ModuleContext) -> Option<&'a Type> {
    match type_ {
        Type::ClassType(class_type)
            if class_type.class_object() == context.stdlib.awaitable_object()
                && class_type.targs().as_slice().len() == 1 =>
        {
            Some(&class_type.targs().as_slice()[0])
        }
        _ => None,
    }
}

fn strip_coroutine<'a>(type_: &'a Type, context: &ModuleContext) -> Option<&'a Type> {
    match type_ {
        Type::ClassType(class_type)
            if class_type.class_object() == context.stdlib.coroutine_object()
                && class_type.targs().as_slice().len() >= 3 =>
        {
            Some(&class_type.targs().as_slice()[2])
        }
        _ => None,
    }
}

fn is_scalar_type(get: &Type, want: &Class, context: &ModuleContext) -> bool {
    if let Some(inner) = strip_optional(get) {
        return is_scalar_type(inner, want, context);
    }
    if let Some(inner) = strip_awaitable(get, context) {
        return is_scalar_type(inner, want, context);
    }
    if let Some(inner) = strip_coroutine(get, context) {
        return is_scalar_type(inner, want, context);
    }
    match get {
        Type::ClassType(class_type) => has_superclass(class_type.class_object(), want, context),
        Type::TypeAlias(alias) => is_scalar_type(&alias.as_type(), want, context),
        _ => false,
    }
}

fn get_classes_of_type(type_: &Type, context: &ModuleContext) -> ClassNamesFromType {
    if let Some(inner) = strip_optional(type_) {
        return get_classes_of_type(inner, context).with_strip_optional();
    }
    if let Some(inner) = strip_awaitable(type_, context) {
        return get_classes_of_type(inner, context).with_strip_coroutine();
    }
    if let Some(inner) = strip_coroutine(type_, context) {
        return get_classes_of_type(inner, context).with_strip_coroutine();
    }
    // No need to strip ReadOnly[], it is already stripped by pyrefly.
    match type_ {
        Type::ClassType(class_type) => {
            ClassNamesFromType::from_class(class_type.class_object().clone(), context)
        }
        Type::Tuple(_) => {
            ClassNamesFromType::from_class(context.stdlib.tuple_object().clone(), context)
        }
        Type::Union(elements) if !elements.is_empty() => elements
            .iter()
            .map(|inner| get_classes_of_type(inner, context))
            .reduce(|acc, next| acc.join_with(next))
            .unwrap()
            .sort_and_dedup(),
        Type::TypeAlias(alias) => get_classes_of_type(&alias.as_type(), context),
        _ => ClassNamesFromType::not_a_class(),
    }
}

impl PysaType {
    fn from_type(type_: &Type, context: &ModuleContext) -> PysaType {
        // Promote `Literal[..]` into `str` or `int`.
        let type_ = type_.clone().promote_literals(&context.stdlib);
        let type_ = strip_self_type(type_);

        let string = string_for_type(&type_);

        PysaType {
            string,
            is_bool: is_scalar_type(&type_, context.stdlib.bool().class_object(), context),
            is_int: is_scalar_type(&type_, context.stdlib.int().class_object(), context),
            is_float: is_scalar_type(&type_, context.stdlib.float().class_object(), context),
            is_enum: is_scalar_type(&type_, context.stdlib.enum_class().class_object(), context),
            class_names: get_classes_of_type(&type_, context),
        }
    }
}

struct VisitorContext<'a> {
    module_context: &'a ModuleContext<'a>,
    type_of_expression: &'a mut HashMap<String, PysaType>,
    global_variables: &'a mut HashMap<String, GlobalVariable>,
}

fn export_function_parameter(param: &Param, context: &ModuleContext) -> FunctionParameter {
    match param {
        Param::PosOnly(name, ty, required) => FunctionParameter::PosOnly {
            name: name.as_ref().map(|n| n.to_string()),
            annotation: PysaType::from_type(ty, context),
            required: matches!(required, pyrefly_types::callable::Required::Required),
        },
        Param::Pos(name, ty, required) => FunctionParameter::Pos {
            name: name.to_string(),
            annotation: PysaType::from_type(ty, context),
            required: matches!(required, pyrefly_types::callable::Required::Required),
        },
        Param::VarArg(name, ty) => FunctionParameter::VarArg {
            name: name.as_ref().map(|n| n.to_string()),
            annotation: PysaType::from_type(ty, context),
        },
        Param::KwOnly(name, ty, required) => FunctionParameter::KwOnly {
            name: name.to_string(),
            annotation: PysaType::from_type(ty, context),
            required: matches!(required, pyrefly_types::callable::Required::Required),
        },
        Param::Kwargs(name, ty) => FunctionParameter::Kwargs {
            name: name.as_ref().map(|n| n.to_string()),
            annotation: PysaType::from_type(ty, context),
        },
    }
}

fn export_function_parameters(params: &Params, context: &ModuleContext) -> FunctionParameters {
    match params {
        Params::List(params) => FunctionParameters::List(
            params
                .items()
                .iter()
                .map(|param| export_function_parameter(param, context))
                .collect(),
        ),
        Params::Ellipsis => FunctionParameters::Ellipsis,
        Params::ParamSpec(_, _) => FunctionParameters::ParamSpec,
    }
}

fn export_function_signature(function: &Callable, context: &ModuleContext) -> FunctionSignature {
    FunctionSignature {
        parameters: export_function_parameters(&function.params, context),
        return_annotation: PysaType::from_type(&function.ret, context),
    }
}

fn visit_expression(e: &Expr, context: &mut VisitorContext) {
    let range = e.range();

    // If the expression has a type, export it.
    if let Some(type_) = context.module_context.answers.get_type_trace(range) {
        let display_range = context.module_context.module_info.display_range(range);

        assert!(
            context
                .type_of_expression
                .insert(
                    location_key(&display_range),
                    PysaType::from_type(&type_, context.module_context)
                )
                .is_none(),
            "Found expressions with the same location"
        );
    }

    e.recurse(&mut |e| visit_expression(e, context));
}

fn visit_assign_target(target: &Expr, is_top_level: bool, context: &mut VisitorContext) {
    if !is_top_level {
        return;
    }

    Ast::expr_lvalue(target, &mut |global: &ExprName| {
        let type_ = context
            .module_context
            .bindings
            .key_to_idx_hashed_opt(Hashed::new(&Key::Definition(ShortIdentifier::expr_name(
                global,
            ))))
            .and_then(|idx| context.module_context.answers.get_idx(idx));
        if let Some(type_) = type_.as_ref()
            && type_.ty().is_type_variable()
        {
            // Don't export type variable globals.
            return;
        }
        let location = location_key(
            &context
                .module_context
                .module_info
                .display_range(global.range()),
        );
        context
            .global_variables
            .entry(global.id.to_string())
            .or_insert(GlobalVariable {
                type_: type_.map(|type_| PysaType::from_type(type_.ty(), context.module_context)),
                location,
            });
    });
}

fn visit_statement(stmt: &Stmt, is_top_level: bool, context: &mut VisitorContext) {
    match stmt {
        Stmt::FunctionDef(function_def) => {
            visit_expressions(
                function_def
                    .decorator_list
                    .iter()
                    .map(|decorator| &decorator.expression),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .posonlyargs
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .args
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_expressions(
                function_def
                    .parameters
                    .kwonlyargs
                    .iter()
                    .filter_map(|argument| argument.default.as_deref()),
                context,
            );
            visit_statements(
                function_def.body.iter(),
                /* is_top_level */ false,
                context,
            );
        }
        Stmt::ClassDef(class_def) => {
            visit_expressions(
                class_def
                    .decorator_list
                    .iter()
                    .map(|decorator| &decorator.expression),
                context,
            );
            if let Some(arguments) = &class_def.arguments {
                visit_expressions(arguments.args.iter(), context);
                visit_expressions(
                    arguments.keywords.iter().map(|keyword| &keyword.value),
                    context,
                );
            }
            visit_statements(
                class_def.body.iter(),
                /* is_top_level */ false,
                context,
            );
        }
        Stmt::Expr(e) => {
            visit_expression(&e.value, context);
        }
        Stmt::Assign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            for t in &assign.targets {
                visit_assign_target(t, is_top_level, context);
            }
        }
        Stmt::AnnAssign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            visit_assign_target(&assign.target, is_top_level, context);
        }
        Stmt::AugAssign(assign) => {
            stmt.visit(&mut |e| visit_expression(e, context));
            visit_assign_target(&assign.target, is_top_level, context);
        }
        Stmt::Return(_) | Stmt::Delete(_) | Stmt::Raise(_) => {
            // Statements that only contains expressions, use Visit<Expr>
            stmt.visit(&mut |e| visit_expression(e, context));
        }
        Stmt::For(for_stmt) => {
            visit_expression(&for_stmt.iter, context);
            visit_expression(&for_stmt.target, context);
            visit_statements(for_stmt.body.iter(), is_top_level, context);
            visit_statements(for_stmt.orelse.iter(), is_top_level, context);
        }
        Stmt::While(while_stmt) => {
            visit_expression(&while_stmt.test, context);
            visit_statements(while_stmt.body.iter(), is_top_level, context);
            visit_statements(while_stmt.orelse.iter(), is_top_level, context);
        }
        Stmt::If(if_stmt) => {
            visit_expression(&if_stmt.test, context);
            visit_statements(if_stmt.body.iter(), is_top_level, context);
            for elif_else_clause in &if_stmt.elif_else_clauses {
                if let Some(test) = &elif_else_clause.test {
                    visit_expression(test, context);
                }
                visit_statements(elif_else_clause.body.iter(), is_top_level, context);
            }
        }
        Stmt::With(with_stmt) => {
            for item in &with_stmt.items {
                visit_expression(&item.context_expr, context);
                visit_expressions(item.optional_vars.iter().map(|x| &**x), context);
            }
            visit_statements(with_stmt.body.iter(), is_top_level, context);
        }
        Stmt::Match(match_stmt) => {
            visit_expression(&match_stmt.subject, context);
            for case in &match_stmt.cases {
                if let Some(guard) = &case.guard {
                    visit_expression(guard, context);
                }
                visit_statements(case.body.iter(), is_top_level, context);
            }
        }
        Stmt::Try(try_stmt) => {
            visit_statements(try_stmt.body.iter(), is_top_level, context);
            visit_statements(try_stmt.orelse.iter(), is_top_level, context);
            visit_statements(try_stmt.finalbody.iter(), is_top_level, context);
            for ruff_python_ast::ExceptHandler::ExceptHandler(except_handler) in &try_stmt.handlers
            {
                if let Some(annotation) = &except_handler.type_ {
                    visit_expression(annotation, context);
                }
                visit_statements(except_handler.body.iter(), is_top_level, context);
            }
        }
        Stmt::Assert(assert_stmt) => {
            visit_expression(&assert_stmt.test, context);
            if let Some(msg) = &assert_stmt.msg {
                visit_expression(msg, context);
            }
        }
        Stmt::TypeAlias(_)
        | Stmt::Import(_)
        | Stmt::ImportFrom(_)
        | Stmt::Global(_)
        | Stmt::Nonlocal(_)
        | Stmt::Pass(_)
        | Stmt::Break(_)
        | Stmt::Continue(_)
        | Stmt::IpyEscapeCommand(_) => {
            // do nothing.
        }
    }
}

fn visit_expressions<'a>(
    expressions: impl Iterator<Item = &'a Expr>,
    context: &mut VisitorContext,
) {
    for expr in expressions {
        visit_expression(expr, context);
    }
}

fn visit_statements<'a>(
    statements: impl Iterator<Item = &'a Stmt>,
    is_top_level: bool,
    context: &mut VisitorContext,
) {
    for stmt in statements {
        visit_statement(stmt, is_top_level, context);
    }
}

fn get_scope_parent(ast: &ModModule, module_info: &Module, range: TextRange) -> ScopeParent {
    Ast::locate_node(ast, range.start())
        .iter()
        .find_map(|node| match node {
            AnyNodeRef::Identifier(id) if id.range() == range => None,
            AnyNodeRef::StmtClassDef(class_def) if class_def.name.range() == range => None,
            AnyNodeRef::StmtFunctionDef(fun_def) if fun_def.name.range() == range => None,
            AnyNodeRef::StmtClassDef(class_def) => Some(ScopeParent::Class {
                location: location_key(&module_info.display_range(class_def.name.range())),
            }),
            AnyNodeRef::StmtFunctionDef(fun_def) => Some(ScopeParent::Function {
                location: location_key(&module_info.display_range(fun_def.name.range())),
            }),
            _ => None,
        })
        .unwrap_or(ScopeParent::TopLevel)
}

fn get_all_functions(
    bindings: &Bindings,
    answers: &Answers,
) -> impl Iterator<Item = DecoratedFunction> {
    bindings
        .keys::<KeyDecoratedFunction>()
        .map(|idx| DecoratedFunction::from_bindings_answers(idx, bindings, answers))
}

// Return the function type, considering decorators and overloads.
fn get_function_type(function: &DecoratedFunction, context: &ModuleContext) -> Type {
    let definition_binding = Key::Definition(function.undecorated.identifier);
    let idx = context.bindings.key_to_idx(&definition_binding);
    context.answers.get_idx(idx).unwrap().arc_clone_ty()
}

fn get_undecorated_return_type(function: &DecoratedFunction, context: &ModuleContext) -> Type {
    let return_binding = Key::ReturnType(function.undecorated.identifier);
    let idx = context.bindings.key_to_idx(&return_binding);
    context.answers.get_idx(idx).unwrap().arc_clone_ty()
}

fn should_export_function(function: &DecoratedFunction, context: &ModuleContext) -> bool {
    // We only want to export one function when we have an @overload chain.
    // If the function has no successor (function in the same scope with the same name), then we should export it.
    // If the function has successors, but is not an overload, then we should export it. It probably means the successor is a redefinition.
    let has_successor = context.bindings.get(function.idx).successor.is_some();
    !has_successor || !function.is_overload()
}

fn export_all_functions(
    reversed_override_graph: &DashMap<DefinitionRef, DefinitionRef>,
    context: &ModuleContext,
) -> HashMap<FunctionId, FunctionDefinition> {
    let mut function_definitions = HashMap::new();

    for function in get_all_functions(&context.bindings, &context.answers) {
        if !should_export_function(&function, context) {
            continue;
        }

        // We need the list of raw parameters, ignoring decorators.
        // For overloads, we need the list of all overloads, not just the current one.
        // To get it, we check if `get_function_type` returns `Type::Overload`.
        let decorated_type = get_function_type(&function, context);
        let undecorated_signatures = match decorated_type {
            Type::Overload(Overload { signatures, .. }) => signatures
                .iter()
                .map(|overload_type| match overload_type {
                    pyrefly_types::types::OverloadType::Function(f) => f,
                    pyrefly_types::types::OverloadType::Forall(pyrefly_types::types::Forall {
                        body,
                        ..
                    }) => body,
                })
                .map(|function| export_function_signature(&function.signature, context))
                .collect::<Vec<_>>(),
            _ => vec![FunctionSignature {
                parameters: FunctionParameters::List(
                    function
                        .undecorated
                        .params
                        .iter()
                        .map(|param| export_function_parameter(param, context))
                        .collect(),
                ),
                return_annotation: PysaType::from_type(
                    &get_undecorated_return_type(&function, context),
                    context,
                ),
            }],
        };

        let current_function = DefinitionRef::from_decorated_function(&function, context);
        let parent = get_scope_parent(&context.ast, &context.module_info, function.id_range());
        assert!(
            function_definitions
                .insert(
                    current_function.function_id.clone(),
                    FunctionDefinition {
                        name: current_function.identifier.clone(),
                        parent,
                        undecorated_signatures,
                        is_overload: function.metadata().flags.is_overload,
                        is_staticmethod: function.metadata().flags.is_staticmethod,
                        is_classmethod: function.metadata().flags.is_classmethod,
                        is_property_getter: function.metadata().flags.is_property_getter,
                        is_property_setter: function
                            .metadata()
                            .flags
                            .is_property_setter_with_getter
                            .is_some(),
                        is_stub: function.is_stub(),
                        defining_class: function
                            .defining_cls()
                            .map(|class| ClassRef::from_class(class, context.module_ids)),
                        overridden_base_method: reversed_override_graph
                            .get(&current_function)
                            .map(|r| r.clone()),
                    }
                )
                .is_none(),
            "Found function definitions with the same location"
        );
    }

    function_definitions
}

fn get_all_classes(bindings: &Bindings, answers: &Answers) -> impl Iterator<Item = Class> {
    bindings
        .keys::<KeyClass>()
        .map(|idx| answers.get_idx(idx).unwrap().0.clone().unwrap())
}

fn get_class_field(
    class: &Class,
    field: &Name,
    context: &ModuleContext,
) -> Option<Arc<ClassField>> {
    context
        .transaction
        .ad_hoc_solve(context.handle, |solver| {
            solver.get_field_from_current_class_only(class, field)
        })
        .unwrap()
}

fn get_class_field_declaration<'a>(
    class: &'a Class,
    field: &'a Name,
    context: &'a ModuleContext,
) -> Option<&'a BindingClassField> {
    let key_class_field = KeyClassField(class.index(), field.clone());
    // We use `key_to_idx_hashed_opt` below because the key might not be valid (could be a synthesized field).
    context
        .bindings
        .key_to_idx_hashed_opt(Hashed::new(&key_class_field))
        .map(|idx| context.bindings.get(idx))
}

fn get_class_mro(class: &Class, bindings: &Bindings, answers: &Answers) -> Arc<ClassMro> {
    answers
        .get_idx(bindings.key_to_idx(&KeyClassMro(class.index())))
        .unwrap()
}

fn export_class_fields(class: &Class, context: &ModuleContext) -> HashMap<String, PysaClassField> {
    class
        .fields()
        .filter_map(|name| get_class_field(class, name, context).map(|field| (name, field)))
        .filter_map(|(name, field)| {
            match get_class_field_declaration(class, name, context) {
                Some(BindingClassField {
                    definition: ClassFieldDefinition::MethodLike { .. },
                    ..
                }) => {
                    // Exclude fields that are functions definitons, because they are already exported in `function_definitions`.
                    None
                }
                Some(BindingClassField { range, .. }) => Some((
                    name.to_string(),
                    PysaClassField {
                        type_: PysaType::from_type(&field.ty(), context),
                        location: Some(location_key(&context.module_info.display_range(*range))),
                    },
                )),
                _ => Some((
                    name.to_string(),
                    PysaClassField {
                        type_: PysaType::from_type(&field.ty(), context),
                        location: None,
                    },
                )),
            }
        })
        .collect()
}

fn export_all_classes(
    ast: &ModModule,
    context: &ModuleContext,
) -> HashMap<String, ClassDefinition> {
    let mut class_definitions = HashMap::new();

    for class_idx in context.bindings.keys::<KeyClass>() {
        let class = context
            .answers
            .get_idx(class_idx)
            .unwrap()
            .0
            .clone()
            .unwrap();
        let display_range = context.module_info.display_range(class.qname().range());
        let class_index = class.index();
        let parent = get_scope_parent(ast, &context.module_info, class.qname().range());
        let metadata = context
            .answers
            .get_idx(context.bindings.key_to_idx(&KeyClassMetadata(class_index)))
            .unwrap();

        let is_synthesized = match context.bindings.get(class_idx) {
            BindingClass::FunctionalClassDef(_, _, _, _) => true,
            BindingClass::ClassDef(_) => false,
        };

        let fields = export_class_fields(&class, context);

        let bases = metadata
            .base_class_objects()
            .iter()
            .map(|base_class| ClassRef::from_class(base_class, context.module_ids))
            .collect::<Vec<_>>();

        let mro = match &*get_class_mro(&class, &context.bindings, &context.answers) {
            ClassMro::Resolved(mro) => PysaClassMro::Resolved(
                mro.iter()
                    .map(|class_type| {
                        ClassRef::from_class(class_type.class_object(), context.module_ids)
                    })
                    .collect(),
            ),
            ClassMro::Cyclic => PysaClassMro::Cyclic,
        };

        let class_definition = ClassDefinition {
            class_id: ClassId::from_class(&class),
            name: class.qname().id().to_string(),
            parent,
            bases,
            mro,
            is_synthesized,
            fields,
        };

        assert!(
            class_definitions
                .insert(location_key(&display_range), class_definition)
                .is_none(),
            "Found class definitions with the same location"
        );
    }

    class_definitions
}

fn is_unittest_module(bindings: &Bindings, answers: &Answers) -> bool {
    get_all_classes(bindings, answers).any(|class| {
        match &*get_class_mro(&class, bindings, answers) {
            ClassMro::Resolved(mro) => mro
                .iter()
                .any(|base| base.has_qname("unittest.case", "TestCase")),
            ClassMro::Cyclic => false,
        }
    })
}

fn is_pytest_module(bindings: &Bindings, answers: &Answers, ast: &ModModule) -> bool {
    fn has_pytest_prefix(name: &Name) -> bool {
        name == "pytest" || name.starts_with("pytest.")
    }
    fn imports_pytest(ast: &ModModule) -> bool {
        ast.body.iter().any(|stmt| match stmt {
            Stmt::Import(import_stmt) => import_stmt
                .names
                .iter()
                .any(|alias| has_pytest_prefix(&alias.name.id)),
            Stmt::ImportFrom(StmtImportFrom {
                module: Some(module),
                ..
            }) => has_pytest_prefix(&module.id),
            _ => false,
        })
    }
    fn has_test_function(bindings: &Bindings, answers: &Answers) -> bool {
        get_all_functions(bindings, answers).any(|function| {
            function
                .metadata()
                .kind
                .as_func_id()
                .func
                .starts_with("test_")
        })
    }
    imports_pytest(ast) && has_test_function(bindings, answers)
}

/// Returns true if the module is considered a test module for Pysa.
/// In that case, we won't analyze the module at all.
///
/// We currently use the following heuristics:
/// - If a class inherits from `unittest.TestCase`, we assume this is a test file.
/// - If `pytest` is imported and at least one function starts with `test_`, we assume this is a test file.
fn is_test_module(context: &ModuleContext) -> bool {
    is_unittest_module(&context.bindings, &context.answers)
        || is_pytest_module(&context.bindings, &context.answers, &context.ast)
}

fn get_module_file(
    context: &ModuleContext,
    reversed_override_graph: &DashMap<DefinitionRef, DefinitionRef>,
) -> PysaModuleFile {
    let mut type_of_expression = HashMap::new();
    let mut global_variables = HashMap::new();

    for stmt in &context.ast.body {
        visit_statement(
            stmt,
            /* is_top_level */ true,
            &mut VisitorContext {
                module_context: context,
                type_of_expression: &mut type_of_expression,
                global_variables: &mut global_variables,
            },
        );
    }

    let function_definitions = export_all_functions(reversed_override_graph, context);
    let class_definitions = export_all_classes(&context.ast, context);

    PysaModuleFile {
        format_version: 1,
        module_id: context.module_id,
        module_name: context.module_info.name().to_string(),
        source_path: context.module_info.path().details().clone(),
        type_of_expression,
        function_definitions,
        class_definitions,
        global_variables,
    }
}

#[allow(dead_code)]
fn collect_function_names_for_module(context: &ModuleContext) -> HashMap<FunctionId, String> {
    let mut function_names = HashMap::new();
    for function in get_all_functions(&context.bindings, &context.answers) {
        if !should_export_function(&function, context) {
            continue;
        }
        let current_function = DefinitionRef::from_decorated_function(&function, context);
        function_names.insert(current_function.function_id, current_function.identifier);
    }
    function_names
}

#[allow(dead_code)]
pub fn collect_function_names(
    handles: &Vec<Handle>,
    transaction: &Transaction,
    module_ids: &ModuleIds,
) -> DashMap<ModuleId, HashMap<FunctionId, String>> {
    let all_function_names = DashMap::new();
    ThreadPool::new().install(|| {
        handles.par_iter().for_each(|handle| {
            let module_id = module_ids.get(ModuleKey::from_handle(handle)).unwrap();
            let function_names = ModuleContext::create(handle, transaction, module_ids)
                .map_or(HashMap::new(), |context| {
                    collect_function_names_for_module(&context)
                });
            all_function_names.insert(module_id, function_names);
        });
    });
    all_function_names
}

pub fn write_results(results_directory: &Path, transaction: &Transaction) -> anyhow::Result<()> {
    let start = Instant::now();
    info!("Writing results to `{}`", results_directory.display());
    fs_anyhow::create_dir_all(results_directory)?;
    fs_anyhow::create_dir_all(&results_directory.join("modules"))?;

    let handles = transaction.handles();
    let module_ids = ModuleIds::new(&handles);
    let mut project_modules = HashMap::new();

    let mut module_info_tasks = Vec::new();
    for handle in &handles {
        let module_id = module_ids.get(ModuleKey::from_handle(handle)).unwrap();

        // Path where we will store the information on the module.
        let info_path = match handle.path().details() {
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
                    module_id.0
                )))
            }
        };

        assert!(
            project_modules
                .insert(
                    module_id,
                    PysaProjectModule {
                        module_id,
                        module_name: handle.module().to_string(),
                        source_path: handle.path().details().clone(),
                        info_path: info_path.clone(),
                        is_test: false,
                        is_interface: handle.path().is_interface(),
                        is_init: handle.path().is_init(),
                    }
                )
                .is_none(),
            "Found multiple handles with the same module id"
        );

        if let Some(info_path) = info_path {
            module_info_tasks.push((handle, module_id, info_path));
        }
    }

    let project_modules = Arc::new(Mutex::new(project_modules));

    let reversed_override_graph = {
        let reversed_override_graph = DashMap::new();
        ThreadPool::new().install(|| {
            module_info_tasks.par_iter().for_each(|(handle, _, _)| {
                let context = ModuleContext::create(handle, transaction, &module_ids).unwrap();

                for (key, value) in create_reversed_override_graph_for_module(&context) {
                    reversed_override_graph.insert(key, value);
                }
            });
        });
        reversed_override_graph
    };

    let _override_graph = OverrideGraph::from_reversed(&reversed_override_graph);

    // Retrieve and dump information about each module, in parallel.
    ThreadPool::new().install(|| -> anyhow::Result<()> {
        module_info_tasks.into_par_iter().try_for_each(
            |(handle, module_id, info_path)| -> anyhow::Result<()> {
                let writer = BufWriter::new(File::create(
                    results_directory.join("modules").join(info_path),
                )?);
                let context = ModuleContext::create(handle, transaction, &module_ids).unwrap();
                serde_json::to_writer(
                    writer,
                    &get_module_file(&context, &reversed_override_graph),
                )?;

                if is_test_module(&context) {
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
