/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(warnings)] // It will be autogerated

use serde::Serialize;

use crate::report::glean::schema::*;

/// Represents a name in the Python code
#[derive(Debug, Clone, Serialize)]
pub struct Name {
    pub id: u64,
    pub key: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct Type {
    pub id: u64,
    pub key: String,
}

/// Represents a module in the Python code
#[derive(Debug, Clone, Serialize)]
pub struct Module {
    pub id: u64,
    pub key: Module_key,
}

/// Represents a module in the Python code
#[derive(Debug, Clone, Serialize)]
pub struct Module_key {
    pub name: Name,
}

/// Reference to a class
#[derive(Debug, Clone, Serialize)]
pub struct ClassDeclaration {
    pub id: u64,
    pub key: Box<ClassDeclaration_key>,
}

/// Represents a class declaration
#[derive(Debug, Clone, Serialize)]
pub struct ClassDeclaration_key {
    pub name: Name,
    // Add bases
}

/// Reference to a function
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub id: u64,
    pub key: Box<FunctionDeclaration_key>,
}

/// Represents a function declaration
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDeclaration_key {
    pub name: Name,
}

/// Reference to a variable
#[derive(Debug, Clone, Serialize)]
pub struct VariableDeclaration {
    pub id: u64,
    pub key: Box<VariableDeclaration_key>,
}

/// Represents a variable declaration
#[derive(Debug, Clone, Serialize)]
pub struct VariableDeclaration_key {
    pub name: Name,
}

/// Reference to an import
#[derive(Debug, Clone, Serialize)]
pub struct ImportStatement {
    pub id: u64,
    pub key: Box<ImportStatement_key>,
}

/// Represents an import statement
#[derive(Debug, Clone, Serialize)]
pub struct ImportStatement_key {
    pub from_name: Name,
    pub as_name: Name,
}

/// Represents a declaration in the Python code
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Declaration {
    Module { module: Box<Module> },
    Class { cls: Box<ClassDeclaration> },
    Function { func: Box<FunctionDeclaration> },
    Variable { variable: Box<VariableDeclaration> },
    Import { imp: Box<ImportStatement> },
}

#[derive(Debug, Clone, Serialize)]
pub struct DeclarationLocation {
    pub id: u64,
    pub key: Box<DeclarationLocation_key>,
}

/// Represents a declaration location
#[derive(Debug, Clone, Serialize)]
pub struct DeclarationLocation_key {
    pub declaration: Declaration,
    pub file: src::File,
    pub span: src::ByteSpan,
}

/// Represents a container for declarations
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum DeclarationContainer {
    Module { module: Module },
    Class { cls: ClassDeclaration },
    Function { func: FunctionDeclaration },
}

/// Represents a containing top-level declaration
#[derive(Debug, Clone, Serialize)]
pub struct ContainingTopLevelDeclaration {
    pub id: u64,
    pub key: Box<ContainingTopLevelDeclaration_key>,
}

/// Represents a containing top-level declaration
#[derive(Debug, Clone, Serialize)]
pub struct ContainingTopLevelDeclaration_key {
    pub declaration: Declaration,
    pub container: DeclarationContainer,
}

/// Represents a cross-reference via name
#[derive(Debug, Clone, Serialize)]
pub struct XRefViaName {
    pub target: Name,
    pub source: src::ByteSpan,
}

#[derive(Debug, Clone, Serialize)]
pub struct XRefsViaNameByFile {
    pub id: u64,
    pub key: Box<XRefsViaNameByFile_key>,
}

/// Represents cross-references via name by file
#[derive(Debug, Clone, Serialize)]
pub struct XRefsViaNameByFile_key {
    pub file: src::File,
    pub xrefs: Vec<XRefViaName>,
}

/// Represents a structured name
#[derive(Debug, Clone, Serialize)]
pub struct SName {
    pub id: u64,
    pub key: Box<SName_key>,
}

/// Represents a structured name
#[derive(Debug, Clone, Serialize)]
pub struct SName_key {
    pub local_name: Name,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<SName>,
}

/// Maps a name to a structured name
#[derive(Debug, Clone, Serialize)]
pub struct NameToSName {
    pub id: u64,
    pub key: Name,
    pub value: SName,
}

/// Represents an import star statement
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarStatement {
    pub id: u64,
    pub key: Box<ImportStarStatement_key>,
}

/// Represents an import star statement
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarStatement_key {
    pub from_name: Name,
    pub into_module: Module,
}

/// Represents an import star location
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarLocation {
    pub id: u64,
    pub key: Box<ImportStarLocation_key>,
}

/// Represents an import star location
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarLocation_key {
    pub import_star: ImportStarStatement,
    pub file: src::File,
    pub span: src::ByteSpan,
}

/// Represents a definition
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Definition {
    Class { cls: Box<ClassDefinition> },
    Function { func: Box<FunctionDefinition> },
    Variable { variable: Box<VariableDefinition> },
    Module { module: Box<ModuleDefinition> },
}

/// Represents a class definition
#[derive(Debug, Clone, Serialize)]
pub struct ClassDefinition {
    pub id: u64,
    pub key: Box<ClassDefinition_key>,
}

/// Represents a class definition
#[derive(Debug, Clone, Serialize)]
pub struct ClassDefinition_key {
    pub declaration: ClassDeclaration,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub bases: Vec<ClassDeclaration>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub decorators: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<DeclarationContainer>,
}

/// Represents a function definition
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDefinition {
    pub id: u64,
    pub key: Box<FunctionDefinition_key>,
}

/// Represents a function definition
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDefinition_key {
    pub declaration: FunctionDeclaration,
    pub is_async: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "returnsInfo")]
    pub returns_info: Option<TypeInfo>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub params: Vec<Parameter>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub posonly_params: Vec<Parameter>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub kwonly_params: Vec<Parameter>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub star_arg: Option<Parameter>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub star_kwarg: Option<Parameter>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub decorators: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<DeclarationContainer>,
}

/// Represents a variable definition
#[derive(Debug, Clone, Serialize)]
pub struct VariableDefinition {
    pub id: u64,
    pub key: Box<VariableDefinition_key>,
}

/// Represents a variable definition
#[derive(Debug, Clone, Serialize)]
pub struct VariableDefinition_key {
    pub declaration: VariableDeclaration,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "typeInfo")]
    pub type_info: Option<TypeInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<DeclarationContainer>,
}

/// Represents a module definition
#[derive(Debug, Clone, Serialize)]
pub struct ModuleDefinition {
    pub id: u64,
    pub key: Box<ModuleDefinition_key>,
}

/// Represents a module definition
#[derive(Debug, Clone, Serialize)]
pub struct ModuleDefinition_key {
    pub module: Module,
}

/// Represents a parameter
#[derive(Debug, Clone, Serialize)]
pub struct Parameter {
    pub name: Name,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "typeInfo")]
    pub type_info: Option<TypeInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
}

/// Represents type information
#[derive(Debug, Clone, Serialize)]
pub struct TypeInfo {
    #[serde(rename = "displayType")]
    pub display_type: Type,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub xrefs: Vec<XRefViaName>,
}

/// Represents a definition location
#[derive(Debug, Clone, Serialize)]
pub struct DefinitionLocation {
    pub id: u64,
    pub key: Box<DefinitionLocation_key>,
}

/// Represents a definition location
#[derive(Debug, Clone, Serialize)]
pub struct DefinitionLocation_key {
    pub definition: Definition,
    pub file: src::File,
    pub span: src::ByteSpan,
}

/// Represents a declaration docstring
#[derive(Debug, Clone, Serialize)]
pub struct DeclarationDocstring {
    pub id: u64,
    pub key: Box<DeclarationDocstring_key>,
}

/// Represents a declaration docstring
#[derive(Debug, Clone, Serialize)]
pub struct DeclarationDocstring_key {
    pub declaration: Declaration,
    pub location: src::ByteSpan,
    pub pretty_text: String,
}

/// Represents a call argument
#[derive(Debug, Clone, Serialize)]
pub struct CallArgument {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<Name>,
    pub span: src::ByteSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub argument: Option<Argument>,
}

/// Represents an argument
#[derive(Debug, Clone, Serialize)]
pub enum Argument {
    StringLiteral { lit: StringLiteral },
}

/// Represents a string literal
#[derive(Debug, Clone, Serialize)]
pub struct StringLiteral {
    pub id: u64,
    pub key: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct FileCall {
    pub id: u64,
    pub key: FileCall_key,
}

/// Represents a file call
#[derive(Debug, Clone, Serialize)]
pub struct FileCall_key {
    pub file: src::File,
    pub callee_span: src::ByteSpan,
    pub call_args: Vec<CallArgument>,
}

/// Represents a callee to caller relationship
#[derive(Debug, Clone, Serialize)]
pub struct CalleeToCaller {
    pub id: u64,
    pub key: Box<CalleeToCaller_key>,
}

/// Represents a callee to caller relationship
#[derive(Debug, Clone, Serialize)]
pub struct CalleeToCaller_key {
    pub callee: Name,
    pub caller: Name,
}
