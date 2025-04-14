/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // We don't yet generate this

use serde::Serialize;

/// The Schema ID for Python as specified by Glean
pub const PYTHON_SCHEMA_ID: &str = "54195d609c9195d2bc09d8fa05050bf7";

/// Represents a Glean JSON file containing Python indexer data
#[derive(Debug, Clone, Serialize)]
pub struct Glean {
    /// The schema entries in the Glean file
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub entries: Vec<GleanEntry>,
}

/// Represents an entry in a Glean file, which can be either a schema ID or a predicate
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum GleanEntry {
    /// Schema ID entry
    SchemaId { schema_id: String },
    /// Predicate entry containing facts
    Predicate { predicate: String, facts: Vec<Fact> },
}

/// Represents a fact in a predicate
#[derive(Debug, Clone, Serialize)]
pub struct Fact {
    /// The ID of the fact
    pub id: u64,
    /// The key of the fact, which can contain various nested structures
    pub key: serde_json::Value,
    /// Optional value field that some facts may have
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<serde_json::Value>,
}

/// Represents a name in the Python code
#[derive(Debug, Clone, Serialize)]
pub struct Name {
    pub id: u64,
    pub key: String,
}

/// Represents a module in the Python code
#[derive(Debug, Clone, Serialize)]
pub struct Module {
    pub name: Name,
}

/// Represents a byte span in the source code
#[derive(Debug, Clone, Serialize)]
pub struct ByteSpan {
    pub start: u64,
    pub length: u64,
}

/// Represents a file in the source code
#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: u64,
    pub key: String,
}

/// Represents a file digest
#[derive(Debug, Clone, Serialize)]
pub struct FileDigest {
    pub file: File,
    pub digest: Digest,
}

/// Represents a digest with hash and size
#[derive(Debug, Clone, Serialize)]
pub struct Digest {
    pub hash: String,
    pub size: u64,
}

/// Represents a declaration in the Python code
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum Declaration {
    Module { module: Box<ModuleRef> },
    Class { cls: Box<ClassRef> },
    Function { func: Box<FunctionRef> },
    Variable { variable: Box<VariableRef> },
    Import { imp: Box<ImportRef> },
}

/// Reference to a module
#[derive(Debug, Clone, Serialize)]
pub struct ModuleRef {
    pub id: u64,
    pub key: Module,
}

/// Reference to a class
#[derive(Debug, Clone, Serialize)]
pub struct ClassRef {
    pub id: u64,
    pub key: ClassDeclaration,
}

/// Reference to a function
#[derive(Debug, Clone, Serialize)]
pub struct FunctionRef {
    pub id: u64,
    pub key: FunctionDeclaration,
}

/// Reference to a variable
#[derive(Debug, Clone, Serialize)]
pub struct VariableRef {
    pub id: u64,
    pub key: VariableDeclaration,
}

/// Reference to an import
#[derive(Debug, Clone, Serialize)]
pub struct ImportRef {
    pub id: u64,
    pub key: ImportStatement,
}

/// Represents a class declaration
#[derive(Debug, Clone, Serialize)]
pub struct ClassDeclaration {
    pub name: Name,
}

/// Represents a function declaration
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub name: Name,
}

/// Represents a variable declaration
#[derive(Debug, Clone, Serialize)]
pub struct VariableDeclaration {
    pub name: Name,
}

/// Represents an import statement
#[derive(Debug, Clone, Serialize)]
pub struct ImportStatement {
    pub from_name: Name,
    pub as_name: Name,
}

/// Represents a declaration location
#[derive(Debug, Clone, Serialize)]
pub struct DeclarationLocation {
    pub declaration: Declaration,
    pub file: File,
    pub span: ByteSpan,
}

/// Represents a container for declarations
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum DeclarationContainer {
    Module { module: ModuleRef },
    Class { cls: ClassRef },
    Function { func: FunctionRef },
}

/// Represents a containing top-level declaration
#[derive(Debug, Clone, Serialize)]
pub struct ContainingTopLevelDeclaration {
    pub declaration: Declaration,
    pub container: DeclarationContainer,
}

/// Represents a cross-reference via name
#[derive(Debug, Clone, Serialize)]
pub struct XRefViaName {
    pub target: Name,
    pub source: ByteSpan,
}

/// Represents cross-references via name by file
#[derive(Debug, Clone, Serialize)]
pub struct XRefsViaNameByFile {
    pub file: File,
    pub xrefs: Vec<XRefViaName>,
}

/// Represents file lines information
#[derive(Debug, Clone, Serialize)]
pub struct FileLines {
    pub file: File,
    pub lengths: Vec<u64>,
    #[serde(rename = "endsInNewline")]
    pub ends_in_newline: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    pub has_unicode_or_tabs: bool,
}

/// Represents the language of a file
#[derive(Debug, Clone, Serialize)]
pub struct FileLanguage {
    pub file: File,
    pub language: u8,
}

/// Represents a structured name
#[derive(Debug, Clone, Serialize)]
pub struct SName {
    pub local_name: Name,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<Box<SName>>,
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
    pub from_name: Name,
    pub into_module: ModuleRef,
}

/// Represents an import star location
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarLocation {
    pub import_star: ImportStarRef,
    pub file: File,
    pub span: ByteSpan,
}

/// Reference to an import star statement
#[derive(Debug, Clone, Serialize)]
pub struct ImportStarRef {
    pub id: u64,
    pub key: ImportStarStatement,
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
    pub declaration: ClassRef,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub bases: Vec<ClassRef>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub decorators: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<DeclarationContainer>,
}

/// Represents a function definition
#[derive(Debug, Clone, Serialize)]
pub struct FunctionDefinition {
    pub declaration: FunctionRef,
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
    pub declaration: VariableRef,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "typeInfo")]
    pub type_info: Option<TypeInfo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<DeclarationContainer>,
}

/// Represents a module definition
#[derive(Debug, Clone, Serialize)]
pub struct ModuleDefinition {
    pub module: ModuleRef,
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

/// Represents a type
#[derive(Debug, Clone, Serialize)]
pub struct Type {
    pub id: u64,
    pub key: String,
}

/// Represents a definition location
#[derive(Debug, Clone, Serialize)]
pub struct DefinitionLocation {
    pub definition: Definition,
    pub file: File,
    pub span: ByteSpan,
}

/// Represents a declaration docstring
#[derive(Debug, Clone, Serialize)]
pub struct DeclarationDocstring {
    pub declaration: Declaration,
    pub location: ByteSpan,
    pub pretty_text: String,
}

/// Represents a call argument
#[derive(Debug, Clone, Serialize)]
pub struct CallArgument {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<Name>,
    pub span: ByteSpan,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub argument: Option<Argument>,
}

/// Represents an argument
#[derive(Debug, Clone, Serialize)]
pub struct Argument {
    pub lit: StringLiteral,
}

/// Represents a string literal
#[derive(Debug, Clone, Serialize)]
pub struct StringLiteral {
    pub id: u64,
    pub key: String,
}

/// Represents a file call
#[derive(Debug, Clone, Serialize)]
pub struct FileCall {
    pub file: File,
    pub callee_span: ByteSpan,
    pub call_args: Vec<CallArgument>,
}

/// Represents a callee to caller relationship
#[derive(Debug, Clone, Serialize)]
pub struct CalleeToCaller {
    pub callee: Name,
    pub caller: Name,
}
