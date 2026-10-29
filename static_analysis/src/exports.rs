/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The interface through which domains resolve imported names across modules.

use pyrefly_python::module_name::ModuleName;
use ruff_python_ast::name::Name;

/// Where an imported name ultimately comes from.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExportTarget {
    /// A definition `name` in module `module`, after following re-exports.
    Definition(ModuleName, Name),
    /// The name is a module (a submodule import, or a re-exported module alias).
    Module(ModuleName),
    /// The provider cannot resolve the name (missing module, dynamic `__getattr__`, cycle).
    Unknown,
}

/// Resolves `from module import name` across modules.
pub trait ExportProvider {
    fn resolve(&self, module: ModuleName, name: &Name) -> ExportTarget;
}

/// A provider with no cross-module knowledge: `module.name` is taken at face value.
pub struct SyntacticExports;

impl ExportProvider for SyntacticExports {
    fn resolve(&self, module: ModuleName, name: &Name) -> ExportTarget {
        ExportTarget::Definition(module, name.clone())
    }
}
