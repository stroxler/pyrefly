/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! An experimental lazy-import safety analysis in the style of
//! [Lifeguard](https://github.com/facebook/Lifeguard), built as a consumer of the neutral
//! `pyrefly_static_analysis` frontend and engine.
//!
//! A module is safe to import lazily when executing its body has no effect that another
//! module could observe: it does not mutate imported objects or other modules' state,
//! does not call functions with such effects, and does not depend on import order
//! through `sys.modules`, `exec` or `builtins.__import__`.
//!
//! The analysis never walks syntax. Each module is lowered once by the shared frontend,
//! and an abstract interpretation ([`domain`]) runs over the module region with the
//! shared [`Engine`](pyrefly_static_analysis::Engine). A call to a project function runs
//! the same engine over the callee's region, so function bodies count only when called,
//! while decorators and defaults count where the function is defined. Anything the
//! analysis cannot resolve is reported with an unknown kind instead of being treated as
//! safe. See `README.md` for the supported subset and its provenance.

mod analyzer;
mod domain;
mod known;
mod value;

use std::fmt;

use pyrefly_static_analysis::ExportProvider;
use pyrefly_static_analysis::ModuleName;

use crate::analyzer::Analyzer;

/// What a diagnostic reports.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    /// Import-time mutation of an object owned by another module (attribute, item,
    /// deletion, a mutating method, or rebinding another module's global).
    ImportedMutation,
    /// An imported object passed to a parameter that the callee mutates.
    ImportedArgument,
    /// A call whose body has an import-time effect.
    UnsafeCall,
    /// A call to a builtin or library function with process-wide effects.
    ProhibitedCall,
    /// A `raise` that import-time execution reaches.
    UnhandledRaise,
    /// A class defining `__del__`, which runs at an unpredictable time.
    CustomFinalizer,
    /// `exec`, or `eval` of a computed string.
    ExecCall,
    /// An access to `sys.modules` that depends on which modules were already imported.
    SysModulesAccess,
    /// A store to or deletion of `builtins.__import__`.
    BuiltinsImportOverride,
    /// A call whose callee, or some operation in it, cannot be resolved.
    UnknownCall,
    /// A mutation of a value whose owner cannot be determined.
    UnknownMutation,
    /// A construct the frontend or the engine does not model, or a non-converged analysis.
    Unsupported,
}

impl Kind {
    pub fn as_str(self) -> &'static str {
        match self {
            Kind::ImportedMutation => "imported-mutation",
            Kind::ImportedArgument => "imported-argument",
            Kind::UnsafeCall => "unsafe-call",
            Kind::ProhibitedCall => "prohibited-call",
            Kind::UnhandledRaise => "unhandled-raise",
            Kind::CustomFinalizer => "custom-finalizer",
            Kind::ExecCall => "exec-call",
            Kind::SysModulesAccess => "sys-modules-access",
            Kind::BuiltinsImportOverride => "builtins-import-override",
            Kind::UnknownCall => "unknown-call",
            Kind::UnknownMutation => "unknown-mutation",
            Kind::Unsupported => "unsupported",
        }
    }

    /// True if the diagnostic is an absence of knowledge rather than a known effect.
    pub fn is_unknown(self) -> bool {
        matches!(
            self,
            Kind::UnknownCall | Kind::UnknownMutation | Kind::Unsupported
        )
    }

    /// True if the module's own imports must run eagerly. These are reported wherever
    /// they occur in the module, including in functions that import time never calls.
    pub fn forces_eager_imports(self) -> bool {
        matches!(
            self,
            Kind::CustomFinalizer
                | Kind::ExecCall
                | Kind::SysModulesAccess
                | Kind::BuiltinsImportOverride
        )
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    /// One-based line and column of the operation.
    pub line: usize,
    pub column: usize,
    pub kind: Kind,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    /// No diagnostics: importing the module lazily is safe within the supported subset.
    Safe,
    /// Some known import-time effect.
    Unsafe,
    /// No known effect, but something could not be analyzed.
    Unknown,
}

impl Verdict {
    pub fn as_str(self) -> &'static str {
        match self {
            Verdict::Safe => "safe",
            Verdict::Unsafe => "unsafe",
            Verdict::Unknown => "unknown",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleReport {
    pub name: ModuleName,
    pub diagnostics: Vec<Diagnostic>,
}

impl ModuleReport {
    pub fn verdict(&self) -> Verdict {
        if self.diagnostics.iter().any(|d| !d.kind.is_unknown()) {
            Verdict::Unsafe
        } else if self.diagnostics.is_empty() {
            Verdict::Safe
        } else {
            Verdict::Unknown
        }
    }

    pub fn load_imports_eagerly(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.kind.forces_eager_imports())
    }
}

/// A set of in-memory modules analyzed together. Imports of modules outside the set
/// are treated as external objects.
#[derive(Default)]
pub struct Project {
    modules: Vec<(ModuleName, String, bool)>,
}

impl Project {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a module. `is_init` marks a package `__init__`, which affects relative imports.
    pub fn add(&mut self, name: &str, source: &str, is_init: bool) -> &mut Self {
        self.modules
            .push((ModuleName::from_str(name), source.to_owned(), is_init));
        self
    }

    /// Analyze every module. `exports` resolves `from m import n` across modules; with
    /// `SyntacticExports`, re-exports are still followed through the analyzed modules'
    /// own import statements. Reports are sorted by module name, and diagnostics by
    /// position.
    pub fn analyze(&self, exports: &dyn ExportProvider) -> Vec<ModuleReport> {
        Analyzer::new(&self.modules, exports).run()
    }
}
