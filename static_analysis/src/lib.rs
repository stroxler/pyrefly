/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! An experimental, domain-neutral Python static-analysis frontend and engine.
//!
//! [`analyze_python`] parses a module, resolves lexical scopes, and lowers the code into
//! a [`Program`] of regions whose control-flow graphs contain operations in runtime
//! evaluation order. An [`Engine`] runs any [`Domain`] over a region; [`History`]
//! answers reaching-definition queries without a domain.
//!
//! # Supported semantics
//!
//! - Scopes follow CPython's syntactic rules: any binding (including in dead code)
//!   makes a name local; `global`/`nonlocal` redirect to the owning scope; class scopes
//!   are invisible to nested functions and comprehensions; comprehension targets are
//!   comprehension-local while a walrus binds in the enclosing non-comprehension scope;
//!   unbound class-body names fall back to the module and unbound module names to
//!   builtins ([`Symbol::fallback`]).
//! - Evaluation order: assignment values before targets (targets left to right, each
//!   evaluating its object and index before storing); augmented assignment reads the
//!   target, then the value, then stores; the callee, then positional and starred
//!   arguments, then keyword and `**` arguments (also for class bases);
//!   decorators, then defaults, then function creation, then decorator application
//!   bottom-up; class bases before the inline class body; the first iterable of a
//!   comprehension in the enclosing scope.
//! - Control flow: `if`/`elif`/`else`, `while`/`for` with `else`, `break` and
//!   `continue`, `and`/`or`, conditional expressions and chained comparisons as explicit
//!   branches; `return`; `raise` and `assert`; `with` with `ContextExit` on fallthrough
//!   and on every `return`/`break`/`continue`/`raise` leaving it; `finally` on every
//!   normal exit.
//! - Regions: functions, lambdas and generator expressions are deferred regions with a
//!   [`Trigger`]; class bodies and list/set/dict comprehensions run inline.
//!
//! # Approximations, reported as [`GapKind`] operations
//!
//! - `try`/`except` (including `except*`): an exception may arise at any statement
//!   boundary inside the body, not mid-statement; handler matching is a
//!   nondeterministic choice; the bound exception value is unknown.
//! - `match`: case selection is a nondeterministic choice tried in order; captures are
//!   unknown values; value patterns are not evaluated.
//! - Star imports, invalid relative imports, PEP 695 type parameters and `type`
//!   statements, annotations with possible side effects (annotations are never
//!   evaluated), unresolvable `nonlocal`, syntax errors and IPython escapes.
//!
//! # Not modeled
//!
//! - Exceptions raised outside `try` simply leave the region; the engine reports only
//!   explicit `raise`/`assert` exits in [`RegionResult::raised`].
//! - A context manager that suppresses exceptions; `assert` removal under `-O`.
//! - Dynamic name definitions (`globals()`, `exec`, `setattr` on modules) and
//!   interleaving at suspension points: these are domain concerns.

pub mod engine;
pub mod exports;
pub mod history;
pub mod ir;
mod lower;
mod scope;

pub use engine::Binding;
pub use engine::Convergence;
pub use engine::Cx;
pub use engine::Domain;
pub use engine::Engine;
pub use engine::Entry;
pub use engine::Frame;
pub use engine::Limits;
pub use engine::Presence;
pub use engine::RaiseSite;
pub use engine::RegionResult;
pub use engine::load;
pub use exports::ExportProvider;
pub use exports::ExportTarget;
pub use exports::SyntacticExports;
pub use history::Def;
pub use history::History;
pub use ir::*;
pub use pyrefly_python::module_name::ModuleName;
pub use ruff_python_ast::CmpOp;
pub use ruff_python_ast::Operator;
pub use ruff_python_ast::UnaryOp;
pub use ruff_python_ast::name::Name;
pub use ruff_text_size::TextRange;

/// Parse and lower one module. `is_init` says whether the module is a package
/// `__init__`, which affects relative imports. Syntax errors are recorded in
/// [`Program::syntax_errors`] and as a [`GapKind::SyntaxError`] at module entry.
pub fn analyze_python(source: &str, module: ModuleName, is_init: bool) -> Program {
    lower::lower(source, module, is_init)
}
