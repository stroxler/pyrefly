/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The scope prepass: which scope owns each name.
//!
//! The declarations come from the shared [`declarations`] walker, which Pyrefly's export
//! `Definitions` also consumes, but this sink answers a runtime question rather than a
//! type-binding one: Python decides ownership syntactically, so every expression position
//! is kept, no branch is pruned by platform or version checks, and a name bound only in
//! dead code is still local. Name lookup is the shared [`lexical`] resolution:
//! `global`/`nonlocal` redirect a name to the owning scope, and class scopes are invisible
//! to the scopes nested inside them.

use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::name::Name;
use ruff_python_ast::visitor::Visitor;
use ruff_python_ast::visitor::walk_expr;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::declarations;
use crate::declarations::ChildScope;
use crate::declarations::Declaration;
use crate::declarations::DeclarationSink;
use crate::ir::RegionId;
use crate::ir::Scope;
use crate::ir::ScopeId;
use crate::ir::ScopeKind;
use crate::ir::Symbol;
use crate::ir::SymbolId;
use crate::ir::SymbolOrigin;
use crate::lexical;
use crate::lexical::Category;
use crate::lexical::Probe;
use crate::lexical::Resolution;
use crate::lexical::ScopeChain;

/// The parameter through which a generator expression receives its first iterator,
/// named as CPython names it.
pub const GENERATOR_ARG: &str = ".0";

/// Declarations collected for one scope before symbols are created.
#[derive(Default)]
struct Declarations {
    bound: SmallMap<Name, SymbolOrigin>,
    globals: SmallSet<Name>,
    nonlocals: SmallSet<Name>,
}

/// The finished scope structure, consulted (and extended with builtins) by lowering.
pub struct ScopeTable {
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub num_regions: u32,
    /// Scopes introduced by `def`, `lambda`, `class` and comprehension nodes, by node range.
    by_node: SmallMap<TextRange, ScopeId>,
    /// Names each scope declared `global` or `nonlocal`.
    globals: Vec<SmallSet<Name>>,
    nonlocals: Vec<SmallSet<Name>>,
    builtins: ScopeId,
}

impl ScopeTable {
    pub fn new(body: &[Stmt], range: TextRange) -> Self {
        let mut builder = Builder {
            scopes: Vec::new(),
            decls: Vec::new(),
            by_node: SmallMap::new(),
            current: ScopeId(0),
            num_regions: 1,
        };
        builder.push(ScopeKind::Module, None, RegionId(0), range);
        declarations::walk_suite(body, &mut builder);
        let mut table = builder.finish();
        // Module and class bodies look names up dynamically (`LOAD_NAME`/`LOAD_GLOBAL`):
        // an unbound class-local name falls back to the module, and an unbound module
        // name falls back to builtins. Function locals have no fallback.
        for i in 0..table.symbols.len() {
            let symbol = &table.symbols[i];
            let name = symbol.name.clone();
            let fallback = match table.scopes[symbol.scope.index()].kind {
                ScopeKind::Class => match table.scopes[0].symbols.get(&name) {
                    Some(module) => Some(*module),
                    None => Some(table.builtin(&name)),
                },
                ScopeKind::Module => Some(table.builtin(&name)),
                ScopeKind::Function
                | ScopeKind::Lambda
                | ScopeKind::Comprehension
                | ScopeKind::Builtins => None,
            };
            table.symbols[i].fallback = fallback;
        }
        table
    }

    pub fn module(&self) -> ScopeId {
        ScopeId(0)
    }

    /// The scope introduced by the `def`, `lambda`, `class` or comprehension at `range`.
    pub fn scope_of_node(&self, range: TextRange) -> ScopeId {
        *self
            .by_node
            .get(&range)
            .expect("the prepass creates a scope for every scope-introducing node")
    }

    fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.index()]
    }

    /// Resolve `name` as code in `scope` sees it, creating a builtins symbol if no
    /// scope defines it. The flag is true for a scope error such as an unresolvable
    /// `nonlocal`, which resolves to a builtins symbol.
    pub fn resolve(&mut self, scope: ScopeId, name: &Name) -> (SymbolId, bool) {
        match lookup(&self.scopes, &self.globals, &self.nonlocals, scope, name) {
            Lookup::Found(symbol) => (symbol, false),
            Lookup::Builtin => (self.builtin(name), false),
            Lookup::ScopeError => (self.builtin(name), true),
        }
    }

    fn builtin(&mut self, name: &Name) -> SymbolId {
        if let Some(symbol) = self.scope(self.builtins).symbols.get(name) {
            return *symbol;
        }
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(Symbol {
            name: name.clone(),
            scope: self.builtins,
            origin: SymbolOrigin::Builtin,
            slot: None,
            captured: false,
            fallback: None,
        });
        self.scopes[self.builtins.index()]
            .symbols
            .insert(name.clone(), id);
        id
    }
}

enum Lookup {
    Found(SymbolId),
    Builtin,
    /// `nonlocal` with no enclosing function binding.
    ScopeError,
}

/// The scope tree as [`lexical`] navigates it.
#[derive(Clone, Copy)]
struct Tree<'s>(&'s [Scope]);

impl ScopeChain for Tree<'_> {
    type Id = ScopeId;
    type Barrier = ();

    fn parent(&self, id: ScopeId) -> Option<ScopeId> {
        self.0[id.index()].parent
    }

    fn category(&self, id: ScopeId) -> Category {
        match self.0[id.index()].kind {
            ScopeKind::Module => Category::Module,
            ScopeKind::Class => Category::Class,
            ScopeKind::Function | ScopeKind::Lambda => Category::Function,
            ScopeKind::Comprehension => Category::Comprehension,
            ScopeKind::Builtins => {
                unreachable!("lookup from the builtins scope is answered directly")
            }
        }
    }

    fn barrier(&self, _: ScopeId) {}

    fn join(_: (), _: ()) {}
}

/// Python's lexical lookup.
fn lookup(
    scopes: &[Scope],
    globals: &[SmallSet<Name>],
    nonlocals: &[SmallSet<Name>],
    scope: ScopeId,
    name: &Name,
) -> Lookup {
    let symbols = |id: ScopeId| scopes[id.index()].symbols.get(name).copied();
    // The builtins scope sees only itself.
    if scopes[scope.index()].kind == ScopeKind::Builtins {
        return symbols(scope).map_or(Lookup::Builtin, Lookup::Found);
    }
    // Only the module can both own a name and declare it, and there the declaration is
    // a no-op (`global`) or a syntax error (`nonlocal`).
    let probe = |id: ScopeId| match symbols(id) {
        Some(symbol) => Probe::Owned(symbol),
        None if globals[id.index()].contains(name) => Probe::Global,
        None if nonlocals[id.index()].contains(name) => Probe::Nonlocal,
        None => Probe::Absent,
    };
    match lexical::resolve(Tree(scopes), scope, probe) {
        Resolution::Found(symbol) => Lookup::Found(symbol),
        Resolution::Unbound => Lookup::Builtin,
        Resolution::Nonlocal(_) => Lookup::ScopeError,
    }
}

impl ScopeTable {
    /// The pieces [`crate::ir::Program`] keeps for public name resolution.
    pub fn into_parts(self) -> ScopeParts {
        ScopeParts {
            scopes: self.scopes,
            symbols: self.symbols,
            lexical: Lexical {
                globals: self.globals,
                nonlocals: self.nonlocals,
                builtins: self.builtins,
            },
        }
    }
}

pub struct ScopeParts {
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub lexical: Lexical,
}

/// The `global`/`nonlocal` declarations a finished program needs for name lookup.
pub struct Lexical {
    globals: Vec<SmallSet<Name>>,
    nonlocals: Vec<SmallSet<Name>>,
    builtins: ScopeId,
}

impl Lexical {
    /// Lookup over a finished program; `None` for a builtin that no code referenced.
    pub fn resolve(&self, scopes: &[Scope], scope: ScopeId, name: &Name) -> Option<SymbolId> {
        match lookup(scopes, &self.globals, &self.nonlocals, scope, name) {
            Lookup::Found(s) => Some(s),
            Lookup::Builtin | Lookup::ScopeError => {
                scopes[self.builtins.index()].symbols.get(name).copied()
            }
        }
    }
}

struct Builder {
    scopes: Vec<Scope>,
    decls: Vec<Declarations>,
    by_node: SmallMap<TextRange, ScopeId>,
    current: ScopeId,
    num_regions: u32,
}

impl Builder {
    fn push(
        &mut self,
        kind: ScopeKind,
        parent: Option<ScopeId>,
        region: RegionId,
        range: TextRange,
    ) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope {
            kind,
            parent,
            region,
            range,
            symbols: SmallMap::new(),
        });
        self.decls.push(Declarations::default());
        id
    }

    /// Create the scope for the node at `range`, with its own region if it is deferred.
    fn enter(&mut self, kind: ScopeKind, range: TextRange, deferred: bool) -> ScopeId {
        let region = if deferred {
            self.num_regions += 1;
            RegionId(self.num_regions - 1)
        } else {
            self.scopes[self.current.index()].region
        };
        let id = self.push(kind, Some(self.current), region, range);
        self.by_node.insert(range, id);
        id
    }

    fn bind(&mut self, name: &Name, origin: SymbolOrigin) {
        if name.is_empty() {
            return;
        }
        let bound = &mut self.decls[self.current.index()].bound;
        if !bound.contains_key(name) {
            bound.insert(name.clone(), origin);
        }
    }

    fn finish(mut self) -> ScopeTable {
        // `global x` anywhere creates the module symbol `x`.
        let declared: Vec<Name> = self.decls[1..]
            .iter()
            .flat_map(|d| d.globals.iter().cloned())
            .collect();
        for name in declared {
            let module = &mut self.decls[0].bound;
            if !module.contains_key(&name) {
                module.insert(name, SymbolOrigin::GlobalDeclaration);
            }
        }
        let builtins = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope {
            kind: ScopeKind::Builtins,
            parent: None,
            region: RegionId(0),
            range: TextRange::default(),
            symbols: SmallMap::new(),
        });
        self.scopes[0].parent = None;
        let mut symbols = Vec::new();
        let mut slots = vec![0u32; self.num_regions as usize];
        let mut globals = Vec::new();
        let mut nonlocals = Vec::new();
        for (i, decls) in self.decls.into_iter().enumerate() {
            let scope = &mut self.scopes[i];
            for (name, origin) in decls.bound {
                // In the module, `global x` is a no-op; elsewhere it removes ownership.
                if i > 0 && (decls.globals.contains(&name) || decls.nonlocals.contains(&name)) {
                    continue;
                }
                let slot = &mut slots[scope.region.index()];
                let id = SymbolId(symbols.len() as u32);
                symbols.push(Symbol {
                    name: name.clone(),
                    scope: ScopeId(i as u32),
                    origin,
                    slot: Some(*slot),
                    captured: false,
                    fallback: None,
                });
                *slot += 1;
                scope.symbols.insert(name, id);
            }
            globals.push(if i > 0 {
                decls.globals
            } else {
                SmallSet::new()
            });
            nonlocals.push(decls.nonlocals);
        }
        globals.push(SmallSet::new());
        nonlocals.push(SmallSet::new());
        ScopeTable {
            scopes: self.scopes,
            symbols,
            num_regions: self.num_regions,
            by_node: self.by_node,
            globals,
            nonlocals,
            builtins,
        }
    }
}

impl<'a> DeclarationSink<'a> for Builder {
    fn declare(&mut self, declaration: Declaration<'a>) {
        match declaration {
            Declaration::Import(a) => match &a.asname {
                Some(alias) => self.bind(&alias.id, SymbolOrigin::Bound),
                None => {
                    let root = a.name.id.split('.').next().unwrap_or_default();
                    self.bind(&Name::new(root), SymbolOrigin::Bound)
                }
            },
            Declaration::ImportFrom(x) => {
                // Star imports are reported as a gap by lowering.
                for a in x.names.iter().filter(|a| &a.name != "*") {
                    self.bind(
                        &a.asname.as_ref().unwrap_or(&a.name).id,
                        SymbolOrigin::Bound,
                    )
                }
            }
            Declaration::Function(x) => self.bind(&x.name.id, SymbolOrigin::Bound),
            Declaration::Class(x) => self.bind(&x.name.id, SymbolOrigin::Bound),
            Declaration::Assign(x, _)
            | Declaration::AugAssign(x, _)
            | Declaration::Annotated(x, _)
            | Declaration::Store(x)
            | Declaration::Delete(x)
            | Declaration::Walrus(x) => self.bind(&x.id, SymbolOrigin::Bound),
            Declaration::Handler(x) | Declaration::Capture(x) => {
                self.bind(&x.id, SymbolOrigin::Bound)
            }
            Declaration::Parameter(x) => self.bind(&x.id, SymbolOrigin::Parameter),
            Declaration::Global(x) => {
                self.decls[self.current.index()]
                    .globals
                    .insert(x.id.clone());
            }
            Declaration::Nonlocal(x) => {
                self.decls[self.current.index()]
                    .nonlocals
                    .insert(x.id.clone());
            }
        }
    }

    /// Functions, lambdas and generator expressions get their own region; other scopes run
    /// in the current region.
    fn child(&mut self, child: ChildScope<'a>) {
        let (kind, range, deferred) = match child {
            ChildScope::Function(x) => (ScopeKind::Function, x.range, true),
            ChildScope::Lambda(x) => (ScopeKind::Lambda, x.range, true),
            ChildScope::Class(x) => (ScopeKind::Class, x.range, false),
            ChildScope::Comprehension {
                range,
                is_generator,
                ..
            } => (ScopeKind::Comprehension, range, is_generator),
        };
        let scope = self.enter(kind, range, deferred);
        let outer = self.current;
        self.current = scope;
        if matches!(
            child,
            ChildScope::Comprehension {
                is_generator: true,
                ..
            }
        ) {
            // A generator expression receives its first iterator as parameter `.0`.
            self.bind(&Name::new_static(GENERATOR_ARG), SymbolOrigin::Parameter);
        }
        declarations::walk_child(child, self);
        self.current = outer;
    }
}

/// Expressions in `x` that would be evaluated at definition time if annotations were
/// eager, and that could have effects.
pub fn annotation_may_have_effects(x: &Expr) -> bool {
    struct Finder(bool);
    impl<'a> Visitor<'a> for Finder {
        fn visit_expr(&mut self, x: &'a Expr) {
            match x {
                Expr::Call(_)
                | Expr::Named(_)
                | Expr::Await(_)
                | Expr::Yield(_)
                | Expr::YieldFrom(_) => self.0 = true,
                _ => walk_expr(self, x),
            }
        }
    }
    let mut f = Finder(false);
    f.visit_expr(x);
    f.0
}
