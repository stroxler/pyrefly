/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The scope prepass: which scope owns each name.
//!
//! The binding collection follows Pyrefly's export `Definitions` builder (assignment,
//! import, `def`/`class`, `del`, `for`/`with`/`except` targets, match captures, and the
//! PEP 572 rule that a walrus in a comprehension binds in the enclosing scope), but it
//! answers a runtime question rather than a type-binding one: Python decides ownership
//! syntactically, so no branch is pruned by platform or version checks, and a name bound
//! only in dead code is still local. `global`/`nonlocal` redirect a name to the owning
//! scope, and class scopes are invisible to the scopes nested inside them.

use pyrefly_python::ast::Ast;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprContext;
use ruff_python_ast::Parameters;
use ruff_python_ast::Stmt;
use ruff_python_ast::name::Name;
use ruff_python_ast::visitor::Visitor;
use ruff_python_ast::visitor::walk_expr;
use ruff_python_ast::visitor::walk_stmt;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::ir::RegionId;
use crate::ir::Scope;
use crate::ir::ScopeId;
use crate::ir::ScopeKind;
use crate::ir::Symbol;
use crate::ir::SymbolId;
use crate::ir::SymbolOrigin;

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
        builder.visit_body(body);
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

/// Python's lexical lookup.
fn lookup(
    scopes: &[Scope],
    globals: &[SmallSet<Name>],
    nonlocals: &[SmallSet<Name>],
    scope: ScopeId,
    name: &Name,
) -> Lookup {
    let module = ScopeId(0);
    let in_module = || match scopes[module.index()].symbols.get(name) {
        Some(s) => Lookup::Found(*s),
        None => Lookup::Builtin,
    };
    let mut current = scope;
    let mut is_origin = true;
    loop {
        let s = &scopes[current.index()];
        // A class scope is visible only to its own body.
        if is_origin || s.kind != ScopeKind::Class {
            if globals[current.index()].contains(name) {
                return in_module();
            }
            if nonlocals[current.index()].contains(name) {
                // `nonlocal` skips the module: find the nearest enclosing function binding.
                let mut p = s.parent;
                while let Some(id) = p {
                    let ps = &scopes[id.index()];
                    if ps.kind != ScopeKind::Class && ps.kind != ScopeKind::Module {
                        if let Some(symbol) = ps.symbols.get(name) {
                            return Lookup::Found(*symbol);
                        }
                        if nonlocals[id.index()].contains(name) {
                            return lookup(scopes, globals, nonlocals, id, name);
                        }
                    }
                    p = ps.parent;
                }
                return Lookup::ScopeError;
            }
            if let Some(symbol) = s.symbols.get(name) {
                return Lookup::Found(*symbol);
            }
        }
        match s.parent {
            Some(p) if scopes[p.index()].kind != ScopeKind::Builtins => {
                current = p;
                is_origin = false;
            }
            _ => return in_module(),
        }
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

    /// Enter a new scope for the node at `range`. Functions, lambdas and generator
    /// expressions get their own region; other scopes run in the current region.
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

    fn in_scope(&mut self, scope: ScopeId, f: impl FnOnce(&mut Self)) {
        let outer = self.current;
        self.current = scope;
        f(self);
        self.current = outer;
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

    /// A walrus binds in the nearest enclosing scope that is not a comprehension.
    fn bind_walrus(&mut self, name: &Name) {
        let mut scope = self.current;
        while self.scopes[scope.index()].kind == ScopeKind::Comprehension {
            scope = self.scopes[scope.index()]
                .parent
                .expect("a comprehension always has a parent scope");
        }
        let bound = &mut self.decls[scope.index()].bound;
        if !bound.contains_key(name) {
            bound.insert(name.clone(), SymbolOrigin::Bound);
        }
    }

    /// Defaults are evaluated in the current scope; parameters bind in `scope`.
    fn parameters(&mut self, parameters: &Parameters, scope: ScopeId) {
        for p in parameters.iter_non_variadic_params() {
            if let Some(default) = &p.default {
                self.visit_expr(default);
            }
        }
        self.in_scope(scope, |b| {
            for p in parameters.iter() {
                b.bind(&p.name().id, SymbolOrigin::Parameter);
            }
        });
    }

    fn comprehension(
        &mut self,
        range: TextRange,
        generators: &[Comprehension],
        deferred: bool,
        elts: &[&Expr],
    ) {
        // The first iterable is evaluated in the enclosing scope.
        self.visit_expr(&generators[0].iter);
        let scope = self.enter(ScopeKind::Comprehension, range, deferred);
        self.in_scope(scope, |b| {
            if deferred {
                // A generator expression receives its first iterator as parameter `.0`.
                b.bind(&Name::new_static(GENERATOR_ARG), SymbolOrigin::Parameter);
            }
            for (i, g) in generators.iter().enumerate() {
                if i > 0 {
                    b.visit_expr(&g.iter);
                }
                b.visit_expr(&g.target);
                for cond in &g.ifs {
                    b.visit_expr(cond);
                }
            }
            for e in elts {
                b.visit_expr(e);
            }
        });
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

impl<'a> Visitor<'a> for Builder {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        match stmt {
            Stmt::FunctionDef(f) => {
                for d in &f.decorator_list {
                    self.visit_expr(&d.expression);
                }
                let scope = self.enter(ScopeKind::Function, f.range, true);
                self.parameters(&f.parameters, scope);
                self.bind(&f.name.id, SymbolOrigin::Bound);
                self.in_scope(scope, |b| b.visit_body(&f.body));
            }
            Stmt::ClassDef(c) => {
                for d in &c.decorator_list {
                    self.visit_expr(&d.expression);
                }
                if let Some(arguments) = &c.arguments {
                    self.visit_arguments(arguments);
                }
                self.bind(&c.name.id, SymbolOrigin::Bound);
                let scope = self.enter(ScopeKind::Class, c.range, false);
                self.in_scope(scope, |b| b.visit_body(&c.body));
            }
            Stmt::Import(x) => {
                for a in &x.names {
                    match &a.asname {
                        Some(alias) => self.bind(&alias.id, SymbolOrigin::Bound),
                        None => {
                            let root = a.name.id.split('.').next().unwrap_or_default();
                            self.bind(&Name::new(root), SymbolOrigin::Bound)
                        }
                    }
                }
            }
            Stmt::ImportFrom(x) => {
                for a in &x.names {
                    if &a.name != "*" {
                        self.bind(
                            &a.asname.as_ref().unwrap_or(&a.name).id,
                            SymbolOrigin::Bound,
                        );
                    }
                }
            }
            Stmt::Global(x) => {
                for n in &x.names {
                    self.decls[self.current.index()]
                        .globals
                        .insert(n.id.clone());
                }
            }
            Stmt::Nonlocal(x) => {
                for n in &x.names {
                    self.decls[self.current.index()]
                        .nonlocals
                        .insert(n.id.clone());
                }
            }
            Stmt::Try(x) => {
                for ruff_python_ast::ExceptHandler::ExceptHandler(h) in &x.handlers {
                    if let Some(name) = &h.name {
                        self.bind(&name.id, SymbolOrigin::Bound);
                    }
                }
                walk_stmt(self, stmt);
            }
            Stmt::Match(x) => {
                for case in &x.cases {
                    Ast::pattern_lvalue(&case.pattern, &mut |id| {
                        self.bind(&id.id, SymbolOrigin::Bound)
                    });
                }
                walk_stmt(self, stmt);
            }
            // The value of a `type` alias is lazily evaluated in its own scope.
            Stmt::TypeAlias(x) => self.visit_expr(&x.name),
            _ => walk_stmt(self, stmt),
        }
    }

    /// Annotations are never evaluated, so they bind nothing.
    fn visit_annotation(&mut self, _: &'a Expr) {}

    fn visit_expr(&mut self, x: &'a Expr) {
        match x {
            Expr::Name(n) => {
                if matches!(n.ctx, ExprContext::Store | ExprContext::Del) {
                    self.bind(&n.id, SymbolOrigin::Bound);
                }
            }
            Expr::Named(n) => {
                self.visit_expr(&n.value);
                if let Expr::Name(target) = &*n.target {
                    self.bind_walrus(&target.id);
                }
            }
            Expr::Lambda(l) => {
                let scope = self.enter(ScopeKind::Lambda, l.range, true);
                if let Some(parameters) = &l.parameters {
                    self.parameters(parameters, scope);
                }
                self.in_scope(scope, |b| b.visit_expr(&l.body));
            }
            Expr::ListComp(c) => self.comprehension(c.range, &c.generators, false, &[&c.elt]),
            Expr::SetComp(c) => self.comprehension(c.range, &c.generators, false, &[&c.elt]),
            Expr::DictComp(c) => {
                let elts: Vec<&Expr> = c.key.as_deref().into_iter().chain([&*c.value]).collect();
                self.comprehension(c.range, &c.generators, false, &elts)
            }
            Expr::Generator(c) => self.comprehension(c.range, &c.generators, true, &[&c.elt]),
            _ => walk_expr(self, x),
        }
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
