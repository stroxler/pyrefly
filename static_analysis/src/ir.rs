/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The neutral intermediate representation produced by lowering.
//!
//! A [`Program`] is one Python module. Its code is split into [`Region`]s: the module
//! body, plus one region per function, lambda and generator expression. A region is a
//! control-flow graph of [`Block`]s; each block holds [`Op`]s in Python's runtime
//! evaluation order and ends in a [`Terminator`]. Operations communicate through
//! region-local temporaries ([`Reg`]) and through symbols ([`SymbolId`]) that the
//! scope prepass resolved with Python's lexical rules.
//!
//! Class bodies and list/set/dict comprehensions execute immediately, so they are
//! lowered inline into the enclosing region and only contribute their own scope.

use std::collections::BTreeSet;
use std::fmt;
use std::sync::OnceLock;

use pyrefly_python::module_name::ModuleName;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Operator;
use ruff_python_ast::UnaryOp;
use ruff_python_ast::name::Name;
use ruff_source_file::LineIndex;
use ruff_source_file::OneIndexed;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::scope::Lexical;

macro_rules! index_type {
    ($(#[$attr:meta])* $name:ident) => {
        $(#[$attr])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);

        impl $name {
            pub fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

index_type!(
    /// A lexical scope. Stable for a given source text.
    ScopeId
);
index_type!(
    /// A symbol: one name owned by one scope. Stable for a given source text.
    SymbolId
);
index_type!(
    /// A code region with its own control-flow graph.
    RegionId
);
index_type!(
    /// An operation, unique across the whole program. Stable for a given source text.
    OpId
);
index_type!(
    /// A basic block, local to its region.
    BlockId
);
index_type!(
    /// A temporary value, local to its region. A temporary may be assigned on several
    /// branches that later join (for example both arms of `a if c else b`).
    Reg
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Module,
    Class,
    Function,
    Lambda,
    /// A list/set/dict comprehension (inline) or a generator expression (its own region).
    Comprehension,
    /// The implicit outermost scope that holds names no enclosing scope defines.
    Builtins,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    /// The region whose frame stores this scope's symbols. Class and inline
    /// comprehension scopes share the frame of the region they execute in. The
    /// builtins scope is attributed to the module region but owns no frame slots.
    pub region: RegionId,
    pub range: TextRange,
    pub symbols: SmallMap<Name, SymbolId>,
}

/// How a scope came to own a symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolOrigin {
    /// Some statement in the scope binds the name (assignment, import, `def`, `del`, ...),
    /// whether or not that statement is reachable.
    Bound,
    /// A parameter of a function or lambda scope.
    Parameter,
    /// A module symbol created only because a nested scope declared it `global`.
    GlobalDeclaration,
    /// A builtin, or a name that no scope defines (a `NameError` at runtime unless a
    /// dynamic definition such as a star import provides it).
    Builtin,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Name,
    pub scope: ScopeId,
    pub origin: SymbolOrigin,
    /// The slot in the owning region's frame; `None` for builtins.
    pub slot: Option<u32>,
    /// True if a different region reads or writes the symbol (a closure cell, or a
    /// module global used from a function).
    pub captured: bool,
    /// What a load reads while this symbol is unbound. A class-body name falls back to
    /// the module symbol of the same name (or the builtin), and a module name falls back
    /// to the builtin, following CPython's `LOAD_NAME`/`LOAD_GLOBAL`. Function locals
    /// have none: reading them unbound raises `UnboundLocalError`.
    pub fallback: Option<SymbolId>,
}

/// What a region's code runs in response to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Trigger {
    /// The module body runs when the module is imported.
    OnImport,
    /// A plain function or lambda body runs when the function object is called.
    OnCall,
    /// A generator (function containing `yield`, or a generator expression) runs
    /// piecewise each time the generator is advanced, not when it is called.
    OnIteration,
    /// A coroutine (`async def`) runs when it is awaited or scheduled, not when called.
    OnAwait,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamKind {
    PositionalOnly,
    Positional,
    VarArgs,
    KeywordOnly,
    KwArgs,
}

/// A neutral, unevaluated description of an annotation or other static expression.
/// Names are resolved in the scope that would evaluate the expression.
#[derive(Debug, Clone, PartialEq)]
pub enum StaticExpr {
    Name(Name, SymbolId),
    Attribute(Box<StaticExpr>, Name),
    /// `base[a, b]`; a tuple index is flattened into its elements.
    Subscript(Box<StaticExpr>, Vec<StaticExpr>),
    Literal(Literal),
    /// `[a, b]` or `(a, b)` outside a subscript.
    Sequence(Vec<StaticExpr>),
    /// Anything else (calls, operators, ...), identified by its source range.
    Other(TextRange),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Name,
    pub symbol: SymbolId,
    pub kind: ParamKind,
    pub annotation: Option<StaticExpr>,
    pub has_default: bool,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    None,
    Bool(bool),
    /// `None` when the integer does not fit in an `i64`.
    Int(Option<i64>),
    Float(f64),
    Complex,
    Str(Box<str>),
    Bytes,
    Ellipsis,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuildKind {
    Tuple,
    List,
    Set,
    Dict,
    Slice,
    /// An f-string or t-string; the elements are the interpolated values in order.
    String,
}

/// One element of a built collection or comprehension accumulation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Element {
    Value(Reg),
    /// `*x` inside a tuple, list or set display.
    Star(Reg),
    /// `key: value` inside a dict display.
    Pair(Reg, Reg),
    /// `**m` inside a dict display.
    DoubleStar(Reg),
    /// An omitted slice bound, as in `a[:n]`.
    Missing,
}

/// One argument of a call or class definition. Arguments are listed in evaluation
/// order: positional and starred arguments, then keyword and `**` arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Positional(Reg),
    Star(Reg),
    Keyword(Name, Reg),
    DoubleStar(Reg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Arith(Operator),
    /// The in-place form, as performed by augmented assignment.
    InPlace(Operator),
    Compare(CmpOp),
}

/// What an operation does to a name, attribute or subscript location.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    /// Read the location into a temporary.
    Load(Reg),
    /// Write a temporary into the location.
    Store(Reg),
    Delete,
}

/// What an import statement binds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportItem {
    /// `import a.b.c` loads `a.b.c` and binds the top-level package `a`.
    Root,
    /// `import a.b.c as x` binds the module `a.b.c` itself.
    Module,
    /// `from m import name [as x]` binds `m.name` (an attribute or submodule).
    Name(Name),
}

/// A construct whose semantics the frontend approximates or does not model. A
/// reachable gap means results for that region are not a complete safety claim.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum GapKind {
    SyntaxError,
    /// `try`: exceptions are modeled only at statement boundaries inside the body,
    /// and `finally` on the exceptional path is approximated.
    Exceptions,
    /// `match`: case selection is a nondeterministic choice; captures are unknown.
    Match,
    /// `from m import *` defines names that the scope prepass cannot see.
    StarImport,
    /// A relative import that goes above the top-level package.
    InvalidRelativeImport,
    /// PEP 695 type parameters and `type` statements.
    TypeParams,
    /// An annotation with possible side effects; annotations are never evaluated.
    AnnotationEvaluation,
    /// `nonlocal` with no enclosing function binding, or a similar scope error.
    ScopeError,
    /// IPython escape commands.
    IpyEscape,
    /// A region needed more copies of `finally` bodies than the lowering budget allows,
    /// and this path skips the `finally` body. Every duplicated exit counts toward the
    /// budget: the normal path, the exception path, and each `return`, `break` and
    /// `continue` leaving the `try`. Because copies multiply with nesting, moderate
    /// nesting (about ten levels) can reach the budget.
    FinallyLimit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpKind {
    /// Access a variable. Loads of a symbol owned by another region read that
    /// region's current binding (closure cell or module global).
    Name {
        symbol: SymbolId,
        access: Access,
    },
    Attribute {
        object: Reg,
        attr: Name,
        access: Access,
    },
    Subscript {
        object: Reg,
        index: Reg,
        access: Access,
    },
    Literal {
        value: Literal,
        dst: Reg,
    },
    Build {
        kind: BuildKind,
        elements: Vec<Element>,
        dst: Reg,
    },
    /// Append to a collection under construction by a comprehension.
    Append {
        collection: Reg,
        element: Element,
    },
    Call {
        func: Reg,
        args: Vec<Arg>,
        dst: Reg,
    },
    Unary {
        op: UnaryOp,
        operand: Reg,
        dst: Reg,
    },
    Binary {
        op: BinaryOp,
        left: Reg,
        right: Reg,
        dst: Reg,
    },
    /// Destructure an iterable into `dsts`; `star` is the index of a starred target.
    Unpack {
        value: Reg,
        dsts: Vec<Reg>,
        star: Option<usize>,
    },
    /// `iter(iterable)` (or `aiter` when `is_async`).
    Iter {
        iterable: Reg,
        dst: Reg,
        is_async: bool,
    },
    /// The next item of an iterator. Exhaustion is a separate `Choice` edge.
    Next {
        iterator: Reg,
        dst: Reg,
        is_async: bool,
    },
    /// The value of a parameter at region entry.
    Param {
        index: usize,
        dst: Reg,
    },
    /// Create a function object for a deferred region. `defaults` are already evaluated
    /// in the enclosing region, in parameter order. Decorators are applied by later
    /// `Call` operations.
    MakeFunction {
        region: RegionId,
        defaults: Vec<Reg>,
        dst: Reg,
    },
    /// Create a class after its body (lowered inline, before this op) has run.
    MakeClass {
        name: Name,
        scope: ScopeId,
        bases: Vec<Arg>,
        dst: Reg,
    },
    /// Execute an import. `module` is absolute.
    Import {
        module: ModuleName,
        item: ImportItem,
        dst: Reg,
    },
    /// `__enter__` (or `__aenter__`). `context` identifies the `with` item; the
    /// matching `ContextExit` on every normal exit path carries the same identity.
    ContextEnter {
        manager: Reg,
        dst: Reg,
        context: OpId,
        is_async: bool,
    },
    ContextExit {
        manager: Reg,
        context: OpId,
        is_async: bool,
    },
    /// A suspension point. The resumed value is unknown to the frontend.
    Await {
        value: Reg,
        dst: Reg,
    },
    Yield {
        value: Option<Reg>,
        dst: Reg,
        from: bool,
    },
    /// An approximated or unmodeled construct. `dst`, if any, receives the unknown value
    /// the construct produces (a match capture, an exception bound by `except ... as`).
    Gap {
        kind: GapKind,
        dst: Option<Reg>,
    },
}

impl OpKind {
    /// The temporaries this operation reads, in evaluation order.
    pub fn reads(&self) -> Vec<Reg> {
        fn element(e: &Element, out: &mut Vec<Reg>) {
            match *e {
                Element::Value(r) | Element::Star(r) | Element::DoubleStar(r) => out.push(r),
                Element::Pair(k, v) => out.extend([k, v]),
                Element::Missing => {}
            }
        }
        fn arg(a: &Arg) -> Reg {
            match a {
                Arg::Positional(r) | Arg::Star(r) | Arg::Keyword(_, r) | Arg::DoubleStar(r) => *r,
            }
        }
        let stored = |access: &Access| match access {
            Access::Store(r) => Some(*r),
            Access::Load(_) | Access::Delete => None,
        };
        let mut out = Vec::new();
        match self {
            OpKind::Name { access, .. } => out.extend(stored(access)),
            OpKind::Attribute { object, access, .. } => {
                out.push(*object);
                out.extend(stored(access));
            }
            OpKind::Subscript {
                object,
                index,
                access,
            } => {
                out.extend([*object, *index]);
                out.extend(stored(access));
            }
            OpKind::Build { elements, .. } => elements.iter().for_each(|e| element(e, &mut out)),
            OpKind::Append {
                collection,
                element: e,
            } => {
                out.push(*collection);
                element(e, &mut out);
            }
            OpKind::Call { func, args, .. } => {
                out.push(*func);
                out.extend(args.iter().map(arg));
            }
            OpKind::Unary { operand, .. } => out.push(*operand),
            OpKind::Binary { left, right, .. } => out.extend([*left, *right]),
            OpKind::Unpack { value, .. } => out.push(*value),
            OpKind::Iter { iterable, .. } => out.push(*iterable),
            OpKind::Next { iterator, .. } => out.push(*iterator),
            OpKind::MakeFunction { defaults, .. } => out.extend(defaults),
            OpKind::MakeClass { bases, .. } => out.extend(bases.iter().map(arg)),
            OpKind::ContextEnter { manager, .. } | OpKind::ContextExit { manager, .. } => {
                out.push(*manager)
            }
            OpKind::Await { value, .. } => out.push(*value),
            OpKind::Yield { value, .. } => out.extend(value),
            OpKind::Literal { .. }
            | OpKind::Param { .. }
            | OpKind::Import { .. }
            | OpKind::Gap { .. } => {}
        }
        out
    }

    /// The temporaries this operation defines.
    pub fn writes(&self) -> Vec<Reg> {
        match self {
            OpKind::Name { access, .. }
            | OpKind::Attribute { access, .. }
            | OpKind::Subscript { access, .. } => match access {
                Access::Load(r) => vec![*r],
                Access::Store(_) | Access::Delete => Vec::new(),
            },
            OpKind::Unpack { dsts, .. } => dsts.clone(),
            OpKind::Literal { dst, .. }
            | OpKind::Build { dst, .. }
            | OpKind::Call { dst, .. }
            | OpKind::Unary { dst, .. }
            | OpKind::Binary { dst, .. }
            | OpKind::Iter { dst, .. }
            | OpKind::Next { dst, .. }
            | OpKind::Param { dst, .. }
            | OpKind::MakeFunction { dst, .. }
            | OpKind::MakeClass { dst, .. }
            | OpKind::Import { dst, .. }
            | OpKind::ContextEnter { dst, .. }
            | OpKind::Await { dst, .. }
            | OpKind::Yield { dst, .. } => vec![*dst],
            OpKind::Gap { dst, .. } => dst.iter().copied().collect(),
            OpKind::Append { .. } | OpKind::ContextExit { .. } => Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Op {
    pub kind: OpKind,
    pub region: RegionId,
    /// The lexical scope the operation is evaluated in (an inline class or
    /// comprehension scope, or the region's own scope).
    pub scope: ScopeId,
    pub block: BlockId,
    /// The source range of the expression or statement fragment that performs the op.
    pub range: TextRange,
    /// The range of the innermost enclosing statement: a statement-level view of the CFG.
    pub stmt: TextRange,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terminator {
    Jump(BlockId),
    /// Branch on the truthiness of `cond`.
    Branch {
        cond: Reg,
        then: BlockId,
        orelse: BlockId,
    },
    /// A choice the frontend cannot decide (loop exhaustion, exception edges, match
    /// cases). Every target is feasible.
    Choice(Vec<BlockId>),
    /// Leave the region normally. Function returns always carry a value (a bare
    /// `return` loads `None`); the end of the module carries none.
    Return(Option<Reg>),
    /// Leave the region by raising.
    Raise {
        /// The exception value, if the source names one.
        exc: Option<Reg>,
        cause: RaiseCause,
        /// The `raise` or `assert` statement, or the `try` statement for a re-raise.
        range: TextRange,
    },
}

impl Terminator {
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Jump(t) => vec![*t],
            Terminator::Branch { then, orelse, .. } => vec![*then, *orelse],
            Terminator::Choice(ts) => ts.clone(),
            Terminator::Return(_) | Terminator::Raise { .. } => Vec::new(),
        }
    }
}

/// Why a region is left by raising.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RaiseCause {
    /// A `raise` statement.
    Explicit,
    /// A failing `assert`.
    AssertFailure,
    /// An exception that reached a `try` and propagates onward: no handler matched,
    /// or it is leaving through `finally`.
    Reraise,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub ops: Vec<OpId>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct Region {
    pub name: Name,
    pub trigger: Trigger,
    pub scope: ScopeId,
    /// The region that creates this one, and the first `MakeFunction` op that does so.
    /// A definition inside a `finally` body is lowered once per exit path, so several
    /// `MakeFunction` ops (each with its own defaults) may create the same region.
    pub parent: Option<(RegionId, OpId)>,
    pub params: Vec<Param>,
    pub returns: Option<StaticExpr>,
    pub blocks: Vec<Block>,
    pub entry: BlockId,
    pub num_regs: u32,
    pub num_slots: u32,
    pub range: TextRange,
}

impl Region {
    /// Blocks reachable from the entry, in reverse postorder: every block precedes its
    /// successors except along loop back edges.
    pub fn reverse_postorder(&self) -> Vec<BlockId> {
        let successors = |b: BlockId| self.blocks[b.index()].terminator.successors();
        let mut seen = vec![false; self.blocks.len()];
        let mut post = Vec::new();
        // Iterative depth-first search; each entry is a block and its next successor.
        let mut stack = vec![(self.entry, successors(self.entry), 0)];
        seen[self.entry.index()] = true;
        while let Some((block, succs, i)) = stack.last_mut() {
            if let Some(&next) = succs.get(*i) {
                *i += 1;
                if !seen[next.index()] {
                    seen[next.index()] = true;
                    let s = successors(next);
                    stack.push((next, s, 0));
                }
            } else {
                post.push(*block);
                stack.pop();
            }
        }
        post.reverse();
        post
    }
}

/// A lowered module.
pub struct Program {
    pub module: ModuleName,
    pub scopes: Vec<Scope>,
    pub symbols: Vec<Symbol>,
    pub regions: Vec<Region>,
    pub ops: Vec<Op>,
    pub syntax_errors: Vec<(String, TextRange)>,
    pub(crate) lexical: Lexical,
    /// Computed on the first [`Program::free_symbols`] query.
    pub(crate) free_symbols: OnceLock<Vec<Vec<SymbolId>>>,
    pub(crate) line_index: LineIndex,
    pub(crate) source: String,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Program")
            .field("module", &self.module)
            .field("regions", &self.regions.len())
            .field("ops", &self.ops.len())
            .finish()
    }
}

impl Program {
    pub const MODULE: RegionId = RegionId(0);

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id.index()]
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.index()]
    }

    pub fn region(&self, id: RegionId) -> &Region {
        &self.regions[id.index()]
    }

    pub fn op(&self, id: OpId) -> &Op {
        &self.ops[id.index()]
    }

    /// The region whose frame stores `symbol`, or `None` for builtins.
    pub fn owner(&self, symbol: SymbolId) -> Option<RegionId> {
        let s = self.symbol(symbol);
        s.slot.map(|_| self.scope(s.scope).region)
    }

    /// Look up a name as code in `scope` would see it at runtime. `None` for a builtin
    /// that no code in the module references.
    pub fn resolve(&self, scope: ScopeId, name: &Name) -> Option<SymbolId> {
        self.lexical.resolve(&self.scopes, scope, name)
    }

    /// The symbols of the module scope (not of classes lowered inline into the module).
    pub fn module_symbols(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.scope(self.region(Self::MODULE).scope)
            .symbols
            .values()
            .copied()
    }

    /// The symbols that `region` or a region nested in it accesses (or may read as a
    /// [`Symbol::fallback`]) but that no region in that subtree owns: enclosing
    /// function cells, module globals seen from functions, and builtins. Sorted.
    pub fn free_symbols(&self, region: RegionId) -> &[SymbolId] {
        &self
            .free_symbols
            .get_or_init(|| self.compute_free_symbols())[region.index()]
    }

    fn compute_free_symbols(&self) -> Vec<Vec<SymbolId>> {
        // `ancestors[r]` is `r` followed by the regions that enclose it.
        let ancestors: Vec<Vec<RegionId>> = (0..self.regions.len())
            .map(|r| {
                let mut chain = vec![RegionId(r as u32)];
                while let Some((parent, _)) = self.region(*chain.last().unwrap()).parent {
                    chain.push(parent);
                }
                chain
            })
            .collect();
        let mut free: Vec<BTreeSet<SymbolId>> = vec![BTreeSet::new(); self.regions.len()];
        for op in &self.ops {
            let OpKind::Name { symbol, access } = op.kind else {
                continue;
            };
            // Only a load can read a fallback.
            let is_load = matches!(access, Access::Load(_));
            let mut next = Some(symbol);
            while let Some(s) = next {
                // A symbol is free in every region on the path from the op's region up
                // to, but excluding, the first region whose subtree contains the owner.
                let owner_chain = self.owner(s).map(|o| &ancestors[o.index()]);
                for r in &ancestors[op.region.index()] {
                    if owner_chain.is_some_and(|chain| chain.contains(r)) {
                        break;
                    }
                    free[r.index()].insert(s);
                }
                next = self.symbol(s).fallback.filter(|_| is_load);
            }
        }
        free.into_iter().map(|s| s.into_iter().collect()).collect()
    }

    /// The module-level symbol for `name`, if the module binds or declares it.
    pub fn global(&self, name: &str) -> Option<SymbolId> {
        self.scope(self.region(Self::MODULE).scope)
            .symbols
            .get(&Name::new(name))
            .copied()
    }

    /// One-based line and column of the start of `range`.
    pub fn line_col(&self, range: TextRange) -> (OneIndexed, OneIndexed) {
        let loc = self.line_index.line_column(range.start(), &self.source);
        (loc.line, loc.column)
    }

    pub fn text(&self, range: TextRange) -> &str {
        &self.source[range]
    }

    /// A readable listing of a region, one block per paragraph. Symbols owned by
    /// another region are suffixed with `@` and the owning region's name.
    pub fn dump(&self, region: RegionId) -> String {
        let r = self.region(region);
        let reg = |r: &Reg| format!("r{}", r.0);
        let regs = |rs: &[Reg]| rs.iter().map(reg).collect::<Vec<_>>().join(", ");
        let sym = |s: &SymbolId| {
            let symbol = self.symbol(*s);
            match self.owner(*s) {
                Some(owner) if owner == region => symbol.name.to_string(),
                Some(owner) => format!("{}@{}", symbol.name, self.region(owner).name),
                None => format!("{}@builtins", symbol.name),
            }
        };
        let element = |e: &Element| match e {
            Element::Value(r) => reg(r),
            Element::Star(r) => format!("*{}", reg(r)),
            Element::Pair(k, v) => format!("{}: {}", reg(k), reg(v)),
            Element::DoubleStar(r) => format!("**{}", reg(r)),
            Element::Missing => "_".to_owned(),
        };
        let args = |args: &[Arg]| {
            args.iter()
                .map(|a| match a {
                    Arg::Positional(r) => reg(r),
                    Arg::Star(r) => format!("*{}", reg(r)),
                    Arg::Keyword(k, r) => format!("{k}={}", reg(r)),
                    Arg::DoubleStar(r) => format!("**{}", reg(r)),
                })
                .collect::<Vec<_>>()
                .join(", ")
        };
        let access = |target: String, a: &Access| match a {
            Access::Load(r) => format!("{} = {target}", reg(r)),
            Access::Store(r) => format!("{target} = {}", reg(r)),
            Access::Delete => format!("del {target}"),
        };
        let mut out = String::new();
        for (i, block) in r.blocks.iter().enumerate() {
            out.push_str(&format!("b{i}:\n"));
            for op in &block.ops {
                let line = match &self.op(*op).kind {
                    OpKind::Name { symbol, access: a } => access(sym(symbol), a),
                    OpKind::Attribute {
                        object,
                        attr,
                        access: a,
                    } => access(format!("{}.{attr}", reg(object)), a),
                    OpKind::Subscript {
                        object,
                        index,
                        access: a,
                    } => access(format!("{}[{}]", reg(object), reg(index)), a),
                    OpKind::Literal { value, dst } => format!("{} = {value:?}", reg(dst)),
                    OpKind::Build {
                        kind,
                        elements,
                        dst,
                    } => format!(
                        "{} = {kind:?}[{}]",
                        reg(dst),
                        elements.iter().map(element).collect::<Vec<_>>().join(", ")
                    ),
                    OpKind::Append {
                        collection,
                        element: e,
                    } => {
                        format!("append {} <- {}", reg(collection), element(e))
                    }
                    OpKind::Call { func, args: a, dst } => {
                        format!("{} = {}({})", reg(dst), reg(func), args(a))
                    }
                    OpKind::Unary { op, operand, dst } => {
                        format!("{} = {op:?} {}", reg(dst), reg(operand))
                    }
                    OpKind::Binary {
                        op,
                        left,
                        right,
                        dst,
                    } => {
                        format!("{} = {} {op:?} {}", reg(dst), reg(left), reg(right))
                    }
                    OpKind::Unpack { value, dsts, star } => {
                        format!("{} = unpack {} star={star:?}", regs(dsts), reg(value))
                    }
                    OpKind::Iter { iterable, dst, .. } => {
                        format!("{} = iter {}", reg(dst), reg(iterable))
                    }
                    OpKind::Next { iterator, dst, .. } => {
                        format!("{} = next {}", reg(dst), reg(iterator))
                    }
                    OpKind::Param { index, dst } => format!("{} = param {index}", reg(dst)),
                    OpKind::MakeFunction {
                        region,
                        defaults,
                        dst,
                    } => format!(
                        "{} = function {}({})",
                        reg(dst),
                        self.region(*region).name,
                        regs(defaults)
                    ),
                    OpKind::MakeClass {
                        name, bases, dst, ..
                    } => {
                        format!("{} = class {name}({})", reg(dst), args(bases))
                    }
                    OpKind::Import { module, item, dst } => {
                        format!("{} = import {module} {item:?}", reg(dst))
                    }
                    OpKind::ContextEnter { manager, dst, .. } => {
                        format!("{} = enter {}", reg(dst), reg(manager))
                    }
                    OpKind::ContextExit { manager, .. } => format!("exit {}", reg(manager)),
                    OpKind::Await { value, dst } => format!("{} = await {}", reg(dst), reg(value)),
                    OpKind::Yield { value, dst, from } => format!(
                        "{} = yield{} {}",
                        reg(dst),
                        if *from { " from" } else { "" },
                        value.as_ref().map_or(String::new(), reg)
                    ),
                    OpKind::Gap { kind, dst } => match dst {
                        Some(d) => format!("{} = gap {kind:?}", reg(d)),
                        None => format!("gap {kind:?}"),
                    },
                };
                out.push_str(&format!("  {line}\n"));
            }
            let t = match &block.terminator {
                Terminator::Jump(b) => format!("jump b{}", b.0),
                Terminator::Branch { cond, then, orelse } => {
                    format!("branch {} ? b{} : b{}", reg(cond), then.0, orelse.0)
                }
                Terminator::Choice(bs) => format!(
                    "choice [{}]",
                    bs.iter()
                        .map(|b| format!("b{}", b.0))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Terminator::Return(Some(r)) => format!("return {}", reg(r)),
                Terminator::Return(None) => "return".to_owned(),
                Terminator::Raise { exc, cause, .. } => format!(
                    "raise {} {cause:?}",
                    exc.as_ref().map_or("_".to_owned(), reg)
                ),
            };
            out.push_str(&format!("  {t}\n"));
        }
        out
    }
}
