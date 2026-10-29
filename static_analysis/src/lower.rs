/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Lowering from the Ruff AST to the operation CFG. This is the only module that walks
//! syntax; every other component consumes [`Program`].
//!
//! Operations are emitted in CPython's evaluation order: an assignment evaluates its
//! value before any target subexpression, a call evaluates the callee before its
//! arguments, decorators are evaluated before defaults, and `and`/`or`, conditional
//! expressions and chained comparisons branch explicitly.

use std::sync::OnceLock;

use pyrefly_python::ast::Ast;
use pyrefly_python::module_name::ModuleName;
use ruff_python_ast::Arguments;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::InterpolatedStringElement;
use ruff_python_ast::Number;
use ruff_python_ast::Parameters;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::StmtTry;
use ruff_python_ast::name::Name;
use ruff_python_ast::visitor::Visitor;
use ruff_python_ast::visitor::walk_expr;
use ruff_source_file::LineIndex;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::ir::Access;
use crate::ir::Arg;
use crate::ir::BinaryOp;
use crate::ir::Block;
use crate::ir::BlockId;
use crate::ir::BuildKind;
use crate::ir::Element;
use crate::ir::GapKind;
use crate::ir::ImportItem;
use crate::ir::Literal;
use crate::ir::Op;
use crate::ir::OpId;
use crate::ir::OpKind;
use crate::ir::Param;
use crate::ir::ParamKind;
use crate::ir::Program;
use crate::ir::RaiseCause;
use crate::ir::Reg;
use crate::ir::Region;
use crate::ir::RegionId;
use crate::ir::ScopeId;
use crate::ir::ScopeKind;
use crate::ir::StaticExpr;
use crate::ir::Terminator;
use crate::ir::Trigger;
use crate::scope::GENERATOR_ARG;
use crate::scope::ScopeTable;
use crate::scope::annotation_may_have_effects;

pub fn lower(source: &str, module: ModuleName, is_init: bool) -> Program {
    let (ast, parse_errors, unsupported) = Ast::parse(source, PySourceType::Python);
    let syntax_errors: Vec<(String, TextRange)> = parse_errors
        .iter()
        .map(|e| (e.error.to_string(), e.location))
        .chain(unsupported.iter().map(|e| (e.to_string(), e.range)))
        .collect();
    let table = ScopeTable::new(&ast.body, ast.range);
    let mut lowerer = Lowerer {
        module,
        is_init,
        regions: (0..table.num_regions).map(|_| None).collect(),
        table,
        ops: Vec::new(),
        r: RegionState::new(ScopeId(0)),
    };
    let module_scope = lowerer.table.module();
    lowerer.r = RegionState::new(module_scope);
    if let Some((_, range)) = syntax_errors.first() {
        lowerer.gap(GapKind::SyntaxError, None, *range);
    }
    lowerer.stmts(&ast.body);
    lowerer.terminate(Terminator::Return(None));
    let state = std::mem::replace(&mut lowerer.r, RegionState::new(module_scope));
    let (blocks, num_regs) = state.into_blocks();
    lowerer.regions[0] = Some(Region {
        name: Name::new_static("<module>"),
        trigger: Trigger::OnImport,
        scope: module_scope,
        parent: None,
        params: Vec::new(),
        returns: None,
        blocks,
        entry: BlockId(0),
        num_regs,
        num_slots: 0,
        range: ast.range,
    });
    let Lowerer {
        module,
        regions,
        table,
        ops,
        ..
    } = lowerer;
    let parts = table.into_parts();
    let mut program = Program {
        module,
        scopes: parts.scopes,
        symbols: parts.symbols,
        regions: regions
            .into_iter()
            .map(|r| r.expect("every region in the scope table is lowered"))
            .collect(),
        ops,
        syntax_errors,
        lexical: parts.lexical,
        free_symbols: OnceLock::new(),
        line_index: LineIndex::from_source_text(source),
        source: source.to_owned(),
    };
    for symbol in &program.symbols {
        if let Some(slot) = symbol.slot {
            let region = &mut program.regions[program.scopes[symbol.scope.index()].region.index()];
            region.num_slots = region.num_slots.max(slot + 1);
        }
    }
    // A symbol is captured if code in another region accesses it.
    for i in 0..program.ops.len() {
        if let OpKind::Name { symbol, .. } = program.ops[i].kind
            && program
                .owner(symbol)
                .is_some_and(|owner| owner != program.ops[i].region)
        {
            program.symbols[symbol.index()].captured = true;
        }
    }
    program
}

/// The number of `finally` bodies a region may copy onto exit paths before further
/// copies are replaced by a [`GapKind::FinallyLimit`]. Nested `finally` blocks multiply
/// copies, so this bounds the size of the lowered graph.
const MAX_FINALLY_COPIES: usize = 256;

/// A pending action on the way out of a construct.
enum Unwind<'a> {
    /// A `with` item whose `ContextExit` must run.
    With {
        manager: Reg,
        context: OpId,
        is_async: bool,
    },
    /// A `finally` body that must run.
    Finally(&'a [Stmt]),
    /// Where an exception raised at this depth is dispatched.
    Handler(BlockId),
    /// The name bound by `except ... as name`, deleted however the handler is left.
    DeleteName(&'a Identifier),
}

struct Loop {
    break_to: BlockId,
    continue_to: BlockId,
    depth: usize,
}

/// The region currently being lowered.
struct RegionState<'a> {
    scope: ScopeId,
    blocks: Vec<(Vec<OpId>, Option<Terminator>)>,
    /// `None` after a terminator: following code is unreachable.
    current: Option<BlockId>,
    num_regs: u32,
    unwind: Vec<Unwind<'a>>,
    loops: Vec<Loop>,
    stmt: TextRange,
    /// `finally` bodies lowered so far by unwinding.
    finally_copies: usize,
}

impl<'a> RegionState<'a> {
    fn new(scope: ScopeId) -> Self {
        Self {
            scope,
            blocks: vec![(Vec::new(), None)],
            current: Some(BlockId(0)),
            num_regs: 0,
            unwind: Vec::new(),
            loops: Vec::new(),
            stmt: TextRange::default(),
            finally_copies: 0,
        }
    }

    /// The finished blocks and temporary count.
    fn into_blocks(self) -> (Vec<Block>, u32) {
        let blocks = self
            .blocks
            .into_iter()
            .map(|(ops, terminator)| Block {
                ops,
                terminator: terminator.expect("every block is terminated"),
            })
            .collect();
        (blocks, self.num_regs)
    }
}

struct Lowerer<'a> {
    module: ModuleName,
    is_init: bool,
    table: ScopeTable,
    regions: Vec<Option<Region>>,
    ops: Vec<Op>,
    r: RegionState<'a>,
}

/// The source of a deferred region's body.
enum Body<'a> {
    Function(&'a StmtFunctionDef),
    Lambda(&'a Expr),
    Generator(&'a [Comprehension], &'a Expr),
}

impl<'a> Lowerer<'a> {
    // Building blocks.

    fn new_block(&mut self) -> BlockId {
        self.r.blocks.push((Vec::new(), None));
        BlockId(self.r.blocks.len() as u32 - 1)
    }

    /// The current block, starting a fresh unreachable one after a terminator.
    fn current(&mut self) -> BlockId {
        match self.r.current {
            Some(b) => b,
            None => {
                let b = self.new_block();
                self.r.current = Some(b);
                b
            }
        }
    }

    fn start(&mut self, block: BlockId) {
        self.r.current = Some(block);
    }

    fn terminate(&mut self, terminator: Terminator) {
        let b = self.current();
        self.r.blocks[b.index()].1 = Some(terminator);
        self.r.current = None;
    }

    fn jump(&mut self, to: BlockId) {
        if self.r.current.is_some() {
            self.terminate(Terminator::Jump(to));
        }
    }

    fn reg(&mut self) -> Reg {
        self.r.num_regs += 1;
        Reg(self.r.num_regs - 1)
    }

    fn next_op(&self) -> OpId {
        OpId(self.ops.len() as u32)
    }

    fn emit(&mut self, kind: OpKind, range: TextRange) -> OpId {
        let block = self.current();
        let id = self.next_op();
        self.ops.push(Op {
            kind,
            region: RegionId(self.regions_lowering_index()),
            scope: self.r.scope,
            block,
            range,
            stmt: self.r.stmt,
        });
        self.r.blocks[block.index()].0.push(id);
        id
    }

    fn regions_lowering_index(&self) -> u32 {
        self.table.scopes[self.r.scope.index()].region.0
    }

    fn gap(&mut self, kind: GapKind, dst: Option<Reg>, range: TextRange) {
        self.emit(OpKind::Gap { kind, dst }, range);
    }

    fn name(&mut self, name: &Name, access: Access, range: TextRange) {
        let (symbol, error) = self.table.resolve(self.r.scope, name);
        if error {
            self.gap(GapKind::ScopeError, None, range);
        }
        self.emit(OpKind::Name { symbol, access }, range);
    }

    // Unwinding.

    /// Emit the exits of every pending `with` and `finally` above `depth`, innermost first.
    fn unwind_to(&mut self, depth: usize) {
        for i in (depth..self.r.unwind.len()).rev() {
            if self.r.current.is_none() {
                return;
            }
            match self.r.unwind[i] {
                Unwind::With {
                    manager,
                    context,
                    is_async,
                } => {
                    let range = self.op_range(context);
                    self.emit(
                        OpKind::ContextExit {
                            manager,
                            context,
                            is_async,
                        },
                        range,
                    );
                }
                Unwind::Finally(body) => {
                    // The finally body runs with only the entries outside it pending.
                    let saved = self.r.unwind.split_off(i);
                    self.finally_body(body);
                    self.r.unwind.extend(saved);
                }
                Unwind::DeleteName(name) => self.name(&name.id, Access::Delete, name.range),
                Unwind::Handler(_) => {}
            }
        }
    }

    fn op_range(&self, op: OpId) -> TextRange {
        self.ops[op.index()].range
    }

    fn innermost_handler(&self) -> Option<(usize, BlockId)> {
        self.r
            .unwind
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, u)| match u {
                Unwind::Handler(b) => Some((i, *b)),
                _ => None,
            })
    }

    /// Raise from the current point: to the innermost handler, or out of the region.
    fn raise(&mut self, exc: Option<Reg>, cause: RaiseCause, range: TextRange) {
        match self.innermost_handler() {
            Some((depth, handler)) => {
                self.unwind_to(depth + 1);
                self.jump(handler);
            }
            None => {
                self.unwind_to(0);
                if self.r.current.is_some() {
                    self.terminate(Terminator::Raise { exc, cause, range });
                }
            }
        }
    }

    /// Inside a `try`, the current point may raise: add an edge to the handler.
    fn exception_edge(&mut self) {
        if self.r.current.is_none() {
            return;
        }
        if let Some((depth, handler)) = self.innermost_handler() {
            let next = self.new_block();
            let edge = self.new_block();
            self.terminate(Terminator::Choice(vec![next, edge]));
            self.start(edge);
            self.unwind_to(depth + 1);
            self.jump(handler);
            self.start(next);
        }
    }

    // Statements.

    fn stmts(&mut self, body: &'a [Stmt]) {
        for x in body {
            self.exception_edge();
            let outer = self.r.stmt;
            self.r.stmt = x.range();
            self.stmt(x);
            self.r.stmt = outer;
        }
        self.exception_edge();
    }

    fn stmt(&mut self, x: &'a Stmt) {
        match x {
            Stmt::FunctionDef(f) => {
                let decorators = self.decorators(&f.decorator_list);
                if let Some(t) = &f.type_params {
                    self.gap(GapKind::TypeParams, None, t.range);
                }
                let annotations = f
                    .parameters
                    .iter()
                    .filter_map(|p| p.annotation())
                    .chain(f.returns.as_deref());
                if let Some(a) = annotations
                    .into_iter()
                    .find(|a| annotation_may_have_effects(a))
                {
                    self.gap(GapKind::AnnotationEvaluation, None, a.range());
                }
                let function = self.reg();
                self.function(f.range, Some(&f.parameters), Body::Function(f), function);
                let value = self.apply_decorators(decorators, function, f.range);
                self.name(&f.name.id, Access::Store(value), f.name.range);
            }
            Stmt::ClassDef(c) => {
                let decorators = self.decorators(&c.decorator_list);
                if let Some(t) = &c.type_params {
                    self.gap(GapKind::TypeParams, None, t.range);
                }
                let bases = match &c.arguments {
                    Some(arguments) => self.arguments(arguments),
                    None => Vec::new(),
                };
                let scope = self.table.scope_of_node(c.range);
                let outer = std::mem::replace(&mut self.r.scope, scope);
                self.stmts(&c.body);
                self.r.scope = outer;
                let dst = self.reg();
                self.emit(
                    OpKind::MakeClass {
                        name: c.name.id.clone(),
                        scope,
                        bases,
                        dst,
                    },
                    c.range,
                );
                let value = self.apply_decorators(decorators, dst, c.range);
                self.name(&c.name.id, Access::Store(value), c.name.range);
            }
            Stmt::Return(x) => {
                let value = match &x.value {
                    Some(v) => self.expr(v),
                    None => self.literal(Literal::None, x.range),
                };
                self.unwind_to(0);
                if self.r.current.is_some() {
                    self.terminate(Terminator::Return(Some(value)));
                }
            }
            Stmt::Delete(x) => {
                for t in &x.targets {
                    self.delete(t);
                }
            }
            Stmt::Assign(x) => {
                let value = self.expr(&x.value);
                for t in &x.targets {
                    self.assign(t, value);
                }
            }
            Stmt::AugAssign(x) => {
                let op = BinaryOp::InPlace(x.op);
                let dst = self.reg();
                match &*x.target {
                    Expr::Name(n) => {
                        let current = self.reg();
                        self.name(&n.id, Access::Load(current), n.range);
                        let right = self.expr(&x.value);
                        self.binary(op, current, right, dst, x.range);
                        self.name(&n.id, Access::Store(dst), n.range);
                    }
                    Expr::Attribute(a) => {
                        let object = self.expr(&a.value);
                        let current = self.reg();
                        let attr = a.attr.id.clone();
                        self.emit(
                            OpKind::Attribute {
                                object,
                                attr: attr.clone(),
                                access: Access::Load(current),
                            },
                            a.range,
                        );
                        let right = self.expr(&x.value);
                        self.binary(op, current, right, dst, x.range);
                        self.emit(
                            OpKind::Attribute {
                                object,
                                attr,
                                access: Access::Store(dst),
                            },
                            a.range,
                        );
                    }
                    Expr::Subscript(s) => {
                        let object = self.expr(&s.value);
                        let index = self.expr(&s.slice);
                        let current = self.reg();
                        self.emit(
                            OpKind::Subscript {
                                object,
                                index,
                                access: Access::Load(current),
                            },
                            s.range,
                        );
                        let right = self.expr(&x.value);
                        self.binary(op, current, right, dst, x.range);
                        self.emit(
                            OpKind::Subscript {
                                object,
                                index,
                                access: Access::Store(dst),
                            },
                            s.range,
                        );
                    }
                    // Any other target is a syntax error that the parser reports.
                    _ => {}
                }
            }
            Stmt::AnnAssign(x) => {
                // Only module and class annotations would ever be evaluated at runtime.
                let kind = self.table.scopes[self.r.scope.index()].kind;
                if matches!(kind, ScopeKind::Module | ScopeKind::Class)
                    && annotation_may_have_effects(&x.annotation)
                {
                    self.gap(GapKind::AnnotationEvaluation, None, x.annotation.range());
                }
                match (&x.value, &*x.target) {
                    (Some(v), _) => {
                        let value = self.expr(v);
                        self.assign(&x.target, value);
                    }
                    // Without a value, CPython still evaluates an attribute target's object
                    // and a subscript target's object and index, but stores nothing.
                    (None, Expr::Attribute(a)) => {
                        self.expr(&a.value);
                    }
                    (None, Expr::Subscript(s)) => {
                        self.expr(&s.value);
                        self.expr(&s.slice);
                    }
                    (None, _) => {}
                }
            }
            Stmt::TypeAlias(x) => {
                let dst = self.reg();
                self.gap(GapKind::TypeParams, Some(dst), x.range);
                self.assign(&x.name, dst);
            }
            Stmt::For(x) => {
                let iterable = self.expr(&x.iter);
                let iterator = self.reg();
                self.emit(
                    OpKind::Iter {
                        iterable,
                        dst: iterator,
                        is_async: x.is_async,
                    },
                    x.iter.range(),
                );
                let head = self.new_block();
                let body = self.new_block();
                let orelse = self.new_block();
                let after = self.new_block();
                self.jump(head);
                self.start(head);
                self.terminate(Terminator::Choice(vec![body, orelse]));
                self.start(body);
                let item = self.reg();
                self.emit(
                    OpKind::Next {
                        iterator,
                        dst: item,
                        is_async: x.is_async,
                    },
                    x.target.range(),
                );
                self.assign(&x.target, item);
                self.loop_body(&x.body, after, head);
                self.start(orelse);
                self.stmts(&x.orelse);
                self.jump(after);
                self.start(after);
            }
            Stmt::While(x) => {
                let head = self.new_block();
                let body = self.new_block();
                let orelse = self.new_block();
                let after = self.new_block();
                self.jump(head);
                self.start(head);
                if is_truthy_constant(&x.test) {
                    self.terminate(Terminator::Jump(body));
                } else {
                    let cond = self.expr(&x.test);
                    self.terminate(Terminator::Branch {
                        cond,
                        then: body,
                        orelse,
                    });
                }
                self.start(body);
                self.loop_body(&x.body, after, head);
                self.start(orelse);
                self.stmts(&x.orelse);
                self.jump(after);
                self.start(after);
            }
            Stmt::If(x) => {
                let after = self.new_block();
                for (test, body) in Ast::if_branches(x) {
                    match test {
                        Some(test) => {
                            let cond = self.expr(test);
                            let then = self.new_block();
                            let next = self.new_block();
                            self.terminate(Terminator::Branch {
                                cond,
                                then,
                                orelse: next,
                            });
                            self.start(then);
                            self.stmts(body);
                            self.jump(after);
                            self.start(next);
                        }
                        None => self.stmts(body),
                    }
                }
                self.jump(after);
                self.start(after);
            }
            Stmt::With(x) => {
                let depth = self.r.unwind.len();
                for item in &x.items {
                    let manager = self.expr(&item.context_expr);
                    let dst = self.reg();
                    let context = self.next_op();
                    self.emit(
                        OpKind::ContextEnter {
                            manager,
                            dst,
                            context,
                            is_async: x.is_async,
                        },
                        item.range,
                    );
                    if let Some(target) = &item.optional_vars {
                        self.assign(target, dst);
                    }
                    self.r.unwind.push(Unwind::With {
                        manager,
                        context,
                        is_async: x.is_async,
                    });
                }
                self.stmts(&x.body);
                self.unwind_to(depth);
                self.r.unwind.truncate(depth);
            }
            Stmt::Match(x) => {
                self.expr(&x.subject);
                self.gap(GapKind::Match, None, x.range);
                let after = self.new_block();
                for case in &x.cases {
                    // Try this case or fall through to the next one.
                    let this = self.new_block();
                    let next = self.new_block();
                    self.terminate(Terminator::Choice(vec![this, next]));
                    self.start(this);
                    let mut captures = Vec::new();
                    Ast::pattern_lvalue(&case.pattern, &mut |id| captures.push(id));
                    for id in captures {
                        let dst = self.reg();
                        self.gap(GapKind::Match, Some(dst), id.range);
                        self.name(&id.id, Access::Store(dst), id.range);
                    }
                    if let Some(guard) = &case.guard {
                        let cond = self.expr(guard);
                        let body = self.new_block();
                        self.terminate(Terminator::Branch {
                            cond,
                            then: body,
                            orelse: next,
                        });
                        self.start(body);
                    }
                    self.stmts(&case.body);
                    self.jump(after);
                    self.start(next);
                    if case.guard.is_none() && case.pattern.is_irrefutable() {
                        // No later case and no fall-through is reachable.
                        self.terminate(Terminator::Choice(Vec::new()));
                    }
                }
                self.jump(after);
                self.start(after);
            }
            Stmt::Raise(x) => {
                let exc = x.exc.as_ref().map(|e| self.expr(e));
                if let Some(cause) = &x.cause {
                    self.expr(cause);
                }
                self.raise(exc, RaiseCause::Explicit, x.range);
            }
            Stmt::Try(x) => self.try_(x),
            Stmt::Assert(x) => {
                let cond = self.expr(&x.test);
                let ok = self.new_block();
                let fail = self.new_block();
                self.terminate(Terminator::Branch {
                    cond,
                    then: ok,
                    orelse: fail,
                });
                self.start(fail);
                // The message is evaluated only on failure; the raised exception is an
                // `AssertionError` that wraps it, so no temporary holds the exception.
                if let Some(m) = &x.msg {
                    self.expr(m);
                }
                self.raise(None, RaiseCause::AssertFailure, x.range);
                self.start(ok);
            }
            Stmt::Import(x) => {
                for a in &x.names {
                    let module = ModuleName::from_name(&a.name.id);
                    let dst = self.reg();
                    match &a.asname {
                        Some(alias) => {
                            self.emit(
                                OpKind::Import {
                                    module,
                                    item: ImportItem::Module,
                                    dst,
                                },
                                a.range,
                            );
                            self.name(&alias.id, Access::Store(dst), alias.range);
                        }
                        None => {
                            self.emit(
                                OpKind::Import {
                                    module,
                                    item: ImportItem::Root,
                                    dst,
                                },
                                a.range,
                            );
                            self.name(&module.first_component(), Access::Store(dst), a.name.range);
                        }
                    }
                }
            }
            Stmt::ImportFrom(x) => {
                let module = self.module.new_maybe_relative(
                    self.is_init,
                    x.level,
                    x.module.as_ref().map(|m| &m.id),
                );
                for a in &x.names {
                    let dst = self.reg();
                    match module {
                        None => self.gap(GapKind::InvalidRelativeImport, Some(dst), a.range),
                        Some(module) if &a.name == "*" => {
                            self.emit(
                                OpKind::Import {
                                    module,
                                    item: ImportItem::Module,
                                    dst,
                                },
                                a.range,
                            );
                            self.gap(GapKind::StarImport, None, a.range);
                        }
                        Some(module) => {
                            self.emit(
                                OpKind::Import {
                                    module,
                                    item: ImportItem::Name(a.name.id.clone()),
                                    dst,
                                },
                                a.range,
                            );
                        }
                    }
                    if &a.name != "*" {
                        let bound = a.asname.as_ref().unwrap_or(&a.name);
                        self.name(&bound.id, Access::Store(dst), bound.range);
                    }
                }
            }
            Stmt::Expr(x) => {
                self.expr(&x.value);
            }
            Stmt::Break(_) | Stmt::Continue(_) => {
                let l = self.r.loops.last().map(|l| {
                    (
                        l.depth,
                        if matches!(x, Stmt::Break(_)) {
                            l.break_to
                        } else {
                            l.continue_to
                        },
                    )
                });
                // Outside a loop this is a syntax error that the parser reports.
                if let Some((depth, to)) = l {
                    self.unwind_to(depth);
                    self.jump(to);
                }
            }
            Stmt::IpyEscapeCommand(x) => self.gap(GapKind::IpyEscape, None, x.range),
            Stmt::Global(_) | Stmt::Nonlocal(_) | Stmt::Pass(_) => {}
        }
    }

    fn loop_body(&mut self, body: &'a [Stmt], break_to: BlockId, continue_to: BlockId) {
        self.r.loops.push(Loop {
            break_to,
            continue_to,
            depth: self.r.unwind.len(),
        });
        self.stmts(body);
        self.r.loops.pop();
        self.jump(continue_to);
    }

    /// `try`: the body may raise at any statement boundary. Exceptions go to a dispatch
    /// that tries each handler in order; an unhandled exception runs `finally` and
    /// propagates. `finally` also runs on every normal exit, including `return`,
    /// `break` and `continue`.
    fn try_(&mut self, x: &'a StmtTry) {
        self.gap(GapKind::Exceptions, None, x.range);
        let depth = self.r.unwind.len();
        // An exception escaping the body and handlers runs a copy of `finally`, then
        // propagates. Pushing the handler above `Finally` means that a jump to it does
        // not also unwind through `Finally`, and that an exception inside a `finally`
        // copy goes to the outer handlers.
        let propagate = (!x.finalbody.is_empty()).then(|| self.new_block());
        if let Some(propagate) = propagate {
            self.r.unwind.push(Unwind::Finally(&x.finalbody));
            self.r.unwind.push(Unwind::Handler(propagate));
        }
        let protected = self.r.unwind.len();
        let dispatch = (!x.handlers.is_empty()).then(|| self.new_block());
        if let Some(dispatch) = dispatch {
            self.r.unwind.push(Unwind::Handler(dispatch));
        }
        self.stmts(&x.body);
        self.r.unwind.truncate(protected);
        self.stmts(&x.orelse);
        let finish = self.new_block();
        self.jump(finish);
        if let Some(dispatch) = dispatch {
            self.start(dispatch);
            for ExceptHandler::ExceptHandler(h) in &x.handlers {
                if let Some(t) = &h.type_ {
                    self.expr(t);
                }
                let this = self.new_block();
                let next = self.new_block();
                self.terminate(Terminator::Choice(vec![this, next]));
                self.start(this);
                let handler_depth = self.r.unwind.len();
                if let Some(name) = &h.name {
                    let dst = self.reg();
                    self.gap(GapKind::Exceptions, Some(dst), name.range);
                    self.name(&name.id, Access::Store(dst), name.range);
                    // Python deletes the bound exception however the handler is left.
                    self.r.unwind.push(Unwind::DeleteName(name));
                }
                self.stmts(&h.body);
                self.unwind_to(handler_depth);
                self.r.unwind.truncate(handler_depth);
                self.jump(finish);
                self.start(next);
                if h.type_.is_none() {
                    // A bare `except` catches everything.
                    self.terminate(Terminator::Choice(Vec::new()));
                }
            }
            // No handler matched.
            self.raise(None, RaiseCause::Reraise, x.range);
        }
        self.r.unwind.truncate(depth);
        self.start(finish);
        if let Some(propagate) = propagate {
            self.finally_body(&x.finalbody);
            let after = self.new_block();
            self.jump(after);
            self.start(propagate);
            self.finally_body(&x.finalbody);
            self.raise(None, RaiseCause::Reraise, x.range);
            self.start(after);
        }
    }

    /// Lower one copy of a `finally` body. Each exit path gets its own copy, so nested
    /// `finally` blocks multiply copies; past a budget, the copy is replaced by a gap.
    fn finally_body(&mut self, body: &'a [Stmt]) {
        if self.r.current.is_none() {
            return;
        }
        if self.r.finally_copies >= MAX_FINALLY_COPIES {
            self.gap(GapKind::FinallyLimit, None, self.r.stmt);
            return;
        }
        self.r.finally_copies += 1;
        self.stmts(body);
    }

    fn decorators(
        &mut self,
        decorators: &'a [ruff_python_ast::Decorator],
    ) -> Vec<(Reg, TextRange)> {
        decorators
            .iter()
            .map(|d| (self.expr(&d.expression), d.range))
            .collect()
    }

    /// Apply already-evaluated decorators bottom-up.
    fn apply_decorators(
        &mut self,
        decorators: Vec<(Reg, TextRange)>,
        mut value: Reg,
        _: TextRange,
    ) -> Reg {
        for (func, range) in decorators.into_iter().rev() {
            let dst = self.reg();
            self.emit(
                OpKind::Call {
                    func,
                    args: vec![Arg::Positional(value)],
                    dst,
                },
                range,
            );
            value = dst;
        }
        value
    }

    // Deferred regions.

    /// Evaluate defaults, lower the deferred region, and emit its `MakeFunction` into `dst`.
    fn function(
        &mut self,
        range: TextRange,
        parameters: Option<&'a Parameters>,
        body: Body<'a>,
        dst: Reg,
    ) {
        let defaults = parameters
            .into_iter()
            .flat_map(|p| p.iter_non_variadic_params())
            .filter_map(|p| p.default.as_deref())
            .map(|d| self.expr(d))
            .collect();
        let scope = self.table.scope_of_node(range);
        let region = self.table.scopes[scope.index()].region;
        let make = self.emit(
            OpKind::MakeFunction {
                region,
                defaults,
                dst,
            },
            range,
        );
        // A `finally` body is lowered once per exit path, so the same definition can be
        // reached again; its region is lowered only the first time.
        if self.regions[region.index()].is_some() {
            return;
        }
        let def_scope = self.r.scope;
        let mut params = Vec::new();
        if let Some(parameters) = parameters {
            let kinds = parameters
                .posonlyargs
                .iter()
                .map(|p| (&p.parameter, ParamKind::PositionalOnly, p.default.is_some()))
                .chain(
                    parameters
                        .args
                        .iter()
                        .map(|p| (&p.parameter, ParamKind::Positional, p.default.is_some())),
                )
                .chain(
                    parameters
                        .vararg
                        .iter()
                        .map(|p| (&**p, ParamKind::VarArgs, false)),
                )
                .chain(
                    parameters
                        .kwonlyargs
                        .iter()
                        .map(|p| (&p.parameter, ParamKind::KeywordOnly, p.default.is_some())),
                )
                .chain(
                    parameters
                        .kwarg
                        .iter()
                        .map(|p| (&**p, ParamKind::KwArgs, false)),
                );
            for (p, kind, has_default) in kinds {
                let annotation = p
                    .annotation
                    .as_deref()
                    .map(|a| self.static_expr(def_scope, a));
                params.push(Param {
                    name: p.name.id.clone(),
                    symbol: self.table.resolve(scope, &p.name.id).0,
                    kind,
                    annotation,
                    has_default,
                    range: p.range,
                });
            }
        }
        let (name, trigger, returns) = match &body {
            Body::Function(f) => {
                let generator = Ast::body_contains_yield(&f.body);
                let trigger = if generator {
                    Trigger::OnIteration
                } else if f.is_async {
                    Trigger::OnAwait
                } else {
                    Trigger::OnCall
                };
                let returns = f.returns.as_deref().map(|r| self.static_expr(def_scope, r));
                (f.name.id.clone(), trigger, returns)
            }
            Body::Lambda(e) => {
                let trigger = if Ast::expr_contains_yield(e) {
                    Trigger::OnIteration
                } else {
                    Trigger::OnCall
                };
                (Name::new_static("<lambda>"), trigger, None)
            }
            Body::Generator(..) => {
                let symbol = self
                    .table
                    .resolve(scope, &Name::new_static(GENERATOR_ARG))
                    .0;
                params.push(Param {
                    name: Name::new_static(GENERATOR_ARG),
                    symbol,
                    kind: ParamKind::PositionalOnly,
                    annotation: None,
                    has_default: false,
                    range,
                });
                (Name::new_static("<genexpr>"), Trigger::OnIteration, None)
            }
        };
        let outer = std::mem::replace(&mut self.r, RegionState::new(scope));
        for (index, p) in params.iter().enumerate() {
            let dst = self.reg();
            self.emit(OpKind::Param { index, dst }, p.range);
            self.emit(
                OpKind::Name {
                    symbol: p.symbol,
                    access: Access::Store(dst),
                },
                p.range,
            );
        }
        match body {
            Body::Function(f) => {
                self.stmts(&f.body);
                if self.r.current.is_some() {
                    let none = self.literal(Literal::None, f.range);
                    self.terminate(Terminator::Return(Some(none)));
                }
            }
            Body::Lambda(e) => {
                self.r.stmt = e.range();
                let value = self.expr(e);
                self.terminate(Terminator::Return(Some(value)));
            }
            Body::Generator(generators, elt) => {
                self.r.stmt = range;
                let iterator = self.reg();
                self.emit(
                    OpKind::Name {
                        symbol: params[0].symbol,
                        access: Access::Load(iterator),
                    },
                    range,
                );
                self.comprehension_loops(generators, iterator, &mut |this| {
                    let value = this.expr(elt);
                    let dst = this.reg();
                    this.emit(
                        OpKind::Yield {
                            value: Some(value),
                            dst,
                            from: false,
                        },
                        elt.range(),
                    );
                });
                let none = self.literal(Literal::None, range);
                self.terminate(Terminator::Return(Some(none)));
            }
        }
        let state = std::mem::replace(&mut self.r, outer);
        let (blocks, num_regs) = state.into_blocks();
        let parent = RegionId(self.regions_lowering_index());
        self.regions[region.index()] = Some(Region {
            name,
            trigger,
            scope,
            parent: Some((parent, make)),
            params,
            returns,
            blocks,
            entry: BlockId(0),
            num_regs,
            // Filled in once all symbols are known.
            num_slots: 0,
            range,
        });
    }

    /// `iter(iterable)` for one comprehension generator.
    fn iter(&mut self, iterable: Reg, g: &Comprehension) -> Reg {
        let iterator = self.reg();
        self.emit(
            OpKind::Iter {
                iterable,
                dst: iterator,
                is_async: g.is_async,
            },
            g.iter.range(),
        );
        iterator
    }

    /// The loops of a comprehension. The first iterator is already created; `body` runs
    /// once per innermost iteration. Leaves the exit block current.
    fn comprehension_loops(
        &mut self,
        generators: &'a [Comprehension],
        iterator: Reg,
        body: &mut dyn FnMut(&mut Self),
    ) {
        let (g, rest) = generators
            .split_first()
            .expect("a comprehension has at least one generator");
        let head = self.new_block();
        let next = self.new_block();
        let exit = self.new_block();
        self.jump(head);
        self.start(head);
        self.terminate(Terminator::Choice(vec![next, exit]));
        self.start(next);
        let item = self.reg();
        self.emit(
            OpKind::Next {
                iterator,
                dst: item,
                is_async: g.is_async,
            },
            g.target.range(),
        );
        self.assign(&g.target, item);
        for cond in &g.ifs {
            let c = self.expr(cond);
            let pass = self.new_block();
            self.terminate(Terminator::Branch {
                cond: c,
                then: pass,
                orelse: head,
            });
            self.start(pass);
        }
        match rest.first() {
            Some(inner) => {
                let inner_iterable = self.expr(&inner.iter);
                let inner_iterator = self.iter(inner_iterable, inner);
                self.comprehension_loops(rest, inner_iterator, body);
            }
            None => body(self),
        }
        self.jump(head);
        self.start(exit);
    }

    /// A list, set or dict comprehension, lowered inline in its own scope.
    fn comprehension(
        &mut self,
        range: TextRange,
        kind: BuildKind,
        generators: &'a [Comprehension],
        element: &mut dyn FnMut(&mut Self) -> Element,
        dst: Reg,
    ) {
        let first = self.expr(&generators[0].iter);
        let iterator = self.iter(first, &generators[0]);
        self.emit(
            OpKind::Build {
                kind,
                elements: Vec::new(),
                dst,
            },
            range,
        );
        let scope = self.table.scope_of_node(range);
        let outer = std::mem::replace(&mut self.r.scope, scope);
        self.comprehension_loops(generators, iterator, &mut |this| {
            let element = element(this);
            this.emit(
                OpKind::Append {
                    collection: dst,
                    element,
                },
                range,
            );
        });
        self.r.scope = outer;
    }

    // Targets.

    fn assign(&mut self, target: &'a Expr, value: Reg) {
        match target {
            Expr::Name(n) => self.name(&n.id, Access::Store(value), n.range),
            Expr::Attribute(a) => {
                let object = self.expr(&a.value);
                self.emit(
                    OpKind::Attribute {
                        object,
                        attr: a.attr.id.clone(),
                        access: Access::Store(value),
                    },
                    a.range,
                );
            }
            Expr::Subscript(s) => {
                let object = self.expr(&s.value);
                let index = self.expr(&s.slice);
                self.emit(
                    OpKind::Subscript {
                        object,
                        index,
                        access: Access::Store(value),
                    },
                    s.range,
                );
            }
            Expr::Tuple(_) | Expr::List(_) => {
                let elts = match target {
                    Expr::Tuple(t) => &t.elts,
                    Expr::List(l) => &l.elts,
                    _ => unreachable!("matched a tuple or list above"),
                };
                let dsts: Vec<Reg> = elts.iter().map(|_| self.reg()).collect();
                let star = elts.iter().position(|e| matches!(e, Expr::Starred(_)));
                self.emit(
                    OpKind::Unpack {
                        value,
                        dsts: dsts.clone(),
                        star,
                    },
                    target.range(),
                );
                for (e, dst) in elts.iter().zip(dsts) {
                    let e = match e {
                        Expr::Starred(s) => &*s.value,
                        e => e,
                    };
                    self.assign(e, dst);
                }
            }
            Expr::Starred(s) => self.assign(&s.value, value),
            // Any other target is a syntax error that the parser reports.
            _ => {}
        }
    }

    fn delete(&mut self, target: &'a Expr) {
        match target {
            Expr::Name(n) => self.name(&n.id, Access::Delete, n.range),
            Expr::Attribute(a) => {
                let object = self.expr(&a.value);
                self.emit(
                    OpKind::Attribute {
                        object,
                        attr: a.attr.id.clone(),
                        access: Access::Delete,
                    },
                    a.range,
                );
            }
            Expr::Subscript(s) => {
                let object = self.expr(&s.value);
                let index = self.expr(&s.slice);
                self.emit(
                    OpKind::Subscript {
                        object,
                        index,
                        access: Access::Delete,
                    },
                    s.range,
                );
            }
            Expr::Tuple(t) => t.elts.iter().for_each(|e| self.delete(e)),
            Expr::List(l) => l.elts.iter().for_each(|e| self.delete(e)),
            _ => {}
        }
    }

    // Expressions.

    fn expr(&mut self, x: &'a Expr) -> Reg {
        let dst = self.reg();
        self.expr_to(x, dst);
        dst
    }

    fn literal(&mut self, value: Literal, range: TextRange) -> Reg {
        let dst = self.reg();
        self.emit(OpKind::Literal { value, dst }, range);
        dst
    }

    fn binary(&mut self, op: BinaryOp, left: Reg, right: Reg, dst: Reg, range: TextRange) {
        self.emit(
            OpKind::Binary {
                op,
                left,
                right,
                dst,
            },
            range,
        );
    }

    fn build(&mut self, kind: BuildKind, elts: &'a [Expr], dst: Reg, range: TextRange) {
        let elements = elts
            .iter()
            .map(|e| match e {
                Expr::Starred(s) => Element::Star(self.expr(&s.value)),
                e => Element::Value(self.expr(e)),
            })
            .collect();
        self.emit(
            OpKind::Build {
                kind,
                elements,
                dst,
            },
            range,
        );
    }

    /// Evaluate call arguments as CPython does: every positional and starred argument
    /// first, then every keyword and `**` argument, each group in source order. This
    /// differs from source order when `*args` follows a keyword argument.
    fn arguments(&mut self, arguments: &'a Arguments) -> Vec<Arg> {
        let positional = arguments.args.iter().map(|a| match a {
            Expr::Starred(s) => Arg::Star(self.expr(&s.value)),
            e => Arg::Positional(self.expr(e)),
        });
        let mut args: Vec<Arg> = positional.collect();
        for k in &arguments.keywords {
            let value = self.expr(&k.value);
            args.push(match &k.arg {
                Some(name) => Arg::Keyword(name.id.clone(), value),
                None => Arg::DoubleStar(value),
            });
        }
        args
    }

    /// Evaluate `x` into `dst`.
    fn expr_to(&mut self, x: &'a Expr, dst: Reg) {
        let range = x.range();
        match x {
            Expr::BoolOp(b) => {
                let after = self.new_block();
                let (last, init) = b
                    .values
                    .split_last()
                    .expect("a boolean operation has operands");
                for v in init {
                    self.expr_to(v, dst);
                    let next = self.new_block();
                    let (then, orelse) = match b.op {
                        BoolOp::And => (next, after),
                        BoolOp::Or => (after, next),
                    };
                    self.terminate(Terminator::Branch {
                        cond: dst,
                        then,
                        orelse,
                    });
                    self.start(next);
                }
                self.expr_to(last, dst);
                self.jump(after);
                self.start(after);
            }
            Expr::Named(n) => {
                self.expr_to(&n.value, dst);
                self.assign(&n.target, dst);
            }
            Expr::BinOp(b) => {
                let left = self.expr(&b.left);
                let right = self.expr(&b.right);
                self.binary(BinaryOp::Arith(b.op), left, right, dst, range);
            }
            Expr::UnaryOp(u) => {
                let operand = self.expr(&u.operand);
                self.emit(
                    OpKind::Unary {
                        op: u.op,
                        operand,
                        dst,
                    },
                    range,
                );
            }
            Expr::Lambda(l) => {
                self.function(l.range, l.parameters.as_deref(), Body::Lambda(&l.body), dst);
            }
            Expr::If(i) => {
                let cond = self.expr(&i.test);
                let then = self.new_block();
                let orelse = self.new_block();
                let after = self.new_block();
                self.terminate(Terminator::Branch { cond, then, orelse });
                self.start(then);
                self.expr_to(&i.body, dst);
                self.jump(after);
                self.start(orelse);
                self.expr_to(&i.orelse, dst);
                self.jump(after);
                self.start(after);
            }
            Expr::Dict(d) => {
                let elements = d
                    .items
                    .iter()
                    .map(|item| match &item.key {
                        Some(k) => {
                            let k = self.expr(k);
                            Element::Pair(k, self.expr(&item.value))
                        }
                        None => Element::DoubleStar(self.expr(&item.value)),
                    })
                    .collect();
                self.emit(
                    OpKind::Build {
                        kind: BuildKind::Dict,
                        elements,
                        dst,
                    },
                    range,
                );
            }
            Expr::Set(s) => self.build(BuildKind::Set, &s.elts, dst, range),
            Expr::List(l) => self.build(BuildKind::List, &l.elts, dst, range),
            Expr::Tuple(t) => self.build(BuildKind::Tuple, &t.elts, dst, range),
            Expr::ListComp(c) => self.comprehension(
                range,
                BuildKind::List,
                &c.generators,
                &mut |this| Element::Value(this.expr(&c.elt)),
                dst,
            ),
            Expr::SetComp(c) => self.comprehension(
                range,
                BuildKind::Set,
                &c.generators,
                &mut |this| Element::Value(this.expr(&c.elt)),
                dst,
            ),
            Expr::DictComp(c) => self.comprehension(
                range,
                BuildKind::Dict,
                &c.generators,
                &mut |this| match &c.key {
                    Some(key) => {
                        let k = this.expr(key);
                        Element::Pair(k, this.expr(&c.value))
                    }
                    None => Element::DoubleStar(this.expr(&c.value)),
                },
                dst,
            ),
            Expr::Generator(g) => {
                // The first iterator is created eagerly and passed to the generator.
                let first = self.expr(&g.generators[0].iter);
                let first = self.iter(first, &g.generators[0]);
                let function = self.reg();
                self.function(
                    range,
                    None,
                    Body::Generator(&g.generators, &g.elt),
                    function,
                );
                self.emit(
                    OpKind::Call {
                        func: function,
                        args: vec![Arg::Positional(first)],
                        dst,
                    },
                    range,
                );
            }
            Expr::Await(a) => {
                let value = self.expr(&a.value);
                self.emit(OpKind::Await { value, dst }, range);
            }
            Expr::Yield(y) => {
                let value = y.value.as_ref().map(|v| self.expr(v));
                self.emit(
                    OpKind::Yield {
                        value,
                        dst,
                        from: false,
                    },
                    range,
                );
            }
            Expr::YieldFrom(y) => {
                let value = Some(self.expr(&y.value));
                self.emit(
                    OpKind::Yield {
                        value,
                        dst,
                        from: true,
                    },
                    range,
                );
            }
            Expr::Compare(c) => {
                // `a < b < c` evaluates `b` once and stops at the first false comparison.
                let after = self.new_block();
                let mut left = self.expr(c.first_operand());
                for (i, (op, comparator)) in c.ops.iter().zip(&c.operands[1..]).enumerate() {
                    let right = self.expr(comparator);
                    self.binary(BinaryOp::Compare(*op), left, right, dst, range);
                    if i + 1 < c.ops.len() {
                        let next = self.new_block();
                        self.terminate(Terminator::Branch {
                            cond: dst,
                            then: next,
                            orelse: after,
                        });
                        self.start(next);
                    }
                    left = right;
                }
                self.jump(after);
                self.start(after);
            }
            Expr::Call(c) => {
                let func = self.expr(&c.func);
                let args = self.arguments(&c.arguments);
                self.emit(OpKind::Call { func, args, dst }, range);
            }
            Expr::FString(_) | Expr::TString(_) => {
                let mut interpolations = Vec::new();
                Interpolations(&mut interpolations).visit_expr(x);
                let elements = interpolations
                    .into_iter()
                    .map(|e| Element::Value(self.expr(e)))
                    .collect();
                self.emit(
                    OpKind::Build {
                        kind: BuildKind::String,
                        elements,
                        dst,
                    },
                    range,
                );
            }
            Expr::StringLiteral(s) => {
                self.emit(
                    OpKind::Literal {
                        value: Literal::Str(s.value.to_str().into()),
                        dst,
                    },
                    range,
                );
            }
            Expr::BytesLiteral(_) => {
                self.emit(
                    OpKind::Literal {
                        value: Literal::Bytes,
                        dst,
                    },
                    range,
                );
            }
            Expr::NumberLiteral(n) => {
                let value = match &n.value {
                    Number::Int(i) => Literal::Int(i.as_i64()),
                    Number::Float(f) => Literal::Float(*f),
                    Number::Complex { .. } => Literal::Complex,
                };
                self.emit(OpKind::Literal { value, dst }, range);
            }
            Expr::BooleanLiteral(b) => {
                self.emit(
                    OpKind::Literal {
                        value: Literal::Bool(b.value),
                        dst,
                    },
                    range,
                );
            }
            Expr::NoneLiteral(_) => {
                self.emit(
                    OpKind::Literal {
                        value: Literal::None,
                        dst,
                    },
                    range,
                );
            }
            Expr::EllipsisLiteral(_) => {
                self.emit(
                    OpKind::Literal {
                        value: Literal::Ellipsis,
                        dst,
                    },
                    range,
                );
            }
            Expr::Attribute(a) => {
                let object = self.expr(&a.value);
                self.emit(
                    OpKind::Attribute {
                        object,
                        attr: a.attr.id.clone(),
                        access: Access::Load(dst),
                    },
                    range,
                );
            }
            Expr::Subscript(s) => {
                let object = self.expr(&s.value);
                let index = self.expr(&s.slice);
                self.emit(
                    OpKind::Subscript {
                        object,
                        index,
                        access: Access::Load(dst),
                    },
                    range,
                );
            }
            // A starred expression outside a display or call is a syntax error; lower
            // its operand so its effects remain visible.
            Expr::Starred(s) => self.expr_to(&s.value, dst),
            Expr::Name(n) => self.name(&n.id, Access::Load(dst), range),
            Expr::Slice(s) => {
                let elements = [&s.lower, &s.upper, &s.step]
                    .into_iter()
                    .map(|b| match b {
                        Some(b) => Element::Value(self.expr(b)),
                        None => Element::Missing,
                    })
                    .collect();
                self.emit(
                    OpKind::Build {
                        kind: BuildKind::Slice,
                        elements,
                        dst,
                    },
                    range,
                );
            }
            Expr::IpyEscapeCommand(_) => self.gap(GapKind::IpyEscape, Some(dst), range),
        }
    }

    /// Make an unknown-to-the-frontend static description of an annotation.
    fn static_expr(&mut self, scope: ScopeId, x: &Expr) -> StaticExpr {
        match x {
            Expr::Name(n) => StaticExpr::Name(n.id.clone(), self.table.resolve(scope, &n.id).0),
            Expr::Attribute(a) => StaticExpr::Attribute(
                Box::new(self.static_expr(scope, &a.value)),
                a.attr.id.clone(),
            ),
            Expr::Subscript(s) => {
                let base = Box::new(self.static_expr(scope, &s.value));
                let index = match &*s.slice {
                    Expr::Tuple(t) => t.elts.iter().map(|e| self.static_expr(scope, e)).collect(),
                    e => vec![self.static_expr(scope, e)],
                };
                StaticExpr::Subscript(base, index)
            }
            Expr::Tuple(t) => {
                StaticExpr::Sequence(t.elts.iter().map(|e| self.static_expr(scope, e)).collect())
            }
            Expr::List(l) => {
                StaticExpr::Sequence(l.elts.iter().map(|e| self.static_expr(scope, e)).collect())
            }
            Expr::StringLiteral(s) => StaticExpr::Literal(Literal::Str(s.value.to_str().into())),
            Expr::NumberLiteral(n) => match &n.value {
                Number::Int(i) => StaticExpr::Literal(Literal::Int(i.as_i64())),
                _ => StaticExpr::Other(n.range),
            },
            Expr::UnaryOp(u)
                if u.op == ruff_python_ast::UnaryOp::USub
                    && let Expr::NumberLiteral(n) = &*u.operand
                    && let Number::Int(i) = &n.value =>
            {
                StaticExpr::Literal(Literal::Int(i.as_i64().map(|i| -i)))
            }
            Expr::BooleanLiteral(b) => StaticExpr::Literal(Literal::Bool(b.value)),
            Expr::NoneLiteral(_) => StaticExpr::Literal(Literal::None),
            Expr::EllipsisLiteral(_) => StaticExpr::Literal(Literal::Ellipsis),
            _ => StaticExpr::Other(x.range()),
        }
    }
}

/// Collects the interpolated expressions of an f-string or t-string in evaluation order,
/// including those nested in format specifications.
struct Interpolations<'a, 'b>(&'b mut Vec<&'a Expr>);

impl<'a> Visitor<'a> for Interpolations<'a, '_> {
    fn visit_interpolated_string_element(&mut self, x: &'a InterpolatedStringElement) {
        if let InterpolatedStringElement::Interpolation(i) = x {
            self.0.push(&i.expression);
            if let Some(spec) = &i.format_spec {
                for e in &spec.elements {
                    self.visit_interpolated_string_element(e);
                }
            }
        }
    }

    fn visit_expr(&mut self, x: &'a Expr) {
        walk_expr(self, x)
    }
}

/// CPython compiles `while True:` and `while 1:` without a test.
fn is_truthy_constant(x: &Expr) -> bool {
    match x {
        Expr::BooleanLiteral(b) => b.value,
        Expr::NumberLiteral(n) => matches!(&n.value, Number::Int(i) if i.as_i64() != Some(0)),
        _ => false,
    }
}
