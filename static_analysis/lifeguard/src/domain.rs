/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The lazy-import effect domain: a transfer function per shared operation that tracks
//! value origins and reports import-time effects.
//!
//! Mutation is judged by the owner of the mutated object, never by the syntax of the
//! target. Rebinding a name is a local write; storing into an attribute or item of an
//! object owned by another module, or calling a mutating method on it, is an effect.
//! Calls to project functions are applied through [`Analyzer::summarize`], which runs
//! the same engine over the callee's region with the caller's cells.

use pyrefly_static_analysis::Access;
use pyrefly_static_analysis::Arg;
use pyrefly_static_analysis::BinaryOp;
use pyrefly_static_analysis::Binding;
use pyrefly_static_analysis::BuildKind;
use pyrefly_static_analysis::Cx;
use pyrefly_static_analysis::Domain;
use pyrefly_static_analysis::Element;
use pyrefly_static_analysis::ImportItem;
use pyrefly_static_analysis::Literal;
use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::Name;
use pyrefly_static_analysis::OpId;
use pyrefly_static_analysis::OpKind;
use pyrefly_static_analysis::Param;
use pyrefly_static_analysis::ParamKind;
use pyrefly_static_analysis::Presence;
use pyrefly_static_analysis::Program;
use pyrefly_static_analysis::Reg;
use pyrefly_static_analysis::ScopeId;
use pyrefly_static_analysis::SymbolId;
use pyrefly_static_analysis::SymbolOrigin;
use pyrefly_static_analysis::Trigger;

use crate::Kind;
use crate::analyzer::Activation;
use crate::analyzer::Analyzer;
use crate::analyzer::Summary;
use crate::known;
use crate::known::Iteration;
use crate::known::Known;
use crate::value::Accessors;
use crate::value::ClassInfo;
use crate::value::FuncRef;
use crate::value::Origin;
use crate::value::Value;

/// How calls are treated.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Calls to project functions run their bodies through summaries.
    Execute,
    /// Calls produce unknown values without running anything. Used to scan function
    /// bodies for effects that matter wherever they occur.
    Scan,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    Report {
        kind: Kind,
        message: String,
    },
    /// A parameter of this or an enclosing activation is mutated: the parameter's object
    /// itself (`Origin::Param`) or an object reached from it (`Origin::Member`).
    MutatesParam(Origin),
    /// A parameter (as for `MutatesParam`) is iterated or awaited, which runs it if the
    /// argument is a generator or coroutine.
    ConsumesParam(Origin),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Finding {
    pub start: u32,
    pub end: u32,
    pub event: Event,
}

/// Who owns an object, relative to the module being imported.
enum Owner {
    Local,
    Param(Origin),
    /// Another module; the string describes the object.
    External(String),
    Unknown,
}

/// How an operation mutates objects.
#[derive(Clone, Copy)]
enum Mutation<'s> {
    /// The operation mutates the object; the string describes the operation.
    Definite(&'s str),
    /// The operation mutates the object if its type is mutable.
    Possible(&'s str),
    /// A callee mutates the parameter this object is passed to.
    Argument,
}

/// The arguments of one call, as values.
#[derive(Clone, Default)]
struct CallArgs {
    /// Positional arguments before any starred argument.
    positional: Vec<Value>,
    keywords: Vec<(Name, Value)>,
    /// What `*` and `**` arguments, and positional arguments after them, may supply to
    /// any parameter.
    spread: Option<Value>,
}

impl CallArgs {
    fn new(cx: &Cx<'_, LifeguardDomain<'_>>, args: &[Arg]) -> Self {
        let mut out = Self::default();
        for arg in args {
            match arg {
                Arg::Positional(r) if out.spread.is_none() => {
                    out.positional.push(cx.reg(*r).clone())
                }
                Arg::Positional(r) => out
                    .spread
                    .get_or_insert_default()
                    .extend(cx.reg(*r).clone()),
                Arg::Star(r) | Arg::DoubleStar(r) => out
                    .spread
                    .get_or_insert_default()
                    .extend(cx.reg(*r).flat_map(|o| Value::of(o.member(None)))),
                Arg::Keyword(name, r) => out.keywords.push((name.clone(), cx.reg(*r).clone())),
            }
        }
        out
    }

    fn positional(positional: Vec<Value>) -> Self {
        Self {
            positional,
            ..Self::default()
        }
    }

    fn with_receiver(&self, receiver: &Value) -> Self {
        let mut out = self.clone();
        out.positional.insert(0, receiver.clone());
        out
    }

    /// The argument at `index` or passed as keyword `name`, if it is known exactly.
    fn get(&self, index: usize, name: &str) -> Option<&Value> {
        self.positional.get(index).or_else(|| {
            self.keywords
                .iter()
                .find(|(n, _)| n.as_str() == name)
                .map(|(_, v)| v)
        })
    }

    fn all(&self) -> impl Iterator<Item = &Value> {
        self.positional
            .iter()
            .chain(self.keywords.iter().map(|(_, v)| v))
            .chain(&self.spread)
    }
}

pub struct LifeguardDomain<'a> {
    analyzer: &'a Analyzer<'a>,
    program: &'a Program,
    /// The module whose import is executing this code.
    importer: ModuleName,
    mode: Mode,
}

impl Domain for LifeguardDomain<'_> {
    type Value = Value;
    type State = ();
    type Diagnostic = Finding;

    fn top(&self) -> Value {
        Value::unknown()
    }

    fn unbound(&self) -> Value {
        Value::default()
    }

    fn join(&self, a: &Value, b: &Value) -> Value {
        a.join(b)
    }

    fn join_state(&self, _: &(), _: &()) {}

    /// Testing an instance's truth runs its `__bool__` or `__len__`.
    fn assume(&self, cond: Reg, _: bool, cx: &mut Cx<'_, Self>) -> bool {
        let value = cx.reg(cond).clone();
        self.protocol(cx, &value, &["__bool__", "__len__"], &CallArgs::default());
        true
    }

    fn transfer(&self, op: OpId, kind: &OpKind, cx: &mut Cx<'_, Self>) {
        match kind {
            OpKind::Name {
                symbol,
                access: Access::Store(_) | Access::Delete,
            } => self.rebind(cx, *symbol),
            OpKind::Name { .. } => {}
            OpKind::Attribute {
                object,
                attr,
                access,
            } => {
                let object = cx.reg(*object).clone();
                match access {
                    Access::Load(dst) => {
                        let value = self.load_attr(cx, &object, attr);
                        cx.set_reg(*dst, value);
                    }
                    Access::Store(src) => {
                        let value = cx.reg(*src).clone();
                        self.escape(cx, &value);
                        self.store_attr(cx, &object, attr, Some(value));
                    }
                    Access::Delete => self.store_attr(cx, &object, attr, None),
                }
            }
            OpKind::Subscript {
                object,
                index,
                access,
            } => {
                let object = cx.reg(*object).clone();
                let index = cx.reg(*index).clone();
                // `sys.modules` is the import system's state: any access other than a read
                // that cannot fail depends on import order, and the module read is opaque.
                let (sys_modules, object): (Vec<Origin>, Vec<Origin>) =
                    object.iter().cloned().partition(is_sys_modules);
                let object: Value = object.into_iter().collect();
                let (hook, hook_args) = match access {
                    Access::Load(_) => ("__getitem__", vec![index.clone()]),
                    Access::Store(src) => {
                        ("__setitem__", vec![index.clone(), cx.reg(*src).clone()])
                    }
                    Access::Delete => ("__delitem__", vec![index.clone()]),
                };
                let hooked = self.protocol(cx, &object, &[hook], &CallArgs::positional(hook_args));
                let safe_read = matches!(access, Access::Load(_))
                    && index.as_str().is_some_and(|k| self.loaded(k));
                if !sys_modules.is_empty() && !safe_read {
                    self.report(
                        cx,
                        Kind::SysModulesAccess,
                        "accesses `sys.modules` with a key that may not be imported yet",
                    );
                }
                match access {
                    Access::Load(dst) => {
                        let mut value = object.flat_map(|o| Value::of(o.member(None)));
                        value.extend(hooked);
                        if !sys_modules.is_empty() {
                            value.extend(Value::unknown());
                        }
                        cx.set_reg(*dst, value);
                    }
                    Access::Store(src) => {
                        let value = cx.reg(*src).clone();
                        self.escape(cx, &value);
                        self.mutate(cx, &object, Mutation::Definite("stores item"));
                    }
                    Access::Delete => self.mutate(cx, &object, Mutation::Definite("deletes item")),
                }
            }
            OpKind::Literal { value, dst } => {
                let origin = match value {
                    Literal::Str(s) => Origin::Str(s.clone()),
                    _ => Origin::Const,
                };
                cx.set_reg(*dst, Value::of(origin));
            }
            OpKind::Build {
                kind,
                elements,
                dst,
            } => {
                for element in elements {
                    self.escape_element(cx, element);
                }
                let origin = match kind {
                    BuildKind::String | BuildKind::Slice => Origin::Const,
                    _ => Origin::Object(self.importer),
                };
                cx.set_reg(*dst, Value::of(origin));
            }
            OpKind::Append { element, .. } => self.escape_element(cx, element),
            OpKind::Call { func, args, dst } => {
                let args = CallArgs::new(cx, args);
                let func = cx.reg(*func).clone();
                let value = func.flat_map(|o| self.call(cx, o, &args));
                cx.set_reg(*dst, value);
            }
            OpKind::Unary { operand, dst, .. } => {
                let operand = cx.reg(*operand).clone();
                let mut value = self.protocol(cx, &operand, UNARY_METHODS, &CallArgs::default());
                value.extend(Value::of(Origin::Const));
                cx.set_reg(*dst, value);
            }
            OpKind::Binary {
                op,
                left,
                right,
                dst,
            } => {
                let names = match op {
                    BinaryOp::Compare(_) => COMPARISON_METHODS,
                    BinaryOp::Arith(_) | BinaryOp::InPlace(_) => ARITHMETIC_METHODS,
                };
                let (l, r) = (cx.reg(*left).clone(), cx.reg(*right).clone());
                let mut hooked =
                    self.protocol(cx, &l, names, &CallArgs::positional(vec![r.clone()]));
                hooked.extend(self.protocol(cx, &r, names, &CallArgs::positional(vec![l])));
                let mut value = match op {
                    BinaryOp::Compare(_) => Value::of(Origin::Const),
                    BinaryOp::Arith(_) => Value::of(Origin::Object(self.importer)),
                    // In-place operators mutate mutable receivers and rebind immutable ones;
                    // without types, a non-local receiver may be mutated.
                    BinaryOp::InPlace(_) => {
                        let left = cx.reg(*left).clone();
                        let mutation = Mutation::Possible("applies an in-place operator to");
                        self.mutate(cx, &left, mutation);
                        left
                    }
                };
                value.extend(hooked);
                cx.set_reg(*dst, value);
            }
            // Iterating a generator, awaiting a coroutine, or delegating to either with
            // `yield from` runs its body.
            OpKind::Iter {
                iterable: value, ..
            }
            | OpKind::Await { value, .. }
            | OpKind::Yield {
                value: Some(value),
                from: true,
                ..
            } => {
                let value = cx.reg(*value).clone();
                self.consume(cx, &value);
                if matches!(kind, OpKind::Iter { .. }) {
                    let names = ["__iter__", "__next__"];
                    let iterator = self.protocol(cx, &value, &names, &CallArgs::default());
                    self.consume(cx, &iterator);
                }
            }
            OpKind::Yield {
                value: Some(value), ..
            } => {
                let value = cx.reg(*value).clone();
                self.escape(cx, &value);
            }
            OpKind::MakeFunction { region, dst, .. } => {
                let func = FuncRef {
                    module: cx.program().module,
                    region: *region,
                };
                cx.set_reg(*dst, Value::of(Origin::Function(func)));
            }
            OpKind::MakeClass {
                name,
                scope,
                bases,
                dst,
            } => {
                let class = self.make_class(cx, op, name, *scope, bases);
                cx.set_reg(*dst, class);
            }
            OpKind::Import { module, item, dst } => {
                let value = match item {
                    ImportItem::Root => {
                        self.module_value(ModuleName::from_name(&module.first_component()))
                    }
                    ImportItem::Module => self.module_value(*module),
                    ImportItem::Name(name) => self.analyzer.imported_name(*module, name),
                };
                cx.set_reg(*dst, value);
            }
            OpKind::ContextEnter {
                manager, is_async, ..
            } => {
                let manager = cx.reg(*manager).clone();
                let method = if *is_async { "__aenter__" } else { "__enter__" };
                self.context(cx, &manager, method);
            }
            OpKind::ContextExit {
                manager, is_async, ..
            } => {
                let manager = cx.reg(*manager).clone();
                if manager.iter().all(|o| matches!(o, Origin::Instance(..))) {
                    let method = if *is_async { "__aexit__" } else { "__exit__" };
                    self.context(cx, &manager, method);
                }
            }
            // The remaining results (unpacked items, iterator items, parameters, sent
            // values, gaps) take the engine's defaults, which are `top` when not tracked.
            OpKind::Unpack { .. }
            | OpKind::Next { .. }
            | OpKind::Param { .. }
            | OpKind::Yield { value: None, .. }
            | OpKind::Gap { .. } => {}
        }
    }
}

impl<'a> LifeguardDomain<'a> {
    pub fn new(
        analyzer: &'a Analyzer<'a>,
        program: &'a Program,
        importer: ModuleName,
        mode: Mode,
    ) -> Self {
        Self {
            analyzer,
            program,
            importer,
            mode,
        }
    }

    fn report(&self, cx: &mut Cx<'_, Self>, kind: Kind, message: impl Into<String>) {
        self.event(
            cx,
            Event::Report {
                kind,
                message: message.into(),
            },
        );
    }

    fn event(&self, cx: &mut Cx<'_, Self>, event: Event) {
        let range = cx.range();
        cx.report(Finding {
            start: range.start().into(),
            end: range.end().into(),
            event,
        });
    }

    fn unknown_call(&self, cx: &mut Cx<'_, Self>, what: &str) -> Value {
        let line = self.program.line_col(cx.range()).0;
        let reason = format!("{}:{line}: {what}", self.program.module);
        self.report(cx, Kind::UnknownCall, reason);
        Value::unknown()
    }

    /// Rebinding a local or closure cell is local. Rebinding a global from a function
    /// changes the defining module's namespace, which is another module's state unless
    /// that module is the one being imported.
    fn rebind(&self, cx: &mut Cx<'_, Self>, symbol: SymbolId) {
        if self.program.owner(symbol) == Some(Program::MODULE) && cx.region() != Program::MODULE {
            let namespace = Value::of(Origin::Module(self.program.module));
            let action = format!("rebinds global `{}`", self.program.symbol(symbol).name);
            self.mutate(cx, &namespace, Mutation::Definite(&action));
        }
    }

    /// True if `sys.modules[key]` cannot fail because the module is always loaded or is
    /// the reading module or one of its packages.
    fn loaded(&self, key: &str) -> bool {
        let module = self.program.module;
        matches!(key, "__main__" | "sys" | "builtins")
            || module.as_str() == key
            || module
                .as_str()
                .strip_prefix(key)
                .is_some_and(|rest| rest.starts_with('.'))
    }

    fn module_value(&self, module: ModuleName) -> Value {
        Value::of(if self.analyzer.contains(module) {
            Origin::Module(module)
        } else {
            Origin::Imported(module)
        })
    }

    fn owner(&self, origin: &Origin) -> Owner {
        let module_owner = |m: ModuleName, what: &str| {
            if m == self.importer {
                Owner::Local
            } else {
                Owner::External(format!("{what} of module `{m}`"))
            }
        };
        match origin {
            Origin::Const | Origin::Str(_) | Origin::Identity => Owner::Local,
            Origin::Object(m) | Origin::Instance(_, m) => module_owner(*m, "an object"),
            Origin::Function(f)
            | Origin::Bound(f, _)
            | Origin::StaticMethod(f)
            | Origin::ClassMethod(f)
            | Origin::Generator(f, _) => module_owner(f.module, "a function"),
            Origin::Property(accessors) => {
                let accessor = accessors.getter.or(accessors.setter).or(accessors.deleter);
                let module = accessor.map_or(self.program.module, |f| f.module);
                module_owner(module, "a property")
            }
            Origin::Class(c) => module_owner(c.module, &format!("class `{}`", c.name)),
            Origin::Module(m) => module_owner(*m, "the namespace"),
            Origin::Imported(path) => Owner::External(format!("imported `{path}`")),
            Origin::Builtin(name) => Owner::External(format!("builtin `{name}`")),
            // Attributes and items of objects are not tracked: an object reached from a
            // parameter or an import keeps that owner, and one reached from anything
            // else may be any object at all.
            Origin::Member { root, .. } => match self.owner(root) {
                Owner::Param(_) => Owner::Param(origin.clone()),
                Owner::External(what) => Owner::External(format!("an object reached from {what}")),
                Owner::Local | Owner::Unknown => Owner::Unknown,
            },
            Origin::Param(..) => Owner::Param(origin.clone()),
            Origin::Unknown => Owner::Unknown,
        }
    }

    /// Report the mutation of every object in `value`.
    fn mutate(&self, cx: &mut Cx<'_, Self>, value: &Value, mutation: Mutation<'_>) {
        for origin in value.iter() {
            let (kind, message) = match (self.owner(origin), mutation) {
                (Owner::Local, _) => continue,
                (Owner::Param(param), _) => {
                    self.event(cx, Event::MutatesParam(param));
                    continue;
                }
                (Owner::External(what), Mutation::Definite(action)) => {
                    (Kind::ImportedMutation, format!("{action} on {what}"))
                }
                (Owner::External(what), Mutation::Possible(action)) => (
                    Kind::UnknownMutation,
                    format!("{action} {what}, which may mutate it"),
                ),
                (Owner::External(what), Mutation::Argument) => (
                    Kind::ImportedArgument,
                    format!("passes {what} to a parameter that the callee mutates"),
                ),
                (Owner::Unknown, Mutation::Definite(action)) => (
                    Kind::UnknownMutation,
                    format!("{action} on a value the analysis cannot resolve"),
                ),
                (Owner::Unknown, Mutation::Possible(action)) => (
                    Kind::UnknownMutation,
                    format!("{action} a value the analysis cannot resolve"),
                ),
                (Owner::Unknown, Mutation::Argument) => (
                    Kind::UnknownMutation,
                    "passes a value to a parameter whose objects the callee mutates, and the \
                     analysis cannot resolve who owns them"
                        .to_owned(),
                ),
            };
            self.report(cx, kind, message);
        }
    }

    fn load_attr(&self, cx: &mut Cx<'_, Self>, object: &Value, attr: &Name) -> Value {
        let name = CallArgs::positional(vec![Value::of(Origin::Str(attr.as_str().into()))]);
        object.flat_map(|o| match o {
            Origin::Module(m) => self.analyzer.module_attr(*m, attr),
            Origin::Class(c) => match c.lookup(attr) {
                Some(member) => self.bind_member(cx, o, c, &member, attr),
                None => Value::of(o.member(Some(attr))),
            },
            // `__getattribute__` runs on every read, and `__getattr__` when the class
            // does not define the attribute.
            Origin::Instance(c, _) => {
                let me = Value::of(o.clone());
                let mut value = self.protocol(cx, &me, &["__getattribute__"], &name);
                value.extend(match c.lookup(attr) {
                    Some(member) => self.bind_member(cx, o, c, &member, attr),
                    None => {
                        let mut v = self.protocol(cx, &me, &["__getattr__"], &name);
                        v.extend(Value::of(o.member(Some(attr))));
                        v
                    }
                });
                value
            }
            _ => Value::of(o.member(Some(attr))),
        })
    }

    /// Read a class attribute through `owner` (the class or an instance of it),
    /// applying the descriptor protocol for functions, `staticmethod`, `classmethod` and
    /// `property`.
    fn bind_member(
        &self,
        cx: &mut Cx<'_, Self>,
        owner: &Origin,
        class: &ClassInfo,
        member: &Value,
        attr: &Name,
    ) -> Value {
        let instance = matches!(owner, Origin::Instance(..));
        let me = Value::of(owner.clone());
        member.flat_map(|m| match m {
            Origin::Function(f) if instance => Value::of(Origin::Bound(*f, Box::new(me.clone()))),
            Origin::StaticMethod(f) => Value::of(Origin::Function(*f)),
            Origin::ClassMethod(f) => {
                let class = Value::of(Origin::Class(Box::new(class.clone())));
                Value::of(Origin::Bound(*f, Box::new(class)))
            }
            Origin::Property(accessors) if instance => match accessors.getter {
                Some(getter) => self.invoke(cx, getter, &CallArgs::positional(vec![me.clone()])),
                None => self.unknown_call(cx, &format!("reads property `{attr}` without a getter")),
            },
            // The instance's own attributes may shadow class data.
            _ if instance => Value::of(m.clone()).join(&Value::of(owner.member(Some(attr)))),
            _ => Value::of(m.clone()),
        })
    }

    /// The bound special method `name` that Python looks up on the type of `owner`.
    /// `__new__` and `__init_subclass__` are implicitly static and class methods.
    fn special(
        &self,
        cx: &mut Cx<'_, Self>,
        owner: &Origin,
        class: &ClassInfo,
        name: &str,
    ) -> Option<Value> {
        let name = Name::new(name);
        let member = class.lookup(&name)?;
        Some(match name.as_str() {
            "__new__" | "__init_subclass__" => member.flat_map(|m| match m {
                Origin::Function(f) | Origin::StaticMethod(f) | Origin::ClassMethod(f) => {
                    Value::of(Origin::Bound(*f, Box::new(Value::of(owner.clone()))))
                }
                other => Value::of(other.clone()),
            }),
            _ => self.bind_member(cx, owner, class, &member, &name),
        })
    }

    /// Run the special methods in `names` that the classes of the instances in `value`
    /// define, as an operator, iteration or builtin does, and join their results.
    fn protocol(
        &self,
        cx: &mut Cx<'_, Self>,
        value: &Value,
        names: &[&str],
        args: &CallArgs,
    ) -> Value {
        let mut out = Value::default();
        for origin in value.iter() {
            let Origin::Instance(c, _) = origin else {
                continue;
            };
            for name in names {
                if let Some(method) = self.special(cx, origin, c, name) {
                    for f in method.iter() {
                        out.extend(self.call(cx, f, args));
                    }
                }
            }
        }
        out
    }

    /// Store (`value = Some`) or delete (`value = None`) an attribute.
    fn store_attr(&self, cx: &mut Cx<'_, Self>, object: &Value, attr: &Name, value: Option<Value>) {
        let action = match value {
            Some(_) => format!("stores attribute `{attr}`"),
            None => format!("deletes attribute `{attr}`"),
        };
        for origin in object.iter() {
            match origin {
                Origin::Imported(p)
                    if p.as_str() == "builtins" && attr.as_str() == "__import__" =>
                {
                    self.report(
                        cx,
                        Kind::BuiltinsImportOverride,
                        "replaces or deletes `builtins.__import__`",
                    );
                }
                Origin::Imported(p) if p.as_str() == "sys" && attr.as_str() == "modules" => {
                    self.report(cx, Kind::SysModulesAccess, "replaces `sys.modules`");
                }
                Origin::Instance(c, _) => self.store_instance_attr(cx, origin, c, attr, &value),
                // Class values and module globals are snapshots; a write through the class
                // or module object would make later dispatch and reads stale.
                Origin::Class(c) if c.module == self.importer => {
                    self.report(
                        cx,
                        Kind::UnknownMutation,
                        format!(
                            "{action} on class `{}` after creation, which is not tracked",
                            c.name
                        ),
                    );
                }
                Origin::Module(m) if *m == self.importer => {
                    self.report(
                        cx,
                        Kind::UnknownMutation,
                        format!("{action} through the module object, which is not tracked"),
                    );
                }
                _ => self.mutate(cx, &Value::of(origin.clone()), Mutation::Definite(&action)),
            }
        }
    }

    fn store_instance_attr(
        &self,
        cx: &mut Cx<'_, Self>,
        instance: &Origin,
        class: &ClassInfo,
        attr: &Name,
        value: &Option<Value>,
    ) {
        let hook = if value.is_some() {
            "__setattr__"
        } else {
            "__delattr__"
        };
        if let Some(hook) = self.special(cx, instance, class, hook) {
            let mut args = vec![Value::of(Origin::Str(attr.as_str().into()))];
            args.extend(value.clone());
            let args = CallArgs::positional(args);
            for f in hook.iter() {
                self.call(cx, f, &args);
            }
            return;
        }
        let me = Value::of(instance.clone());
        let action = format!("stores attribute `{attr}`");
        let Some(member) = class.lookup(attr) else {
            return self.mutate(cx, &me, Mutation::Definite(&action));
        };
        for m in member.iter() {
            match m {
                Origin::Property(accessors) => {
                    let accessor = if value.is_some() {
                        accessors.setter
                    } else {
                        accessors.deleter
                    };
                    match accessor {
                        Some(f) => {
                            let mut args = vec![me.clone()];
                            args.extend(value.clone());
                            self.invoke(cx, f, &CallArgs::positional(args));
                        }
                        None => {
                            self.unknown_call(cx, &format!("writes property `{attr}`, which has no accessor for it"));
                        }
                    }
                }
                // Dispatch reads methods from the class snapshot, so an instance attribute
                // that shadows one is not tracked.
                Origin::Function(_)
                | Origin::Bound(..)
                | Origin::StaticMethod(_)
                | Origin::ClassMethod(_)
                | Origin::Class(_)
                | Origin::Builtin(_)
                | Origin::Identity => self.report(
                    cx,
                    Kind::UnknownMutation,
                    format!("shadows method `{attr}` of class `{}` on an instance, which is not tracked", class.name),
                ),
                _ => self.mutate(cx, &me, Mutation::Definite(&action)),
            }
        }
    }

    /// Call one callee origin.
    fn call(&self, cx: &mut Cx<'_, Self>, callee: &Origin, args: &CallArgs) -> Value {
        match callee {
            Origin::Function(f) | Origin::StaticMethod(f) => self.invoke(cx, *f, args),
            Origin::Bound(f, receiver) => self.invoke(cx, *f, &args.with_receiver(receiver)),
            Origin::Member {
                root,
                attr: Some(name),
                ..
            } => self.call_method(cx, callee, root, name, args),
            Origin::Class(c) => self.construct(cx, c, args),
            Origin::Builtin(name) => self.call_builtin(cx, name, args),
            Origin::Imported(path) => self.call_external(cx, *path, args),
            Origin::Identity => self.identity(args),
            Origin::Instance(c, _) => match self.special(cx, callee, c, "__call__") {
                Some(v) => v.flat_map(|f| self.call(cx, f, args)),
                None => self.unknown_call(cx, &format!("calls an instance of `{}`", c.name)),
            },
            Origin::Param(..) => self.unknown_call(cx, "calls a parameter"),
            _ => self.unknown_call(cx, "calls a value the analysis cannot resolve"),
        }
    }

    /// Call `root.name(...)`, where `method` is the `Member` origin of the attribute.
    fn call_method(
        &self,
        cx: &mut Cx<'_, Self>,
        method: &Origin,
        root: &Origin,
        name: &Name,
        args: &CallArgs,
    ) -> Value {
        // The object the method is called on: `root` itself, or an object reached from
        // it when the attribute chain is longer (`root.a.name`).
        let receiver = match method {
            Origin::Member { deep: true, .. } => Value::of(root.member(None).member(None)),
            _ => Value::of(root.clone()),
        };
        let name = name.as_str();
        match root {
            Origin::Property(accessors) if matches!(name, "setter" | "getter" | "deleter") => {
                match args.positional.as_slice() {
                    [f] if f.iter().all(|o| matches!(o, Origin::Function(_))) => f.flat_map(|o| {
                        let Origin::Function(f) = o else {
                            unreachable!("checked above")
                        };
                        let mut accessors = accessors.clone();
                        match name {
                            "getter" => accessors.getter = Some(*f),
                            "setter" => accessors.setter = Some(*f),
                            _ => accessors.deleter = Some(*f),
                        }
                        Value::of(Origin::Property(accessors))
                    }),
                    _ => self.unknown_call(
                        cx,
                        &format!("calls `property.{name}` with an unresolved function"),
                    ),
                }
            }
            // Without types, builtin method names decide: a known mutator mutates its
            // receiver, a known pure method does nothing, and anything else is unknown.
            Origin::Object(_)
            | Origin::Const
            | Origin::Str(_)
            | Origin::Param(..)
            | Origin::Imported(_) => {
                if known::is_mutating_method(name) || known::is_pure_method(name) {
                    for value in args.all() {
                        if known::is_storing_method(name) {
                            self.escape(cx, value);
                        } else {
                            self.consume(cx, value);
                        }
                    }
                    self.callback(cx, args.get(usize::MAX, "key"));
                }
                if known::is_mutating_method(name) {
                    self.mutate(
                        cx,
                        &receiver,
                        Mutation::Definite(&format!("calls `{name}`")),
                    );
                    receiver.flat_map(|o| Value::of(o.member(None)))
                } else if known::is_pure_method(name) && !matches!(root, Origin::Imported(_)) {
                    match name {
                        "copy" => Value::of(Origin::Object(self.importer)),
                        "get" | "keys" | "values" | "items" => {
                            receiver.flat_map(|o| Value::of(o.member(None)))
                        }
                        _ => Value::of(Origin::Const),
                    }
                } else {
                    if matches!(root, Origin::Param(..)) {
                        self.mutate(
                            cx,
                            &receiver,
                            Mutation::Definite(&format!("calls `{name}`")),
                        );
                    }
                    self.unknown_call(cx, &format!("calls unresolved method `{name}`"))
                }
            }
            _ => self.unknown_call(cx, &format!("calls unresolved method `{name}`")),
        }
    }

    fn identity(&self, args: &CallArgs) -> Value {
        match args.positional.first() {
            Some(v)
                if v.iter()
                    .all(|o| matches!(o, Origin::Function(_) | Origin::Class(_))) =>
            {
                v.clone()
            }
            _ => Value::of(Origin::Identity),
        }
    }

    fn call_builtin(&self, cx: &mut Cx<'_, Self>, name: &Name, args: &CallArgs) -> Value {
        match name.as_str() {
            "exec" => {
                self.report(cx, Kind::ExecCall, "calls `exec`");
                Value::unknown()
            }
            "eval" => {
                if args.get(0, "source").is_some_and(|v| v.as_str().is_some()) {
                    self.unknown_call(cx, "evaluates a string literal, which is not analyzed")
                } else {
                    self.report(cx, Kind::ExecCall, "calls `eval` on a computed string");
                    Value::unknown()
                }
            }
            "getattr" | "setattr" | "delattr" => {
                let object = args.get(0, "").cloned().unwrap_or_else(Value::unknown);
                let Some(attr) = args.get(1, "").and_then(|v| v.as_str().map(Name::new)) else {
                    let message = format!("calls `{name}` with a computed attribute name");
                    self.report(cx, Kind::ProhibitedCall, message);
                    return Value::unknown();
                };
                match name.as_str() {
                    "getattr" => self.load_attr(cx, &object, &attr),
                    "setattr" => {
                        let value = args.get(2, "").cloned().unwrap_or_else(Value::unknown);
                        self.store_attr(cx, &object, &attr, Some(value));
                        Value::of(Origin::Const)
                    }
                    _ => {
                        self.store_attr(cx, &object, &attr, None);
                        Value::of(Origin::Const)
                    }
                }
            }
            "property" => wrap_functions(args.get(0, "fget"), property),
            "staticmethod" => wrap_functions(args.get(0, ""), Origin::StaticMethod),
            "classmethod" => wrap_functions(args.get(0, ""), Origin::ClassMethod),
            // Dynamic namespaces, `super()` and iterator items are opaque.
            "globals" | "locals" | "vars" | "super" | "type" => Value::unknown(),
            "next" => {
                if let Some(iterator) = args.get(0, "") {
                    self.consume(cx, &iterator.clone());
                }
                Value::unknown()
            }
            _ => match known::builtin(name.as_str()) {
                Some(known) => {
                    for value in args.all() {
                        self.protocol(cx, value, CONVERSION_METHODS, &CallArgs::default());
                    }
                    if let Some((index, keyword, eager)) = known::builtin_callback(name.as_str()) {
                        let callback = args.get(index, keyword);
                        // `iter(callable, sentinel)` is the only form of `iter` with a callback.
                        let callback = match name.as_str() {
                            "iter" if args.positional.len() == 2 => args.positional.first(),
                            "iter" => None,
                            _ => callback,
                        };
                        if eager {
                            self.callback(cx, callback);
                        } else if callback.is_some_and(|v| v.iter().any(|o| *o != Origin::Const)) {
                            // Lazy callbacks run during a later iteration that the analysis
                            // does not connect back to them, so any callback is unknown.
                            self.unknown_call(
                                cx,
                                &format!("passes a callback to lazy `{name}`, whose calls are not tracked"),
                            );
                        }
                    }
                    let mut result = self.apply_known(cx, known, name.as_str(), args);
                    match known::builtin_iteration(name.as_str()) {
                        Iteration::Consumes => {
                            for value in args.all() {
                                self.consume(cx, &value.clone());
                            }
                        }
                        // The lazy iterator runs the generators it wraps when it is iterated.
                        Iteration::Wraps => {
                            for value in args.all() {
                                result.extend(
                                    value
                                        .iter()
                                        .filter(|o| matches!(o, Origin::Generator(..)))
                                        .cloned()
                                        .collect(),
                                );
                            }
                        }
                        Iteration::Ignores => {}
                    }
                    result
                }
                None => self.unknown_call(
                    cx,
                    &format!("calls unmodeled builtin or undefined name `{name}`"),
                ),
            },
        }
    }

    fn call_external(&self, cx: &mut Cx<'_, Self>, path: ModuleName, args: &CallArgs) -> Value {
        // A dataclass's generated `__init__` calls default factories, which construction
        // does not model.
        if path.as_str() == "dataclasses.field" && args.get(usize::MAX, "default_factory").is_some()
        {
            return self.unknown_call(
                cx,
                "declares a dataclass default factory, which is not modeled",
            );
        }
        if let Some(known) = known::external(path.as_str()) {
            if args
                .all()
                .any(|v| v.iter().any(|o| matches!(o, Origin::Generator(..))))
            {
                let what = format!("passes a generator to `{path}`, which may iterate it");
                return self.unknown_call(cx, &what);
            }
            return self.apply_known(cx, known, path.as_str(), args);
        }
        let (Some(owner), Some(method)) = (path.parent(), path.components().pop()) else {
            return self.unknown_call(cx, &format!("calls unresolved `{path}`"));
        };
        if owner.as_str() == "sys.modules" {
            self.report(cx, Kind::SysModulesAccess, format!("calls `{path}`"));
            Value::unknown()
        } else if known::is_mutating_method(method.as_str()) {
            let receiver = Value::of(Origin::Imported(owner));
            self.mutate(
                cx,
                &receiver,
                Mutation::Definite(&format!("calls `{method}`")),
            );
            Value::unknown()
        } else {
            self.unknown_call(cx, &format!("calls unresolved `{path}`"))
        }
    }

    fn apply_known(
        &self,
        cx: &mut Cx<'_, Self>,
        known: Known,
        name: &str,
        args: &CallArgs,
    ) -> Value {
        match known {
            Known::Const => Value::of(Origin::Const),
            Known::Fresh => Value::of(Origin::Object(self.importer)),
            Known::Element => {
                let mut items = Value::default();
                for v in args.positional.iter().chain(&args.spread) {
                    items.extend(v.flat_map(|o| Value::of(o.member(None))));
                }
                items
            }
            Known::Returns(index, keyword) => args
                .get(index, keyword)
                .cloned()
                .unwrap_or_else(Value::unknown),
            Known::Identity => self.identity(args),
            Known::Property => wrap_functions(args.get(0, "func"), property),
            Known::IdentityFactory => Value::of(Origin::Identity),
            Known::Prohibited => {
                self.report(cx, Kind::ProhibitedCall, format!("calls `{name}`"));
                Value::unknown()
            }
        }
    }

    /// Iterating a project generator or awaiting a project coroutine runs its body with
    /// the arguments it was created with. Iterating a parameter is recorded so that the
    /// caller runs the generator it passes.
    fn consume(&self, cx: &mut Cx<'_, Self>, value: &Value) {
        for origin in value.iter() {
            match origin {
                Origin::Generator(f, bound) => {
                    self.run(cx, *f, bound.clone());
                }
                Origin::Param(..) | Origin::Member { .. } => {
                    if let Owner::Param(param) = self.owner(origin) {
                        self.event(cx, Event::ConsumesParam(param));
                    }
                }
                _ => {}
            }
        }
    }

    /// A generator stored where the analysis does not follow it (a collection, an
    /// attribute, a yielded value) may run later at import time or not at all.
    fn escape(&self, cx: &mut Cx<'_, Self>, value: &Value) {
        if value.iter().any(|o| matches!(o, Origin::Generator(..))) {
            self.unknown_call(
                cx,
                "stores a generator or coroutine where its iteration is not tracked",
            );
        }
    }

    fn escape_element(&self, cx: &mut Cx<'_, Self>, element: &Element) {
        let regs = match element {
            Element::Value(r) | Element::DoubleStar(r) => vec![*r],
            Element::Pair(k, v) => vec![*k, *v],
            // Unpacking a generator into a display iterates it.
            Element::Star(r) => {
                let value = cx.reg(*r).clone();
                self.consume(cx, &value);
                Vec::new()
            }
            Element::Missing => Vec::new(),
        };
        for r in regs {
            let value = cx.reg(r).clone();
            self.escape(cx, &value);
        }
    }

    /// Run a callback that a builtin calls with arguments the analysis does not track.
    /// `None` is the usual "no callback" default.
    fn callback(&self, cx: &mut Cx<'_, Self>, value: Option<&Value>) {
        let args = CallArgs {
            spread: Some(Value::unknown()),
            ..CallArgs::default()
        };
        for origin in value.into_iter().flat_map(|v| v.iter()) {
            match origin {
                Origin::Const => {}
                // Calling a builtin on items the analysis does not track may run their
                // special methods (`str` runs `__str__`), unless the builtin has effects
                // of its own to report or calls none.
                Origin::Builtin(name)
                    if name.as_str() != "id"
                        && !matches!(
                            known::builtin(name.as_str()),
                            Some(Known::Prohibited) | None
                        ) =>
                {
                    let what = format!(
                        "passes builtin `{name}` as a callback, which may run special methods of untracked items"
                    );
                    self.unknown_call(cx, &what);
                }
                _ => {
                    self.call(cx, origin, &args);
                }
            }
        }
    }

    /// Run a context manager's `method`. The asynchronous methods are awaited, which
    /// runs the coroutine they return.
    fn context(&self, cx: &mut Cx<'_, Self>, manager: &Value, method: &str) {
        for origin in manager.iter() {
            let Origin::Instance(c, _) = origin else {
                self.unknown_call(cx, "enters a context manager the analysis cannot resolve");
                continue;
            };
            match self.special(cx, origin, c, method) {
                Some(v) => {
                    for f in v.iter() {
                        let result = self.call(cx, f, &CallArgs::default());
                        if method.starts_with("__a") {
                            self.consume(cx, &result);
                        }
                    }
                }
                None => {
                    self.unknown_call(cx, &format!("enters a context manager without `{method}`"));
                }
            }
        }
    }

    fn make_class(
        &self,
        cx: &mut Cx<'_, Self>,
        def: OpId,
        name: &Name,
        scope: ScopeId,
        bases: &[Arg],
    ) -> Value {
        // The namespace holds the class-body bindings themselves, not what a load would
        // read: a deleted or never-bound name is absent, not its global fallback.
        let members: Vec<(Name, Value)> = self
            .program
            .scope(scope)
            .symbols
            .iter()
            .filter_map(|(n, s)| {
                let binding = cx.frame().binding(self.program, *s)?;
                let value = match binding.presence {
                    Presence::Bound => binding.value.clone(),
                    Presence::Maybe => binding.value.join(&Value::unknown()),
                    Presence::Unbound => return None,
                };
                Some((n.clone(), value))
            })
            .collect();
        if members.iter().any(|(n, _)| n.as_str() == "__del__") {
            let message = format!("class `{name}` defines `__del__`");
            self.report(cx, Kind::CustomFinalizer, message);
        }
        let mut base_values = Vec::new();
        for arg in bases {
            match arg {
                Arg::Positional(r) => base_values.push(cx.reg(*r).clone()),
                Arg::Keyword(k, r) if k.as_str() == "metaclass" => {
                    let safe = cx.reg(*r).iter().all(|o| match o {
                        Origin::Imported(p) => p.as_str() == "abc.ABCMeta",
                        Origin::Builtin(b) => b.as_str() == "type",
                        _ => false,
                    });
                    if !safe {
                        self.unknown_call(
                            cx,
                            &format!("creates class `{name}` with an unresolved metaclass"),
                        );
                    }
                }
                _ => {
                    self.unknown_call(
                        cx,
                        &format!("creates class `{name}` with unmodeled class arguments"),
                    );
                }
            }
        }
        let class = Origin::Class(Box::new(ClassInfo {
            module: self.program.module,
            def,
            name: name.clone(),
            members,
            bases: base_values.clone(),
        }));
        // Creating a subclass runs the bases' metaclass and `__init_subclass__`.
        for base in &base_values {
            for origin in base.iter() {
                match origin {
                    Origin::Class(c) => {
                        if let Some(hook) = self.special(cx, &class, c, "__init_subclass__") {
                            for f in hook.iter() {
                                self.call(cx, f, &CallArgs::default());
                            }
                        }
                    }
                    Origin::Builtin(b) if known::is_safe_builtin_base(b.as_str()) => {}
                    Origin::Imported(p) if known::is_safe_external_base(p.as_str()) => {}
                    _ => {
                        self.unknown_call(
                            cx,
                            &format!("creates class `{name}` from an unresolved base"),
                        );
                    }
                }
            }
        }
        Value::of(class)
    }

    fn construct(&self, cx: &mut Cx<'_, Self>, class: &ClassInfo, args: &CallArgs) -> Value {
        let class_origin = Origin::Class(Box::new(class.clone()));
        let instance = Origin::Instance(Box::new(class.clone()), self.importer);
        let mut found = false;
        for (owner, method) in [(&class_origin, "__new__"), (&instance, "__init__")] {
            if let Some(v) = self.special(cx, owner, class, method) {
                found = true;
                for f in v.iter() {
                    self.call(cx, f, args);
                }
            }
        }
        if !found && !self.bases_resolved(class) {
            self.unknown_call(
                cx,
                &format!(
                    "constructs `{}`, whose constructor is inherited from an unresolved base",
                    class.name
                ),
            );
        }
        Value::of(instance)
    }

    fn bases_resolved(&self, class: &ClassInfo) -> bool {
        class.bases.iter().all(|base| {
            !base.is_empty()
                && base.iter().all(|o| match o {
                    Origin::Class(c) => self.bases_resolved(c),
                    Origin::Builtin(b) => known::is_safe_builtin_base(b.as_str()),
                    Origin::Imported(p) => known::is_safe_external_base(p.as_str()),
                    _ => false,
                })
        })
    }

    /// Call a project function: bind the arguments, then apply its summary.
    fn invoke(&self, cx: &mut Cx<'_, Self>, func: FuncRef, args: &CallArgs) -> Value {
        if self.mode == Mode::Scan {
            return Value::unknown();
        }
        let region = self.analyzer.program(func.module).region(func.region);
        let bound = bind(&region.params, args);
        match region.trigger {
            Trigger::OnCall => self.run(cx, func, bound),
            // Calling a generator or coroutine function creates it without running it.
            Trigger::OnIteration | Trigger::OnAwait => Value::of(Origin::Generator(func, bound)),
            Trigger::OnImport => unreachable!("only module regions run on import"),
        }
    }

    /// Run a function body with `bound` arguments (missing ones are unknown) and apply
    /// its summary to the caller.
    fn run(&self, cx: &mut Cx<'_, Self>, func: FuncRef, mut bound: Vec<Value>) -> Value {
        if self.mode == Mode::Scan {
            return Value::unknown();
        }
        let callee = self.analyzer.program(func.module);
        bound.resize(callee.region(func.region).params.len(), Value::unknown());
        let same_program = func.module == self.program.module;
        // Cells are read at invocation time: the callee's free symbols, plus every global
        // that some function of its module reads, since the callee may call those
        // functions.
        let exit_globals = (!same_program)
            .then(|| self.analyzer.exit_globals(func.module))
            .flatten();
        let mut symbols: Vec<SymbolId> = callee
            .free_symbols(func.region)
            .iter()
            .chain(self.analyzer.function_globals(func.module))
            .copied()
            .collect();
        symbols.sort();
        symbols.dedup();
        let cells = symbols
            .into_iter()
            .filter_map(|s| {
                let binding = if let Some(builtin) = builtin_binding(callee, s) {
                    builtin
                } else if same_program {
                    cx.frame().binding(callee, s)?.clone()
                } else {
                    // Another module's function sees that module's finished globals;
                    // closure cells of a function that escaped another module are unknown.
                    exit_globals.as_ref()?.get(&s)?.clone()
                };
                Some((s, binding))
            })
            .collect();
        let summary = self.analyzer.summarize(Activation {
            func,
            importer: self.importer,
            cells,
        });
        self.apply(cx, func, &bound, &summary, same_program)
    }

    fn apply(
        &self,
        cx: &mut Cx<'_, Self>,
        func: FuncRef,
        bound: &[Value],
        summary: &Summary,
        same_program: bool,
    ) -> Value {
        if let Some(reason) = summary.unsafe_reasons.first() {
            self.report(cx, Kind::UnsafeCall, reason.clone());
        }
        if let Some(reason) = summary.unknown_reasons.first() {
            self.report(cx, Kind::UnknownCall, reason.clone());
        }
        for param in &summary.consumed {
            if is_own_param(param, func) {
                let argument = Value::of(param.clone()).substitute(func, bound);
                self.consume(cx, &argument);
            } else {
                self.event(cx, Event::ConsumesParam(param.clone()));
            }
        }
        for param in &summary.mutated {
            if is_own_param(param, func) {
                let argument = Value::of(param.clone()).substitute(func, bound);
                self.mutate(cx, &argument, Mutation::Argument);
            } else {
                self.event(cx, Event::MutatesParam(param.clone()));
            }
        }
        if same_program {
            for (symbol, binding) in &summary.writes {
                let binding = Binding {
                    presence: binding.presence,
                    value: binding.value.substitute(func, bound),
                };
                cx.set_binding(*symbol, binding);
            }
        } else if !summary.writes.is_empty() && func.module == self.importer {
            let what = format!(
                "rebinds globals of `{}` through another module",
                func.module
            );
            self.unknown_call(cx, &what);
        }
        summary.returned.substitute(func, bound)
    }
}

const UNARY_METHODS: &[&str] = &["__neg__", "__pos__", "__invert__"];

const COMPARISON_METHODS: &[&str] = &[
    "__eq__",
    "__ne__",
    "__lt__",
    "__le__",
    "__gt__",
    "__ge__",
    "__contains__",
];

const ARITHMETIC_METHODS: &[&str] = &[
    "__add__",
    "__radd__",
    "__iadd__",
    "__sub__",
    "__rsub__",
    "__isub__",
    "__mul__",
    "__rmul__",
    "__imul__",
    "__matmul__",
    "__rmatmul__",
    "__imatmul__",
    "__truediv__",
    "__rtruediv__",
    "__itruediv__",
    "__floordiv__",
    "__rfloordiv__",
    "__ifloordiv__",
    "__mod__",
    "__rmod__",
    "__imod__",
    "__pow__",
    "__rpow__",
    "__ipow__",
    "__lshift__",
    "__rlshift__",
    "__ilshift__",
    "__rshift__",
    "__rrshift__",
    "__irshift__",
    "__and__",
    "__rand__",
    "__iand__",
    "__xor__",
    "__rxor__",
    "__ixor__",
    "__or__",
    "__ror__",
    "__ior__",
];

/// Special methods that builtins may call on their arguments.
const CONVERSION_METHODS: &[&str] = &[
    "__len__",
    "__iter__",
    "__next__",
    "__reversed__",
    "__contains__",
    "__str__",
    "__repr__",
    "__format__",
    "__hash__",
    "__bool__",
    "__index__",
    "__int__",
    "__float__",
    "__abs__",
    "__round__",
];

/// The binding of a builtin symbol, which always holds the builtin itself: the program
/// has no way to rebind it except through the `builtins` module object.
pub fn builtin_binding(program: &Program, symbol: SymbolId) -> Option<Binding<Value>> {
    let s = program.symbol(symbol);
    (s.origin == SymbolOrigin::Builtin)
        .then(|| Binding::bound(Value::of(Origin::Builtin(s.name.clone()))))
}

/// A `property` with only a getter.
fn property(getter: FuncRef) -> Origin {
    Origin::Property(Accessors {
        getter: Some(getter),
        setter: None,
        deleter: None,
    })
}

/// Wrap each function in `value` with `wrap`; anything else is unknown.
fn wrap_functions(value: Option<&Value>, wrap: impl Fn(FuncRef) -> Origin) -> Value {
    match value {
        Some(v) => v.flat_map(|o| match o {
            Origin::Function(f) => Value::of(wrap(*f)),
            _ => Value::unknown(),
        }),
        None => Value::unknown(),
    }
}

/// True if `param`, a parameter origin from a summary, is a parameter of `func` (or an
/// object reached from one) rather than of an enclosing activation.
fn is_own_param(param: &Origin, func: FuncRef) -> bool {
    match param {
        Origin::Param(f, _) => *f == func,
        Origin::Member { root, .. } => matches!(&**root, Origin::Param(f, _) if *f == func),
        _ => unreachable!("summaries record only parameter origins"),
    }
}

fn is_sys_modules(origin: &Origin) -> bool {
    matches!(origin, Origin::Imported(p) if p.as_str() == "sys.modules")
}

/// Bind call arguments to parameters following Python's rules. A value behind `*` or
/// `**` may reach any parameter. A parameter left unbound takes its default value,
/// which is unknown here because function objects do not carry their defaults.
fn bind(params: &[Param], args: &CallArgs) -> Vec<Value> {
    let mut bound: Vec<Option<Value>> = vec![None; params.len()];
    let mut put = |i: usize, v: &Value| match &mut bound[i] {
        Some(old) => old.extend(v.clone()),
        slot @ None => *slot = Some(v.clone()),
    };
    let mut positional_slots = params
        .iter()
        .enumerate()
        .filter(|(_, p)| matches!(p.kind, ParamKind::PositionalOnly | ParamKind::Positional))
        .map(|(i, _)| i);
    let varargs = params.iter().position(|p| p.kind == ParamKind::VarArgs);
    let kwargs = params.iter().position(|p| p.kind == ParamKind::KwArgs);
    for value in &args.positional {
        if let Some(i) = positional_slots.next().or(varargs) {
            put(i, value);
        }
    }
    for (name, value) in &args.keywords {
        let target = params.iter().position(|p| {
            p.name == *name && matches!(p.kind, ParamKind::Positional | ParamKind::KeywordOnly)
        });
        if let Some(i) = target.or(kwargs) {
            put(i, value);
        }
    }
    bound
        .into_iter()
        .zip(params)
        .map(|(b, param)| {
            let mut value = match b {
                Some(v) => v,
                None if param.has_default => Value::unknown(),
                None => Value::default(),
            };
            if let Some(s) = &args.spread {
                value.extend(s.clone());
            }
            value
        })
        .collect()
}
