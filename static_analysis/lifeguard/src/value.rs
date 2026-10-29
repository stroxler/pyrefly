/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The abstract values the lazy-import domain tracks for symbols and temporaries.
//!
//! A [`Value`] is the set of [`Origin`]s an object may come from. The origin decides
//! what mutating the object means at import time: an object created while importing
//! the module under analysis is local state, while an object reached through an import
//! belongs to another module.

use std::collections::BTreeSet;

use pyrefly_static_analysis::ModuleName;
use pyrefly_static_analysis::Name;
use pyrefly_static_analysis::OpId;
use pyrefly_static_analysis::RegionId;

/// A function body: a deferred region of one project module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncRef {
    pub module: ModuleName,
    pub region: RegionId,
}

/// A class created by a `class` statement in a project module, with the values its
/// body bound when the class was created.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassInfo {
    pub module: ModuleName,
    pub def: OpId,
    pub name: Name,
    pub members: Vec<(Name, Value)>,
    pub bases: Vec<Value>,
}

impl ClassInfo {
    /// Look up a class attribute through the class and its project bases. `None`
    /// means the attribute is not defined by any class body the analysis can see.
    pub fn lookup(&self, attr: &Name) -> Option<Value> {
        if let Some((_, v)) = self.members.iter().find(|(n, _)| n == attr) {
            return Some(v.clone());
        }
        self.bases.iter().find_map(|base| {
            base.iter().find_map(|o| match o {
                Origin::Class(c) => c.lookup(attr),
                _ => None,
            })
        })
    }

    fn substitute(&self, func: FuncRef, args: &[Value]) -> ClassInfo {
        ClassInfo {
            members: self
                .members
                .iter()
                .map(|(n, v)| (n.clone(), v.substitute(func, args)))
                .collect(),
            bases: self
                .bases
                .iter()
                .map(|b| b.substitute(func, args))
                .collect(),
            module: self.module,
            def: self.def,
            name: self.name.clone(),
        }
    }
}

/// A `property` object and the accessor functions it holds.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Accessors {
    pub getter: Option<FuncRef>,
    pub setter: Option<FuncRef>,
    pub deleter: Option<FuncRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Origin {
    /// An immutable constant other than a string.
    Const,
    /// A string constant, kept for literal attribute names, `sys.modules` keys and `eval`.
    Str(Box<str>),
    /// A mutable object created while importing the given project module.
    Object(ModuleName),
    /// An object reached through a module outside the project, named by its dotted path.
    Imported(ModuleName),
    /// A project module object.
    Module(ModuleName),
    /// A builtin function or class.
    Builtin(Name),
    /// A function object for a project function body.
    Function(FuncRef),
    /// A function bound to a receiver: a method read from an instance, or a
    /// `classmethod` read from its class or an instance.
    Bound(FuncRef, Box<Value>),
    /// A function wrapped by `staticmethod`.
    StaticMethod(FuncRef),
    /// A function wrapped by `classmethod`.
    ClassMethod(FuncRef),
    /// The generator or coroutine returned by calling a generator or `async` function
    /// with the given arguments; its body has not run yet.
    Generator(FuncRef, Vec<Value>),
    Property(Accessors),
    /// A decorator known to return the object it decorates.
    Identity,
    Class(Box<ClassInfo>),
    /// An instance of a project class, created while importing `owner`.
    Instance(Box<ClassInfo>, ModuleName),
    /// An attribute (`attr = Some`) or item (`attr = None`) of an object reached from
    /// `root`: of `root` itself, or of something reachable from it when `deep`. Mutating
    /// it mutates the object graph of `root`. `root` is never itself a `Member`.
    Member {
        root: Box<Origin>,
        attr: Option<Name>,
        deep: bool,
    },
    /// A parameter of a function activation, as seen inside that function's summary.
    /// Callers replace it with the argument (see [`Value::substitute`]), so it never
    /// reaches a module body.
    Param(FuncRef, usize),
    /// Anything the analysis cannot attribute.
    Unknown,
}

impl Origin {
    /// The origin of `self.attr` (or `self[...]` when `attr` is `None`) when nothing more
    /// specific is known about the attribute.
    pub fn member(&self, attr: Option<&Name>) -> Origin {
        match (self, attr) {
            (Origin::Imported(path), Some(attr)) => Origin::Imported(path.append(attr)),
            (Origin::Unknown, _) => Origin::Unknown,
            (Origin::Member { root, .. }, _) => Origin::Member {
                root: root.clone(),
                attr: attr.cloned(),
                deep: true,
            },
            _ => Origin::Member {
                root: Box::new(self.clone()),
                attr: attr.cloned(),
                deep: false,
            },
        }
    }

    fn substitute(&self, func: FuncRef, args: &[Value]) -> Value {
        let sub = |v: &Value| v.substitute(func, args);
        Value::of(match self {
            Origin::Param(f, i) if *f == func => return args[*i].clone(),
            Origin::Member { root, attr, deep } => {
                return root.substitute(func, args).flat_map(|r| {
                    let base = if *deep { r.member(None) } else { r.clone() };
                    Value::of(base.member(attr.as_ref()))
                });
            }
            Origin::Bound(f, receiver) => Origin::Bound(*f, Box::new(sub(receiver))),
            Origin::Generator(f, bound) => Origin::Generator(*f, bound.iter().map(sub).collect()),
            Origin::Class(c) => Origin::Class(Box::new(c.substitute(func, args))),
            Origin::Instance(c, owner) => {
                Origin::Instance(Box::new(c.substitute(func, args)), *owner)
            }
            _ => self.clone(),
        })
    }
}

/// A set of origins. The empty set is the value of an unbound name: reading it raises,
/// so it contributes nothing to the normal continuation.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(BTreeSet<Origin>);

impl Value {
    pub fn unknown() -> Self {
        Self::of(Origin::Unknown)
    }

    pub fn of(origin: Origin) -> Self {
        Self(BTreeSet::from([origin]))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Origin> {
        self.0.iter()
    }

    pub fn join(&self, other: &Value) -> Value {
        Value(self.0.union(&other.0).cloned().collect())
    }

    pub fn extend(&mut self, other: Value) {
        self.0.extend(other.0);
    }

    /// The single string constant this value holds, if it is exactly one.
    pub fn as_str(&self) -> Option<&str> {
        match self.0.first() {
            Some(Origin::Str(s)) if self.0.len() == 1 => Some(s),
            _ => None,
        }
    }

    /// Replace the parameters of `func` with the corresponding `args`, including inside
    /// structured values (classes, instances, bound methods and generators).
    pub fn substitute(&self, func: FuncRef, args: &[Value]) -> Value {
        self.flat_map(|o| o.substitute(func, args))
    }

    /// Apply `f` to every origin and join the results.
    pub fn flat_map(&self, mut f: impl FnMut(&Origin) -> Value) -> Value {
        let mut out = Value::default();
        for o in &self.0 {
            out.extend(f(o));
        }
        out
    }
}

impl FromIterator<Origin> for Value {
    fn from_iter<I: IntoIterator<Item = Origin>>(iter: I) -> Self {
        Value(iter.into_iter().collect())
    }
}
