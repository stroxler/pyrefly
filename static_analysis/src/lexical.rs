/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Python's lexical name resolution over any tree of scopes.
//!
//! A backend describes its scopes through [`ScopeChain`] and answers, per scope, what that
//! scope's tables say about a name ([`Probe`]). This module owns the navigation rules
//! (<https://docs.python.org/3/reference/executionmodel.html#resolution-of-names>):
//!
//! - lookup proceeds from the innermost scope outward;
//! - a class body is visible only to its own code, and to an annotation scope or `type`
//!   alias scope directly inside it, not to nested functions or comprehensions;
//! - `global` resolves in the root scope;
//! - `nonlocal` resolves in the nearest enclosing scope that is neither a class nor the
//!   root, following an enclosing `nonlocal`; an enclosing `global` is an error.
//!
//! What a scope binds, whether a binding is initialized, and what a name means once found
//! stay with the backend.

/// What kind of scope a backend scope is, as far as navigation is concerned.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    Module,
    Class,
    /// A `def` or `lambda` body.
    Function,
    Comprehension,
    /// The scope of a definition's type parameters (and, for a class, its bases).
    Annotation {
        class_scope: bool,
    },
    /// The scope of a `type` alias value.
    TypeAlias,
}

/// A tree of scopes, navigated from a scope to its parent. Implementations are cheap
/// handles, such as a reference to the backend's scope table.
pub trait ScopeChain: Copy {
    type Id: Copy;
    /// What a scope puts between the scopes it encloses and the scopes enclosing it, in
    /// the backend's terms. `Default` is no barrier.
    type Barrier: Copy + Default;

    fn parent(&self, id: Self::Id) -> Option<Self::Id>;
    fn category(&self, id: Self::Id) -> Category;
    fn barrier(&self, id: Self::Id) -> Self::Barrier;
    /// The barrier of crossing both `a` and `b`.
    fn join(a: Self::Barrier, b: Self::Barrier) -> Self::Barrier;
}

/// A scope visible from the lookup origin.
pub struct View<C: ScopeChain> {
    pub id: C::Id,
    /// How many scopes, visible or not, lie between the origin and this one.
    pub depth: usize,
    /// The joined barriers of the visible scopes from the origin up to, but excluding,
    /// this one. A class body hidden from the origin contributes nothing.
    pub barrier: C::Barrier,
}

/// The scopes visible from `origin`, innermost first.
pub fn visible<C: ScopeChain>(chain: C, origin: C::Id) -> Visible<C> {
    Visible {
        chain,
        next: Some(origin),
        depth: 0,
        barrier: C::Barrier::default(),
        from_annotation_like: matches!(
            chain.category(origin),
            Category::Annotation { .. } | Category::TypeAlias
        ),
    }
}

pub struct Visible<C: ScopeChain> {
    chain: C,
    next: Option<C::Id>,
    depth: usize,
    barrier: C::Barrier,
    from_annotation_like: bool,
}

impl<C: ScopeChain> Iterator for Visible<C> {
    type Item = View<C>;

    fn next(&mut self) -> Option<View<C>> {
        loop {
            let id = self.next?;
            self.next = self.chain.parent(id);
            let depth = self.depth;
            self.depth += 1;
            let category = self.chain.category(id);
            if category == Category::Class
                && !(depth == 0 || (self.from_annotation_like && depth == 1))
            {
                continue;
            }
            let view = View {
                id,
                depth,
                barrier: self.barrier,
            };
            self.barrier = C::join(self.barrier, self.chain.barrier(id));
            return Some(view);
        }
    }
}

/// What one scope's tables say about a name.
pub enum Probe<T> {
    /// The scope binds the name.
    Owned(T),
    Absent,
    /// The scope declares the name `global`.
    Global,
    /// The scope declares the name `nonlocal`.
    Nonlocal,
}

/// Why a `nonlocal` declaration has no binding to refer to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonlocalError {
    /// The name is global: only the root scope binds it, or an enclosing scope declares
    /// it `global`.
    Global,
    NotFound,
}

pub enum Resolution<T> {
    Found(T),
    /// No scope binds the name, so it resolves to a builtin, if any.
    Unbound,
    /// The name is declared `nonlocal` and refers to nothing.
    Nonlocal(NonlocalError),
}

/// Resolve a name as code in `origin` sees it.
pub fn resolve<C: ScopeChain, T>(
    chain: C,
    origin: C::Id,
    mut probe: impl FnMut(C::Id) -> Probe<T>,
) -> Resolution<T> {
    for view in visible(chain, origin) {
        match probe(view.id) {
            Probe::Owned(found) => return Resolution::Found(found),
            Probe::Absent => {}
            // A backend's root probe reports what the root owns before its declarations:
            // `global` at the root is a no-op and `nonlocal` there is a syntax error.
            Probe::Global => {
                return match probe(root(chain, view.id)) {
                    Probe::Owned(found) => Resolution::Found(found),
                    Probe::Absent | Probe::Global | Probe::Nonlocal => Resolution::Unbound,
                };
            }
            Probe::Nonlocal => {
                return match resolve_nonlocal(chain, chain.parent(view.id), probe) {
                    Ok(found) => Resolution::Found(found),
                    Err(e) => Resolution::Nonlocal(e),
                };
            }
        }
    }
    Resolution::Unbound
}

/// Find the binding a `nonlocal` declaration refers to, searching outward from `from`
/// (the declaring scope's parent).
pub fn resolve_nonlocal<C: ScopeChain, T>(
    chain: C,
    from: Option<C::Id>,
    mut probe: impl FnMut(C::Id) -> Probe<T>,
) -> Result<T, NonlocalError> {
    let mut next = from;
    while let Some(id) = next {
        next = chain.parent(id);
        match chain.category(id) {
            Category::Class => {}
            // A `nonlocal` declaration at the root is itself a syntax error, which refers to
            // nothing.
            Category::Module => {
                return Err(match probe(id) {
                    Probe::Absent | Probe::Nonlocal => NonlocalError::NotFound,
                    Probe::Owned(_) | Probe::Global => NonlocalError::Global,
                });
            }
            Category::Function
            | Category::Comprehension
            | Category::Annotation { .. }
            | Category::TypeAlias => match probe(id) {
                Probe::Owned(found) => return Ok(found),
                // An enclosing `nonlocal` refers further out.
                Probe::Absent | Probe::Nonlocal => {}
                Probe::Global => return Err(NonlocalError::Global),
            },
        }
    }
    Err(NonlocalError::NotFound)
}

/// The root scope enclosing `id`, where `global` names live.
pub fn root<C: ScopeChain>(chain: C, mut id: C::Id) -> C::Id {
    while let Some(parent) = chain.parent(id) {
        id = parent;
    }
    id
}
