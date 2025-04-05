/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use itertools::Itertools;
use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use ruff_python_ast::name::Name;
use starlark_map::ordered_map::OrderedMap;
use vec1::Vec1;

use crate::types::types::Type;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

/// The `TypeInfo` datatype represents type information associated with a
/// name or expression in a control flow context.
///
/// This is distinct from `Type` because expressions and bound names can
/// track, in addition to the type of the top-level value, zero or more
/// attribute narrows where we have access to additional control-flow-dependent
/// knowledge about how a chain of attribute accesses will resolve.
///
/// For example:
/// ```
/// x: Foo
/// if x.foo is not None x.foo.bar is None and x.baz is None:
///     # here, `x` is still `Foo` but we also can narrow
///     # `x.foo`, `x.foo.bar`, and `x.baz`.
/// ```
#[derive(
    Debug, Clone, PartialEq, Eq, Visit, VisitMut, TypeEq, PartialOrd, Ord, Hash
)]
pub struct TypeInfo {
    pub ty: Type,
    pub attrs: NarrowedAttrs,
}

impl TypeInfo {
    pub fn of_ty(ty: Type) -> Self {
        Self {
            ty,
            attrs: NarrowedAttrs::new(),
        }
    }

    pub fn with_ty(self, ty: Type) -> Self {
        Self {
            ty,
            attrs: self.attrs,
        }
    }

    // TODO(stroxler): remove this directive once we have a production API
    // to narrow, at the moment only test code uses this method.
    #[allow(dead_code)]
    pub fn add_narrow_mut(&mut self, names: Vec1<&Name>, ty: Type) {
        let (name, more_names) = names.split_off_first();
        self.attrs.add_narrow_mut(name.clone(), &more_names, ty)
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn into_ty(self) -> Type {
        self.ty
    }

    pub fn arc_clone(self: Arc<Self>) -> Self {
        Arc::unwrap_or_clone(self)
    }

    pub fn arc_clone_ty(self: Arc<Self>) -> Type {
        self.arc_clone().into_ty()
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ty().fmt(f)?;
        if let NarrowedAttrs(Some(_)) = &self.attrs {
            write!(f, " ({})", self.attrs)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TypeEq, PartialOrd, Ord, Hash)]
pub struct NarrowedAttrs(Option<OrderedMap<Name, NarrowedAttr>>);

impl NarrowedAttrs {
    fn new() -> Self {
        Self(None)
    }

    fn add_narrow_mut(&mut self, name: Name, more_names: &[&Name], ty: Type) {
        if self.0.is_none() {
            self.0 = Some(OrderedMap::with_capacity(1))
        }
        match &mut self.0 {
            None => unreachable!("We just ensured that we have a map of attrs"),
            Some(attrs) => {
                let attr = match attrs.remove(&name) {
                    Some(attr) => attr.add_narrow(more_names, ty),
                    None => NarrowedAttr::new(more_names, ty),
                };
                attrs.insert(name, attr);
            }
        }
    }

    fn of_narrow(name: Name, more_names: &[&Name], ty: Type) -> Self {
        let mut attrs = OrderedMap::with_capacity(1);
        attrs.insert(name.clone(), NarrowedAttr::new(more_names, ty));
        Self(Some(attrs))
    }

    fn fmt_with_prefix(&self, prefix: &mut Vec<String>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(attrs) = &self.0 {
            let mut first = true;
            for (name, value) in attrs.iter() {
                if first {
                    first = false
                } else {
                    write!(f, ", ")?;
                }
                match value {
                    NarrowedAttr::Leaf(ty) => Self::fmt_type_with_label(prefix, name, ty, f),
                    NarrowedAttr::WithRoot(ty, attrs) => {
                        Self::fmt_type_with_label(prefix, name, ty, f)?;
                        write!(f, ", ")?;
                        attrs.fmt_with_prefix_and_name(prefix, name, f)
                    }
                    NarrowedAttr::WithoutRoot(attrs) => {
                        attrs.fmt_with_prefix_and_name(prefix, name, f)
                    }
                }?;
            }
        }
        Ok(())
    }

    fn fmt_with_prefix_and_name<'a>(
        &self,
        prefix: &mut Vec<String>,
        name: &'a Name,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        prefix.push(name.to_string());
        self.fmt_with_prefix(prefix, f)?;
        prefix.pop();
        Ok(())
    }

    fn fmt_type_with_label(
        prefix: &[String],
        name: &Name,
        ty: &Type,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(
            f,
            "_.{}{}{}: {}",
            prefix.iter().join("."),
            if prefix.is_empty() { "" } else { "." },
            name,
            ty
        )
    }
}

impl Visit<Type> for NarrowedAttrs {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        if let Some(attrs) = &self.0 {
            attrs.values().for_each(|value| {
                value.visit(f);
            })
        }
    }
}

impl VisitMut<Type> for NarrowedAttrs {
    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        if let Some(attrs) = &mut self.0 {
            attrs.values_mut().for_each(|value| {
                value.visit_mut(f);
            })
        }
    }
}

impl Display for NarrowedAttrs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_prefix(&mut Vec::new(), f)
    }
}

#[derive(
    Debug, Clone, Visit, VisitMut, PartialEq, Eq, TypeEq, PartialOrd, Ord, Hash
)]
pub enum NarrowedAttr {
    Leaf(Type),
    WithRoot(Type, NarrowedAttrs),
    WithoutRoot(NarrowedAttrs),
}

impl NarrowedAttr {
    fn new(names: &[&Name], ty: Type) -> Self {
        match names {
            [] => Self::Leaf(ty),
            [name, more_names @ ..] => {
                Self::WithoutRoot(NarrowedAttrs::of_narrow((*name).clone(), more_names, ty))
            }
        }
    }

    fn add_narrow(self, names: &[&Name], narrowed_ty: Type) -> Self {
        match names {
            [] => {
                // We are setting a narrow at the current node (potentially overriding an existing narrow; it is
                // up to callers to make sure this works correctly, we just take what was given).
                match self {
                    Self::Leaf(_) => Self::Leaf(narrowed_ty),
                    Self::WithRoot(_, attrs) | Self::WithoutRoot(attrs) => {
                        Self::WithRoot(narrowed_ty, attrs)
                    }
                }
            }
            [name, more_names @ ..] => {
                // We are setting a narrow in a subtree. We need to preserve any existing tree.
                match self {
                    Self::Leaf(root_ty) => {
                        let attrs =
                            NarrowedAttrs::of_narrow((*name).clone(), more_names, narrowed_ty);
                        Self::WithRoot(root_ty, attrs)
                    }
                    Self::WithoutRoot(mut attrs) => {
                        attrs.add_narrow_mut((*name).clone(), more_names, narrowed_ty);
                        Self::WithoutRoot(attrs)
                    }
                    Self::WithRoot(root_ty, mut attrs) => {
                        attrs.add_narrow_mut((*name).clone(), more_names, narrowed_ty);
                        Self::WithRoot(root_ty, attrs)
                    }
                }
            }
        }
    }
}
