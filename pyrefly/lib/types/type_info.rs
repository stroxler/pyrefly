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
use starlark_map::small_map::SmallMap;
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
///
/// ```python
/// x: Foo
/// if x.foo is not None x.foo.bar is None and x.baz is None:
///     # here, `x` is still `Foo` but we also can narrow
///     # `x.foo`, `x.foo.bar`, and `x.baz`.
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Visit, VisitMut, TypeEq)]
pub struct TypeInfo {
    ty: Type,
    attrs: NarrowedAttrs,
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
    pub fn add_narrow(&mut self, names: Vec1<&Name>, ty: Type) {
        let (name, more_names) = names.split_off_first();
        self.attrs.add_narrow(name.clone(), &more_names, ty)
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

#[derive(Debug, Clone, PartialEq, Eq, TypeEq)]
struct NarrowedAttrs(Option<Box<SmallMap<Name, NarrowedAttr>>>);

impl NarrowedAttrs {
    fn new() -> Self {
        Self(None)
    }

    fn add_narrow(&mut self, name: Name, more_names: &[&Name], ty: Type) {
        if self.0.is_none() {
            self.0 = Some(Box::new(SmallMap::with_capacity(1)))
        }
        match &mut self.0 {
            None => unreachable!("We just ensured that we have a map of attrs"),
            Some(box attrs) => {
                let attr = match attrs.shift_remove(&name) {
                    Some(attr) => attr.with_narrow(more_names, ty),
                    None => NarrowedAttr::new(more_names, ty),
                };
                attrs.insert(name, attr);
            }
        }
    }

    fn of_narrow(name: Name, more_names: &[&Name], ty: Type) -> Self {
        let mut attrs = SmallMap::with_capacity(1);
        attrs.insert(name.clone(), NarrowedAttr::new(more_names, ty));
        Self(Some(Box::new(attrs)))
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

/// A `NarrowedAttr` represents a single attribute within a tree of narrowed
/// attributes. The attribute itself may or may not be narrowed, and it may or
/// may not have any sub-attributes (but at least one must be the case, or it
/// wouldn't be in the tree at all)
#[derive(Debug, Clone, Visit, VisitMut, PartialEq, Eq, TypeEq)]
enum NarrowedAttr {
    /// This attribute is narrowed, and has no narrowed sub-attributes (Leaf)
    Leaf(Type),
    /// This attribute is narrowed, and has one or more narrowed sub-attributes (WithRoot)
    WithRoot(Type, NarrowedAttrs),
    /// This attribute is not narrowed, and has one or more narrowed sub-attributes (WithoutRoot)
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

    fn with_narrow(self, names: &[&Name], narrowed_ty: Type) -> Self {
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
                        attrs.add_narrow((*name).clone(), more_names, narrowed_ty);
                        Self::WithoutRoot(attrs)
                    }
                    Self::WithRoot(root_ty, mut attrs) => {
                        attrs.add_narrow((*name).clone(), more_names, narrowed_ty);
                        Self::WithRoot(root_ty, attrs)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use ruff_python_ast::name::Name;
    use vec1::Vec1;

    use crate::types::class::ClassType;
    use crate::types::class::TArgs;
    use crate::types::display::tests::fake_class;
    use crate::types::type_info::TypeInfo;
    use crate::types::types::Type;

    fn fake_class_type(class_name: &str) -> Type {
        Type::ClassType(ClassType::new(
            fake_class(class_name, "class_defs_module", 5, Vec::new()),
            TArgs::default(),
        ))
    }

    #[test]
    fn test_type_info_one_level_only() {
        let x = Name::new_static("x");
        let y = Name::new_static("y");
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        assert_eq!(type_info.to_string(), "Foo");
        type_info.add_narrow(Vec1::new(&x), fake_class_type("Bar"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar)");
        type_info.add_narrow(Vec1::new(&y), fake_class_type("Baz"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar, _.y: Baz)");
    }

    #[test]
    fn test_type_info_adding_sub_attributes() {
        let x = Name::new_static("x");
        let y = Name::new_static("y");
        let z = Name::new_static("z");
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(Vec1::new(&x), fake_class_type("Bar"));
        type_info.add_narrow(Vec1::from_vec_push(vec![&x], &y), fake_class_type("Baz"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar, _.x.y: Baz)");
        type_info.add_narrow(Vec1::from_vec_push(vec![&x], &z), fake_class_type("Qux"));
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x: Bar, _.x.y: Baz, _.x.z: Qux)"
        );
        type_info.add_narrow(
            Vec1::from_vec_push(vec![&x, &y], &x),
            fake_class_type("Foo"),
        );
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x: Bar, _.x.z: Qux, _.x.y: Baz, _.x.y.x: Foo)"
        );
    }

    #[test]
    fn test_type_info_creating_subtrees_and_narrowing_roots() {
        let x = Name::new_static("x");
        let y = Name::new_static("y");
        let z = Name::new_static("z");
        let w = Name::new_static("w");
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(
            Vec1::from_vec_push(vec![&x, &y], &z),
            fake_class_type("Bar"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y.z: Bar)");
        type_info.add_narrow(
            Vec1::from_vec_push(vec![&x, &y], &w),
            fake_class_type("Baz"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y.z: Bar, _.x.y.w: Baz)");
        type_info.add_narrow(Vec1::from_vec_push(vec![&x], &y), fake_class_type("Qux"));
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x.y: Qux, _.x.y.z: Bar, _.x.y.w: Baz)"
        );
    }

    #[test]
    fn test_type_info_overwiting_existing_narrows() {
        let x = Name::new_static("x");
        let y = Name::new_static("y");
        let z = Name::new_static("z");
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(
            Vec1::from_vec_push(vec![&x, &y], &z),
            fake_class_type("Bar"),
        );
        type_info.add_narrow(Vec1::from_vec_push(vec![&x], &y), fake_class_type("Qux"));
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux, _.x.y.z: Bar)");
        type_info.add_narrow(Vec1::from_vec_push(vec![&x], &y), fake_class_type("Qux1"));
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux1, _.x.y.z: Bar)");
        type_info.add_narrow(
            Vec1::from_vec_push(vec![&x, &y], &z),
            fake_class_type("Bar1"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux1, _.x.y.z: Bar1)");
    }
}
