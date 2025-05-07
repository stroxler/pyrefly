/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::mem;
use std::sync::Arc;

use itertools::Itertools;
use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use starlark_map::small_map::SmallMap;
use vec1::Vec1;

use crate::assert_bytes;
use crate::binding::narrow::PropertyKind;
use crate::types::types::Type;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

assert_bytes!(TypeInfo, 40);

/// The `TypeInfo` datatype represents type information associated with a
/// name or expression in a control flow context.
///
/// This is distinct from `Type` because expressions and bound names can
/// track, in addition to the type of the top-level value, zero or more
/// property narrows where we have access to additional control-flow-dependent
/// knowledge about how a chain of property accesses will resolve.
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
    properties: Option<Box<NarrowedProperties>>,
}

impl TypeInfo {
    pub fn of_ty(ty: Type) -> Self {
        Self {
            ty,
            properties: None,
        }
    }

    pub fn with_ty(self, ty: Type) -> Self {
        Self {
            ty,
            properties: self.properties,
        }
    }

    pub fn type_at_property(&self, property: &PropertyKind) -> Option<&Type> {
        match self.get_at_property(property) {
            None | Some(NarrowedProperty::WithoutRoot(..)) => None,
            Some(NarrowedProperty::Leaf(ty)) | Some(NarrowedProperty::WithRoot(ty, _)) => Some(ty),
        }
    }

    pub fn at_property(&self, property: &PropertyKind, fallback: impl Fn() -> Type) -> Self {
        match self.get_at_property(property) {
            None => TypeInfo::of_ty(fallback()),
            Some(NarrowedProperty::Leaf(ty)) => Self::of_ty(ty.clone()),
            Some(NarrowedProperty::WithoutRoot(props)) => Self {
                ty: fallback(),
                properties: Some(Box::new(props.clone())),
            },
            Some(NarrowedProperty::WithRoot(ty, props)) => Self {
                ty: ty.clone(),
                properties: Some(Box::new(props.clone())),
            },
        }
    }

    pub fn with_narrow(&self, properties: &Vec1<PropertyKind>, ty: Type) -> Self {
        if ty == Type::any_error() {
            return self.clone();
        }
        let mut type_info = self.clone();
        type_info.add_narrow(properties, ty);
        type_info
    }

    /// Join zero or more `TypeInfo`s together:
    /// - We'll take the union of the top-level types
    /// - At property chains where all branches narrow, take a union of the narrowed types.
    /// - Drop narrowing for property chains where at least one branch does not narrow
    ///
    /// In the case where there are no branches, we get `Never` with no narrows.
    pub fn join(mut branches: Vec<Self>, union_types: &impl Fn(Vec<Type>) -> Type) -> Self {
        match branches.len() {
            0 => Self::of_ty(Type::never()),
            1 => branches.pop().unwrap(),
            _ => {
                let (tys, props): (Vec<Type>, Vec<Option<&NarrowedProperties>>) = branches
                    .iter()
                    .map(
                        |TypeInfo {
                             ty,
                             properties: props,
                         }| {
                            (ty.clone(), props.as_ref().map(|a| a.as_ref()))
                        },
                    )
                    .unzip();
                let ty = union_types(tys);
                let props =
                    NarrowedProperties::join(props.into_iter().flatten().collect(), union_types);
                Self {
                    ty,
                    properties: props.map(Box::new),
                }
            }
        }
    }

    /// Add a narrow to the TypeInfo. This is used for narrowing conditions, not assignment - it
    /// only adds a new narrow (possibly overwriting any preexisting narrow), without changing subtrees.
    fn add_narrow(&mut self, names: &Vec1<PropertyKind>, ty: Type) {
        if let Some((name, more_properties)) = names.split_first() {
            if let Some(props) = &mut self.properties {
                props.add_narrow(name.clone(), more_properties, ty);
            } else {
                self.properties = Some(Box::new(NarrowedProperties::of_narrow(
                    name.clone(),
                    more_properties,
                    ty,
                )));
            }
        } else {
            unreachable!(
                "We know the Vec1 will split. But the safe API, split_off_first, is not ref-based."
            )
        }
    }

    /// Update for an assignment. This is different from `add_narrow` for two reasons:
    /// - It invalidates any existing subtree at that property chain in addition to narrowing.
    /// - There may not be a type available for the assignment (in which case we *just* invalidate)
    pub fn update_for_assignment(&mut self, properties: &Vec1<PropertyKind>, ty: Option<Type>) {
        if let Some((prop, more_properties)) = properties.split_first() {
            if let Some(properties) = &mut self.properties {
                // If there might be an existing narrow, we need to recurse down the chain of names and update.
                properties.update_for_assignment(prop, more_properties, ty);
            } else if let Some(ty) = ty {
                // If there is no existing narrow and a Type is available, we should create a narrow.
                self.properties = Some(Box::new(NarrowedProperties::of_narrow(
                    prop.clone(),
                    more_properties,
                    ty,
                )));
            } // ... else we have no type nor an existing narrow, nothing to do
        } else {
            unreachable!(
                "We know the Vec1 will split. But the safe API, split_off_first, is not ref-based."
            )
        }
    }

    /// When we assign to a property chain containing an unknown index, we don't know which index changed
    /// and have to invalidate all of them.
    pub fn invalidate_all_indexes_for_assignment(&mut self, properties: &[PropertyKind]) {
        if let Some(props) = &mut self.properties {
            props.clear_index_narrows(properties);
        }
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

    fn get_at_property(&self, property: &PropertyKind) -> Option<&NarrowedProperty> {
        if let Some(box props) = &self.properties {
            props.get(property)
        } else {
            None
        }
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ty().fmt(f)?;
        if let Some(props) = &self.properties {
            write!(f, " ({})", props)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TypeEq)]
struct NarrowedProperties(SmallMap<PropertyKind, NarrowedProperty>);

impl NarrowedProperties {
    fn add_narrow(&mut self, property: PropertyKind, more_properties: &[PropertyKind], ty: Type) {
        let props = &mut self.0;
        match props.get_mut(&property) {
            Some(prop) => {
                prop.add_narrow(more_properties, ty);
            }
            None => {
                props.insert(property, NarrowedProperty::new(more_properties, ty));
            }
        };
    }

    fn clear_index_narrows(&mut self, properties: &[PropertyKind]) {
        let props = &mut self.0;
        match properties {
            [] => {
                props.retain(|k, _| !k.invalidate_on_unknown_assignment());
            }
            [property, more_properties @ ..] => match props.get_mut(property) {
                Some(prop) => {
                    prop.clear_index_narrows(more_properties);
                }
                _ => {}
            },
        }
    }

    fn update_for_assignment(
        &mut self,
        property: &PropertyKind,
        more_properties: &[PropertyKind],
        ty: Option<Type>,
    ) {
        let props = &mut self.0;
        match more_properties {
            [] => {
                if let Some(ty) = ty {
                    props.insert(property.clone(), NarrowedProperty::new(more_properties, ty));
                } else {
                    props.shift_remove(property);
                }
            }
            [next_property, remaining_properties @ ..] => {
                if let Some(prop) = props.get_mut(next_property) {
                    prop.update_for_assignment(next_property, remaining_properties, ty);
                } else if let Some(ty) = ty {
                    props.insert(property.clone(), NarrowedProperty::new(more_properties, ty));
                }
                // ... else there is no existing narrow and no narrow type, so do nothing.
            }
        }
    }

    fn get(&self, property: &PropertyKind) -> Option<&NarrowedProperty> {
        self.0.get(property)
    }

    fn of_narrow(name: PropertyKind, more_properties: &[PropertyKind], ty: Type) -> Self {
        let mut props = SmallMap::with_capacity(1);
        props.insert(name.clone(), NarrowedProperty::new(more_properties, ty));
        Self(props)
    }

    fn join(mut branches: Vec<&Self>, union_types: &impl Fn(Vec<Type>) -> Type) -> Option<Self> {
        match branches.len() {
            0 => None,
            1 => Some(branches.pop().unwrap().clone()),
            n => {
                let first = branches[0].clone();
                let tail = &branches[1..];
                let props: SmallMap<_, _> = first
                    .0
                    .into_iter()
                    .filter_map(|(name, prop)| {
                        let mut prop_branches = Vec::with_capacity(n);
                        prop_branches.push(prop.clone());
                        prop_branches
                            .extend(tail.iter().filter_map(|props| props.get(&name).cloned()));
                        // If any map lacked this name, we just drop it. Only join if all maps have it.
                        if prop_branches.len() == n {
                            NarrowedProperty::join(prop_branches, union_types)
                                .map(move |prop| (name, prop))
                        } else {
                            None
                        }
                    })
                    .collect();
                if props.is_empty() {
                    None
                } else {
                    Some(Self(props))
                }
            }
        }
    }

    fn fmt_with_prefix(
        &self,
        prefix: &mut Vec<PropertyKind>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let props = &self.0;
        let mut first = true;
        for (name, value) in props.iter() {
            if first {
                first = false
            } else {
                write!(f, ", ")?;
            }
            match value {
                NarrowedProperty::Leaf(ty) => Self::fmt_type_with_label(prefix, name, ty, f),
                NarrowedProperty::WithRoot(ty, props) => {
                    Self::fmt_type_with_label(prefix, name, ty, f)?;
                    write!(f, ", ")?;
                    props.fmt_with_prefix_and_name(prefix, name, f)
                }
                NarrowedProperty::WithoutRoot(props) => {
                    props.fmt_with_prefix_and_name(prefix, name, f)
                }
            }?;
        }
        Ok(())
    }

    fn fmt_with_prefix_and_name<'a>(
        &self,
        prefix: &mut Vec<PropertyKind>,
        property: &'a PropertyKind,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        prefix.push(property.clone());
        self.fmt_with_prefix(prefix, f)?;
        prefix.pop();
        Ok(())
    }

    fn fmt_type_with_label(
        prefix: &[PropertyKind],
        property: &PropertyKind,
        ty: &Type,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "_{}{}: {}", prefix.iter().join(""), property, ty)
    }
}

impl Visit<Type> for NarrowedProperties {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        let props = &self.0;
        props.values().for_each(|value| {
            value.visit(f);
        })
    }
}

impl VisitMut<Type> for NarrowedProperties {
    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        let props = &mut self.0;
        props.values_mut().for_each(|value| {
            value.visit_mut(f);
        })
    }
}

impl Display for NarrowedProperties {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_prefix(&mut Vec::new(), f)
    }
}

/// A `NarrowedProperty` represents a single property within a tree of narrowed
/// properties. The property itself may or may not be narrowed, and it may or
/// may not have any sub-properties (but at least one must be the case, or it
/// wouldn't be in the tree at all)
#[derive(Debug, Clone, Visit, VisitMut, PartialEq, Eq, TypeEq)]
enum NarrowedProperty {
    /// This property is narrowed, and has no narrowed sub-property (Leaf)
    Leaf(Type),
    /// This property is narrowed, and has one or more narrowed sub-property (WithRoot)
    WithRoot(Type, NarrowedProperties),
    /// This property is not narrowed, and has one or more narrowed sub-property (WithoutRoot)
    WithoutRoot(NarrowedProperties),
}

impl NarrowedProperty {
    fn new(properties: &[PropertyKind], ty: Type) -> Self {
        match properties {
            [] => Self::Leaf(ty),
            [first_property, more_properties @ ..] => Self::WithoutRoot(
                NarrowedProperties::of_narrow((*first_property).clone(), more_properties, ty),
            ),
        }
    }

    fn add_narrow(&mut self, names: &[PropertyKind], narrowed_ty: Type) {
        // Take ownership of self so we can destructure it and potentially change enum variants.
        let mut current = NarrowedProperty::Leaf(Type::None);
        mem::swap(self, &mut current);
        *self = current.with_narrow(names, narrowed_ty)
    }

    fn clear_index_narrows(&mut self, properties: &[PropertyKind]) {
        match self {
            Self::Leaf(_) => {}
            Self::WithRoot(_, props) | Self::WithoutRoot(props) => {
                props.clear_index_narrows(properties)
            }
        }
    }

    fn with_narrow(self, names: &[PropertyKind], narrowed_ty: Type) -> Self {
        match names {
            [] => {
                // We are setting a narrow at the current node (potentially overriding an existing narrow; it is
                // up to callers to make sure this works correctly, we just take what was given).
                match self {
                    Self::Leaf(_) => Self::Leaf(narrowed_ty),
                    Self::WithRoot(_, props) | Self::WithoutRoot(props) => {
                        Self::WithRoot(narrowed_ty, props)
                    }
                }
            }
            [name, more_properties @ ..] => {
                // We are setting a narrow in a subtree. We need to preserve any existing tree.
                match self {
                    Self::Leaf(root_ty) => {
                        let props = NarrowedProperties::of_narrow(
                            (*name).clone(),
                            more_properties,
                            narrowed_ty,
                        );
                        Self::WithRoot(root_ty, props)
                    }
                    Self::WithoutRoot(mut props) => {
                        props.add_narrow((*name).clone(), more_properties, narrowed_ty);
                        Self::WithoutRoot(props)
                    }
                    Self::WithRoot(root_ty, mut props) => {
                        props.add_narrow((*name).clone(), more_properties, narrowed_ty);
                        Self::WithRoot(root_ty, props)
                    }
                }
            }
        }
    }

    fn update_for_assignment(
        &mut self,
        property: &PropertyKind,
        more_properties: &[PropertyKind],
        ty: Option<Type>,
    ) {
        match self {
            Self::Leaf(..) => {}
            Self::WithoutRoot(props) | Self::WithRoot(_, props) => {
                props.update_for_assignment(property, more_properties, ty);
            }
        }
    }

    fn join(branches: Vec<Self>, union_types: &impl Fn(Vec<Type>) -> Type) -> Option<Self> {
        fn monadic_push_option<T>(acc: &mut Option<Vec<T>>, item: Option<T>) {
            match item {
                None => *acc = None,
                Some(item) => {
                    if let Some(acc) = acc {
                        acc.push(item)
                    }
                }
            };
        }
        let mut ty_branches = Some(Vec::with_capacity(branches.len()));
        let mut props_branches = Some(Vec::with_capacity(branches.len()));
        for prop in branches {
            let (ty, props) = match prop {
                Self::WithRoot(ty, props) => (Some(ty), Some(props)),
                Self::Leaf(ty) => (Some(ty), None),
                Self::WithoutRoot(props) => (None, Some(props)),
            };
            monadic_push_option(&mut ty_branches, ty);
            monadic_push_option(&mut props_branches, props);
            if let (None, None) = (&ty_branches, &props_branches) {
                // Not needed for correctness, but saves some work.
                return None;
            }
        }
        let ty = ty_branches.map(union_types);
        let props = props_branches.and_then(|props_branches| {
            NarrowedProperties::join(props_branches.iter().collect(), union_types)
        });
        match (ty, props) {
            (None, None) => None,
            (Some(ty), None) => Some(Self::Leaf(ty)),
            (Some(ty), Some(props)) => Some(Self::WithRoot(ty, props)),
            (None, Some(props)) => Some(Self::WithoutRoot(props)),
        }
    }
}

#[cfg(test)]
mod tests {

    use ruff_python_ast::name::Name;
    use vec1::Vec1;

    use crate::binding::narrow::PropertyKind;
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
        let x = || PropertyKind::Attribute(Name::new_static("x"));
        let y = || PropertyKind::Attribute(Name::new_static("y"));
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        assert_eq!(type_info.to_string(), "Foo");
        type_info.add_narrow(&Vec1::new(x()), fake_class_type("Bar"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar)");
        type_info.add_narrow(&Vec1::new(y()), fake_class_type("Baz"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar, _.y: Baz)");
    }

    #[test]
    fn test_type_info_adding_sub_properties() {
        let x = || PropertyKind::Attribute(Name::new_static("x"));
        let y = || PropertyKind::Attribute(Name::new_static("y"));
        let z = || PropertyKind::Attribute(Name::new_static("z"));
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(&Vec1::new(x()), fake_class_type("Bar"));
        type_info.add_narrow(&Vec1::from_vec_push(vec![x()], y()), fake_class_type("Baz"));
        assert_eq!(type_info.to_string(), "Foo (_.x: Bar, _.x.y: Baz)");
        type_info.add_narrow(&Vec1::from_vec_push(vec![x()], z()), fake_class_type("Qux"));
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x: Bar, _.x.y: Baz, _.x.z: Qux)"
        );
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y()], x()),
            fake_class_type("Foo"),
        );
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x: Bar, _.x.y: Baz, _.x.y.x: Foo, _.x.z: Qux)"
        );
    }

    #[test]
    fn test_type_info_creating_subtrees_and_narrowing_roots() {
        let x = || PropertyKind::Attribute(Name::new_static("x"));
        let y = || PropertyKind::Attribute(Name::new_static("y"));
        let z = || PropertyKind::Attribute(Name::new_static("z"));
        let w = || PropertyKind::Attribute(Name::new_static("w"));
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y()], z()),
            fake_class_type("Bar"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y.z: Bar)");
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y()], w()),
            fake_class_type("Baz"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y.z: Bar, _.x.y.w: Baz)");
        type_info.add_narrow(&Vec1::from_vec_push(vec![x()], y()), fake_class_type("Qux"));
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x.y: Qux, _.x.y.z: Bar, _.x.y.w: Baz)"
        );
    }

    #[test]
    fn test_type_info_overwiting_existing_narrows() {
        let x = || PropertyKind::Attribute(Name::new_static("x"));
        let y = || PropertyKind::Attribute(Name::new_static("y"));
        let z = || PropertyKind::Attribute(Name::new_static("z"));
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y()], z()),
            fake_class_type("Bar"),
        );
        type_info.add_narrow(&Vec1::from_vec_push(vec![x()], y()), fake_class_type("Qux"));
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux, _.x.y.z: Bar)");
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x()], y()),
            fake_class_type("Qux1"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux1, _.x.y.z: Bar)");
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y()], z()),
            fake_class_type("Bar1"),
        );
        assert_eq!(type_info.to_string(), "Foo (_.x.y: Qux1, _.x.y.z: Bar1)");
    }

    #[test]
    fn test_type_info_empty_join() {
        let type_info = TypeInfo::join(Vec::new(), &|ts| {
            if ts.is_empty() {
                fake_class_type("Never")
            } else {
                fake_class_type("FakeUnionType")
            }
        });
        assert_eq!(type_info.to_string(), "Never");
    }

    #[test]
    fn test_type_info_invalidating_prefix() {
        let x = || PropertyKind::Attribute(Name::new_static("x"));
        let y = || PropertyKind::Attribute(Name::new_static("y"));
        let idx0 = || PropertyKind::Index(0);
        let idx1 = || PropertyKind::Index(1);
        let z = || PropertyKind::Attribute(Name::new_static("z"));
        let mut type_info = TypeInfo::of_ty(fake_class_type("Foo"));
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y(), idx0()], z()),
            fake_class_type("Bar"),
        );
        type_info.add_narrow(
            &Vec1::from_vec_push(vec![x(), y(), idx1()], z()),
            fake_class_type("Bar"),
        );
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x.y[0].z: Bar, _.x.y[1].z: Bar)"
        );
        // x has no narrowed indexes, so do nothing
        type_info.invalidate_all_indexes_for_assignment(&[x()]);
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x.y[0].z: Bar, _.x.y[1].z: Bar)"
        );
        // this path doesn't have any narrowing, so do nothing
        type_info.invalidate_all_indexes_for_assignment(&[x(), z()]);
        assert_eq!(
            type_info.to_string(),
            "Foo (_.x.y[0].z: Bar, _.x.y[1].z: Bar)"
        );
        // this clears the narrowing for both x.y[0] and x.y[1]
        type_info.invalidate_all_indexes_for_assignment(&[x(), y()]);
        assert_eq!(type_info.to_string(), "Foo ()");
    }
}
