/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use pyrefly_util::visit::Visit;
use pyrefly_util::visit::VisitMut;
use ruff_python_ast::name::Name;
use starlark_map::ordered_map::OrderedMap;

use crate::types::callable::FuncMetadata;
use crate::types::callable::FunctionKind;
use crate::types::literal::Lit;
use crate::types::types::Type;

#[derive(Debug, Clone, PartialEq, Eq, TypeEq, PartialOrd, Ord, Hash)]
pub struct TypeMap(pub OrderedMap<Name, Type>);

impl TypeMap {
    pub fn new() -> Self {
        Self(OrderedMap::new())
    }

    fn get_bool(&self, name: &Name, default: bool) -> bool {
        self.0
            .get(name)
            .and_then(|t| t.as_bool())
            .unwrap_or(default)
    }
}

impl Visit<Type> for TypeMap {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        for (_, ty) in self.0.iter() {
            ty.visit(f);
        }
    }
}

impl VisitMut<Type> for TypeMap {
    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        for (_, ty) in self.0.iter_mut() {
            ty.visit_mut(f);
        }
    }
}

/// Wraps the result of a function call whose keyword arguments have typing effects.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Visit, VisitMut, TypeEq)]
pub struct KwCall {
    /// The metadata of the called function.
    pub func_metadata: FuncMetadata,
    /// The keyword arguments that the function was called with.
    pub keywords: TypeMap,
    /// The return type of the call.
    pub return_ty: Type,
}

impl KwCall {
    pub fn has_function_kind(&self, kind: FunctionKind) -> bool {
        self.func_metadata.kind == kind
    }
}

/// Parameters to `typing.dataclass_transform`.
/// See https://typing.python.org/en/latest/spec/dataclasses.html#dataclass-transform-parameters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Visit, VisitMut, TypeEq)]
pub struct DataclassTransformKeywords {
    pub eq_default: bool,
    pub order_default: bool,
    pub kw_only_default: bool,
    pub frozen_default: bool,
    // TODO(rechen): add field_specifiers
}

impl DataclassTransformKeywords {
    const EQ_DEFAULT: Name = Name::new_static("eq_default");
    const ORDER_DEFAULT: Name = Name::new_static("order_default");
    const KW_ONLY_DEFAULT: Name = Name::new_static("kw_only_default");
    const FROZEN_DEFAULT: Name = Name::new_static("frozen_default");

    pub fn from_type_map(map: &TypeMap) -> Self {
        Self {
            eq_default: map.get_bool(&Self::EQ_DEFAULT, true),
            order_default: map.get_bool(&Self::ORDER_DEFAULT, false),
            kw_only_default: map.get_bool(&Self::KW_ONLY_DEFAULT, false),
            frozen_default: map.get_bool(&Self::FROZEN_DEFAULT, false),
        }
    }

    pub fn defaults(&self) -> Vec<(Name, bool)> {
        vec![
            (Self::EQ_DEFAULT, self.eq_default),
            (Self::ORDER_DEFAULT, self.order_default),
            (Self::KW_ONLY_DEFAULT, self.kw_only_default),
            (Self::FROZEN_DEFAULT, self.frozen_default),
        ]
    }
}

/// A map from keywords to boolean values. Useful for storing sets of keyword arguments for various
/// dataclass functions.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoolKeywords(OrderedMap<Name, bool>);

impl Visit<Type> for BoolKeywords {
    const RECURSE_CONTAINS: bool = false;
    fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a Type)) {}
}

impl VisitMut<Type> for BoolKeywords {
    const RECURSE_CONTAINS: bool = false;
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {}
}

impl BoolKeywords {
    pub fn new() -> Self {
        Self(OrderedMap::new())
    }

    pub fn from_type_map(map: &TypeMap) -> Self {
        let mut kws = Self::new();
        for (name, ty) in map.0.iter() {
            kws.set_keyword(name, ty);
        }
        kws
    }

    pub fn set_keyword(&mut self, name: &Name, ty: &Type) {
        let value = match ty {
            Type::Literal(Lit::Bool(b)) => *b,
            _ => {
                return;
            }
        };
        self.0.insert(name.clone(), value);
    }

    pub fn get(&self, name_and_default: &(Name, bool)) -> bool {
        let (name, default) = name_and_default;
        *(self.0.get(name).unwrap_or(default))
    }

    pub fn set(&mut self, name: Name, value: bool) {
        self.0.insert(name, value);
    }

    pub fn contains(&self, name: &Name) -> bool {
        self.0.contains_key(name)
    }
}

/// Namespace for keyword names and defaults.
pub struct DataclassKeywords;

impl DataclassKeywords {
    pub const INIT: (Name, bool) = (Name::new_static("init"), true);
    pub const ORDER: (Name, bool) = (Name::new_static("order"), false);
    pub const FROZEN: (Name, bool) = (Name::new_static("frozen"), false);
    pub const MATCH_ARGS: (Name, bool) = (Name::new_static("match_args"), true);
    pub const KW_ONLY: (Name, bool) = (Name::new_static("kw_only"), false);
    /// We combine default and default_factory into a single "default" keyword indicating whether
    /// the field has a default. The default value isn't stored.
    pub const DEFAULT: (Name, bool) = (Name::new_static("default"), false);
    pub const EQ: (Name, bool) = (Name::new_static("eq"), true);
    pub const UNSAFE_HASH: (Name, bool) = (Name::new_static("unsafe_hash"), false);
}
