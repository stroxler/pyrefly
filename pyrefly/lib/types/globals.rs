/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Global variables defined at the top level of a module.
//!
//! We do not include `__doc__` as that has a type that changes based on other variables.

use ruff_python_ast::name::Name;

use super::stdlib::Stdlib;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct Global {
    name: Name,
    ty: fn(&Stdlib) -> Type,
}

const GLOBALS: &[Global] = &[
    Global::new("__debug__", |stdlib| stdlib.bool().clone().to_type()),
    Global::new("__file__", |stdlib| stdlib.str().clone().to_type()),
    Global::new("__name__", |stdlib| stdlib.str().clone().to_type()),
];

impl Global {
    const fn new(name: &'static str, ty: fn(&Stdlib) -> Type) -> Self {
        Self {
            name: Name::new_static(name),
            ty,
        }
    }

    pub fn from_name(name: &Name) -> Option<Global> {
        if name.starts_with("__") && name.ends_with("__") {
            GLOBALS.iter().find(|x| &x.name == name).cloned()
        } else {
            None
        }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn as_type(&self, stdlib: &Stdlib) -> Type {
        (self.ty)(stdlib)
    }

    pub fn doc(has_docstring: bool) -> Self {
        if has_docstring {
            Self::new("__doc__", |stdlib| stdlib.str().clone().to_type())
        } else {
            Self::new("__doc__", |_| Type::None)
        }
    }
}
