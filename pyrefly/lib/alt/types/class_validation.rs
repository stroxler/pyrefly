/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use pyrefly_derive::TypeEq;
use pyrefly_util::visit::VisitMut;

use crate::types::types::Type;

#[derive(Clone, Debug, TypeEq, PartialEq, Eq)]
pub struct AbstractClassMembers;

impl VisitMut<Type> for AbstractClassMembers {
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {
        // Empty unit struct has no fields to visit
    }
}

impl Display for AbstractClassMembers {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "AbstractClassMembers()")
    }
}

impl AbstractClassMembers {
    pub fn new() -> AbstractClassMembers {
        AbstractClassMembers
    }

    pub fn recursive() -> Self {
        AbstractClassMembers
    }
}
