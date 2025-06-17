/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Used to take ownership of values that are then returned as references.

use append_only_vec::AppendOnlyVec;

pub struct Owner<T>(AppendOnlyVec<T>);

impl<T> Default for Owner<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Owner<T> {
    pub fn new() -> Self {
        Self(AppendOnlyVec::new())
    }

    pub fn push(&self, value: T) -> &T {
        let i = self.0.push(value);
        &self.0[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owner() {
        let o = Owner::new();
        let a = o.push(1);
        let b = o.push(2);
        assert_eq!(*a, 1);
        assert_eq!(*b, 2);
    }
}
