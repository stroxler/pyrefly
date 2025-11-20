/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use pyrefly_python::qname::QName;

/// A trait that will be will be used for formatting types, but also allow
/// additional functionality. The major difference between implementations
/// of this trait will be the location that type information is written to
/// For example, this will be used to write type information to a formatter as we do now,
/// and also allow us to collect the same types along with their location into a vector.
pub trait TypeOutput {
    fn write_str(&mut self, s: &str) -> fmt::Result;
    fn write_qname(&mut self, qname: &QName) -> fmt::Result;
}
