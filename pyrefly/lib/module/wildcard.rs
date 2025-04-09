/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

use regex::Regex;
use serde::de::Visitor;
use serde::Deserialize;
use serde::Serialize;

use crate::module::module_name::ModuleName;

/// A pattern that allows for matching on module paths
/// with a glob-like syntax. Only full matches are supported,
/// along with a wildcard (`*`) that allows for matching one or more
/// path components. If the wildcard does not start the pattern, it can
/// also match zero components.
///
/// The `*` must be its own path component (`<pattern>.*.<pattern>`), or start
/// (`*.<pattern>`)/end (`<pattern>.*`) the pattern.
#[derive(Debug, Clone)]
pub struct ModuleWildcard {
    pattern: Regex,
    /// The original pattern, before rewriting to escape it and handle wildcards
    origin: String,
}

impl ModuleWildcard {
    pub fn new(value: &str) -> anyhow::Result<Self> {
        Ok(Self {
            pattern: Regex::new(value)?,
            origin: value.to_owned(),
        })
    }

    #[expect(unused)]
    pub fn matches(&self, module_path: ModuleName) -> bool {
        self.pattern.is_match(module_path.as_str())
    }
}

impl PartialEq<ModuleWildcard> for ModuleWildcard {
    fn eq(&self, other: &Self) -> bool {
        self.origin == other.origin
    }
}

impl Eq for ModuleWildcard {}

impl Hash for ModuleWildcard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.origin.hash(state);
    }
}

impl<'de> Deserialize<'de> for ModuleWildcard {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct StrVisitor;
        impl<'de> Visitor<'de> for StrVisitor {
            type Value = ModuleWildcard;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("A string regex pattern")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                ModuleWildcard::new(value).map_err(|err| {
                    serde::de::Error::custom(format!("Parsed regex is not valid: {err}"))
                })
            }
        }
        deserializer.deserialize_str(StrVisitor)
    }
}

impl Serialize for ModuleWildcard {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.origin)
    }
}
