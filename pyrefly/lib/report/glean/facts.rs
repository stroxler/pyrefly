/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // We don't yet generate this

use serde::Serialize;
use serde_json::Value;

use crate::report::glean::schema::*;

/// The Schema ID for Python as specified by Glean
pub const PYTHON_SCHEMA_ID: &str = "54195d609c9195d2bc09d8fa05050bf7";

/// Represents a Glean JSON file containing Python indexer data
#[derive(Debug, Clone, Serialize)]
pub struct Glean {
    /// The schema entries in the Glean file
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub entries: Vec<GleanEntry>,
}

/// Represents an entry in a Glean file, which can be either a schema ID or a predicate
#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum GleanEntry {
    /// Schema ID entry
    SchemaId { schema_id: String },
    /// Predicate entry containing facts
    Predicate {
        predicate: String,
        facts: Vec<Value>,
    },
}

pub fn json(x: impl Serialize) -> Value {
    serde_json::to_value(x).unwrap()
}

impl python::Name {
    pub fn new(name: String) -> Self {
        Self { id: 0, key: name }
    }
}

impl python::Module {
    pub fn new(name: python::Name) -> Self {
        Self {
            id: 0,
            key: python::Module_key { name },
        }
    }
}

impl src::File {
    pub fn new(file: String) -> Self {
        Self { id: 0, key: file }
    }
}

impl digest::FileDigest {
    pub fn new(file: src::File, digest: digest::Digest) -> Self {
        Self {
            id: 0,
            key: digest::FileDigest_key { file, digest },
        }
    }
}
