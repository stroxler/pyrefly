/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(warnings)] // It will be autogerated

use serde::Serialize;
use serde_json::Value;

use crate::report::glean::schema::*;

/// Represents a digest with hash and size
#[derive(Debug, Clone, Serialize)]
pub struct Digest {
    pub hash: String,
    pub size: u64,
}

/// Represents a file digest
#[derive(Debug, Clone, Serialize)]
pub struct FileDigest {
    pub id: u64,
    pub key: FileDigest_key,
}

/// Represents a file digest
#[derive(Debug, Clone, Serialize)]
pub struct FileDigest_key {
    pub file: src::File,
    pub digest: Digest,
}
