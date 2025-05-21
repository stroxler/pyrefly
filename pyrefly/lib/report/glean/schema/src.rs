/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(warnings)] // It will be autogerated

use serde::Serialize;

/// Represents a byte span in the source code
#[derive(Debug, Clone, Serialize)]
pub struct ByteSpan {
    pub start: u64,
    pub length: u64,
}

/// Represents a file in the source code
#[derive(Debug, Clone, Serialize)]
pub struct File {
    pub id: u64,
    pub key: String,
}

/// Represents the language of a file
#[derive(Debug, Clone, Serialize)]
pub struct FileLanguage {
    pub id: u64,
    pub key: FileLanguage_key,
}

/// Represents the language of a file
#[derive(Debug, Clone, Serialize)]
pub struct FileLanguage_key {
    pub file: File,
    pub language: u8,
}

/// Represents file lines information
#[derive(Debug, Clone, Serialize)]
pub struct FileLines {
    pub id: u64,
    pub key: FileLines_key,
}

/// Represents file lines information
#[derive(Debug, Clone, Serialize)]
pub struct FileLines_key {
    pub file: File,
    pub lengths: Vec<u64>,
    #[serde(rename = "endsInNewline")]
    pub ends_in_newline: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    pub has_unicode_or_tabs: bool,
}
