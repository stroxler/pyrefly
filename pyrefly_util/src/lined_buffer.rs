/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A buffer that tracks line numbers, and deals with positional information.

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use ruff_source_file::LineColumn;
use ruff_source_file::LineIndex;
use serde::Serialize;

#[derive(Debug, Clone)]
pub struct LinedBuffer {
    pub buffer: Arc<String>,
    pub lines: LineIndex,
}

impl LinedBuffer {
    pub fn new(buffer: Arc<String>) -> Self {
        let lines = LineIndex::from_source_text(&buffer);
        Self { buffer, lines }
    }
}
