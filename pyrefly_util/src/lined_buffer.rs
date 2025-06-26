/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A buffer that tracks line numbers, and deals with positional information.

use std::fmt;
use std::fmt::Display;
use std::ops::Range;
use std::str::Lines;
use std::sync::Arc;

use ruff_source_file::LineColumn;
use ruff_source_file::LineIndex;
use ruff_source_file::OneIndexed;
use ruff_source_file::PositionEncoding;
use ruff_source_file::SourceLocation;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use serde::Serialize;

#[derive(Debug, Clone)]
pub struct LinedBuffer {
    buffer: Arc<String>,
    lines: LineIndex,
}

impl LinedBuffer {
    pub fn new(buffer: Arc<String>) -> Self {
        let lines = LineIndex::from_source_text(&buffer);
        Self { buffer, lines }
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn contents(&self) -> &Arc<String> {
        &self.buffer
    }

    pub fn lines(&self) -> Lines<'_> {
        self.buffer.lines()
    }

    pub fn line_column(&self, offset: TextSize) -> LineColumn {
        assert!(
            offset.to_usize() <= self.len(),
            "offset out of range, expected {} <= {}",
            offset.to_usize(),
            self.len()
        );
        self.lines.line_column(offset, &self.buffer)
    }

    pub fn code_at(&self, range: TextRange) -> &str {
        match self.buffer.get(Range::<usize>::from(range)) {
            Some(code) => code,
            None => panic!(
                "`range` is invalid, got {range:?}, but file is {} bytes long",
                self.buffer.len()
            ),
        }
    }

    pub fn to_text_size(&self, line: u32, column: u32) -> TextSize {
        self.lines.offset(
            SourceLocation {
                line: OneIndexed::from_zero_indexed(line as usize),
                character_offset: OneIndexed::from_zero_indexed(column as usize),
            },
            &self.buffer,
            PositionEncoding::Utf32,
        )
    }

    pub fn to_text_range(&self, source_range: &UserRange) -> TextRange {
        TextRange::new(
            self.to_text_size(
                source_range.start.line.to_zero_indexed() as u32,
                source_range.start.column.to_zero_indexed() as u32,
            ),
            self.to_text_size(
                source_range.end.line.to_zero_indexed() as u32,
                source_range.end.column.to_zero_indexed() as u32,
            ),
        )
    }

    /// Gets the content from the beginning of start_line to the end of end_line.
    pub fn content_in_line_range(&self, start_line: OneIndexed, end_line: OneIndexed) -> &str {
        debug_assert!(start_line <= end_line);
        let start = self.lines.line_start(start_line, &self.buffer);
        let end = self.lines.line_end(end_line, &self.buffer);
        &self.buffer[start.to_usize()..end.to_usize()]
    }

    pub fn line_start(&self, line: OneIndexed) -> TextSize {
        self.lines.line_start(line, &self.buffer)
    }
}

/// A range in a file, with a start and end, both containing line and column.
/// Stored in terms of characters, not including any BOM.
#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq, Hash, Default)]
pub struct UserRange {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Serialize for UserRange {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("UserRange", 4)?;
        state.serialize_field("start_line", &self.start.line.get())?;
        state.serialize_field("start_col", &self.start.column.get())?;
        state.serialize_field("end_line", &self.end.line.get())?;
        state.serialize_field("end_col", &self.end.column.get())?;
        state.end()
    }
}

impl Display for UserRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.line == self.end.line {
            if self.start.column == self.end.column {
                write!(f, "{}:{}", self.start.line, self.start.column)
            } else {
                write!(
                    f,
                    "{}:{}-{}",
                    self.start.line, self.start.column, self.end.column
                )
            }
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.start.line, self.start.column, self.end.line, self.end.column
            )
        }
    }
}
