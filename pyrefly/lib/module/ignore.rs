/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_source_file::OneIndexed;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::module::module_info::SourceRange;

#[derive(PartialEq, Debug, Clone, Hash, Eq, Dupe, Copy)]
pub enum SuppressionKind {
    Ignore,
    Pyre,
    Pyrefly,
}

/// Record the position of `# type: ignore[valid-type]` statements.
/// For now we don't record the content of the ignore, but we could.
#[derive(Debug, Clone, Default)]
pub struct Ignore {
    ignores: SmallMap<OneIndexed, Vec<SuppressionKind>>,
    ignore_all: bool,
}

impl Ignore {
    pub fn new(code: &str) -> Self {
        let mut ignore_all = false;

        // process top level comments
        for line_str in code.lines() {
            // Skip blank lines
            if line_str.trim().is_empty() {
                continue;
            }
            // If the line is a comment, check if it's exactly "# pyrefly: ignore-all-errors"
            if !line_str.starts_with("#") {
                break;
            } else if line_str.trim() == "# pyrefly: ignore-all-errors" {
                ignore_all = true;
                break; // We found the top-level ignore comment, no need to check further
            }
        }

        // process line level comments
        let mut ignores: SmallMap<OneIndexed, Vec<SuppressionKind>> = SmallMap::new();
        for (line, line_str) in code.lines().enumerate() {
            if let Some(kind) = Self::get_suppression_kind(line_str) {
                ignores.insert(OneIndexed::from_zero_indexed(line), [kind].to_vec());
            }
        }
        Self {
            ignores,
            ignore_all,
        }
    }

    fn get_suppression_kind(line: &str) -> Option<SuppressionKind> {
        fn match_pyrefly_ignore(line: &str) -> bool {
            let mut words = line.split_whitespace();
            if let Some("pyrefly:") = words.next() {
                words.next() == Some("ignore")
            } else {
                false
            }
        }

        for l in line.split("# ").skip(1) {
            if l.starts_with("type: ignore") {
                return Some(SuppressionKind::Ignore);
            } else if match_pyrefly_ignore(l) {
                return Some(SuppressionKind::Pyrefly);
            } else if l.starts_with("pyre-ignore") || l.starts_with("pyre-fixme") {
                return Some(SuppressionKind::Pyre);
            }
        }
        None
    }

    pub fn is_ignored(&self, range: &SourceRange, msg: &str) -> bool {
        if self.ignore_all {
            true
        } else {
            // for now, we ignore the msg
            let _unused = msg;
            // We allow an ignore the line before the range, or on any line within the range.
            // We convert to/from zero-indexed because OneIndexed does not implement Step.
            (range.start.row.to_zero_indexed().saturating_sub(1)..=range.end.row.to_zero_indexed())
                .any(|x| self.ignores.contains_key(&OneIndexed::from_zero_indexed(x)))
        }
    }

    /// Get all the ignores of a given kind.
    pub fn get_ignores(&self, kind: SuppressionKind) -> SmallSet<OneIndexed> {
        self.ignores
            .iter()
            .filter(|ignore| ignore.1.contains(&kind))
            .map(|(line, _)| *line)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_suppression_kind() {
        assert!(Ignore::get_suppression_kind("stuff # type: ignore # and then stuff").is_some());
        assert!(Ignore::get_suppression_kind("more # stuff # type: ignore[valid-type]").is_some());
        assert!(Ignore::get_suppression_kind("# ignore: pyrefly").is_none());
        assert!(Ignore::get_suppression_kind(" pyrefly: ignore").is_none());
        assert!(Ignore::get_suppression_kind("normal line").is_none());
    }
}
