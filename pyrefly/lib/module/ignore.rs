/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Given a file, record which ignore statements are in it.
//!
//! Given `# type: ignore` we should ignore errors on that line.
//! Originally specified in <https://peps.python.org/pep-0484/>.
//!
//! You can also use the name of the linter, e.g. `# pyright: ignore`,
//! `# pyrefly: ignore`, `# mypy: ignore`.
//!
//! You can specify a specific error code, e.g. `# type: ignore[invalid-type]`.
//! Note that Pyright will only honor such codes after `# pyright: ignore[code]`.
//!
//! You can also use `# mypy: ignore-errors`, `# pyrefly: ignore-errors`
//! or `# type: ignore` at the beginning of a file to surpress all errors.
//!
//! For Pyre compatibility we also allow `# pyre-ignore` and `# pyre-fixme`
//! as equivalents to `pyre: ignore`, and `# pyre-ignore-all-errors` as
//! an equivalent to `type: ignore-errors`.
//!
//! We are permissive with whitespace, allowing `#type:ignore[code]` and
//! `#  type:  ignore  [  code  ]`, but do not allow a space after the colon.

use std::str::FromStr;

use dupe::Dupe;
use pyrefly_util::lined_buffer::LineNumber;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::error::kind::ErrorKind;

/// The name of the tool that is being suppressed.
#[derive(PartialEq, Debug, Clone, Hash, Eq, Dupe, Copy)]
pub enum Tool {
    /// Indicates a `type: ignore`.
    Any,
    /// Indicates a `pyrefly: ignore`.
    Pyrefly,
    /// Includes the `pyre-ignore` and `pyre-fixme` hints, along with `pyre: ignore`.
    Pyre,
    Pyright,
    Mypy,
    Ty,
}

impl Tool {
    /// The maximum length of any tool.
    const MAX_LEN: usize = 7;

    fn from_comment(x: &str) -> Option<Self> {
        match x {
            "type" => Some(Tool::Any),
            "pyrefly" => Some(Tool::Pyrefly),
            "pyre" => Some(Tool::Pyre),
            "pyright" => Some(Tool::Pyright),
            "mypy" => Some(Tool::Mypy),
            "ty" => Some(Tool::Ty),
            _ => None,
        }
    }
}

/// A simple lexer that deals with the rules around whitespace.
/// As it consumes the string, it will move forward.
struct Lexer<'a>(&'a str);

impl<'a> Lexer<'a> {
    /// The string starts with the given string, return `true` if so.
    fn starts_with(&mut self, x: &str) -> bool {
        match self.0.strip_prefix(x) {
            Some(x) => {
                self.0 = x;
                true
            }
            None => false,
        }
    }

    /// The string starts with `tool:`, return the tool if it does.
    fn starts_with_tool(&mut self) -> Option<Tool> {
        let p = self
            .0
            .as_bytes()
            .iter()
            .take(Tool::MAX_LEN + 1)
            .position(|&c| c == b':')?;
        let tool = Tool::from_comment(&self.0[..p])?;
        self.0 = &self.0[p + 1..];
        Some(tool)
    }

    /// Trim whitespace from the start of the string.
    fn trim_start(&mut self) {
        self.0 = self.0.trim_start();
    }

    /// Return `true` if the string is empty or only whitespace.
    fn blank(&mut self) -> bool {
        self.0.trim_start().is_empty()
    }
}

#[derive(PartialEq, Debug, Clone, Hash, Eq, Dupe, Copy)]
pub struct Suppression {
    tool: Tool,
    kind: Option<ErrorKind>,
}

/// Record the position of `# type: ignore[valid-type]` statements.
/// For now we don't record the content of the ignore, but we could.
#[derive(Debug, Clone, Default)]
pub struct Ignore {
    ignores: SmallMap<LineNumber, Vec<Suppression>>,
    /// Do we have a generic or Pyrefly-specific ignore-all directive?
    ignore_all_strict: bool,
    /// Do we have any ignore-all directive, regardless of tool?
    ignore_all_permissive: bool,
}

impl Ignore {
    pub fn new(code: &str) -> Self {
        let ignores = Self::parse_ignores(code);
        let ignore_all = Self::parse_ignore_all(code);
        let ignore_all_strict =
            ignore_all.contains_key(&Tool::Pyrefly) || ignore_all.contains_key(&Tool::Any);
        let ignore_all_permissive = !ignore_all.is_empty();
        Self {
            ignores,
            ignore_all_strict,
            ignore_all_permissive,
        }
    }

    /// All the errors that were ignored, and the line number that ignore happened.
    fn parse_ignore_all(code: &str) -> SmallMap<Tool, LineNumber> {
        // process top level comments
        let mut res = SmallMap::new();
        let mut prev_ignore = None;
        for (line, x) in code
            .lines()
            .map(|x| x.trim())
            .take_while(|x| x.is_empty() || x.starts_with('#'))
            .enumerate()
        {
            let line = LineNumber::from_zero_indexed(line as u32);
            if let Some((tool, line)) = prev_ignore {
                // We consider any `# type: ignore` followed by a line with code to be a
                // normal suppression, not an ignore-all directive.
                res.entry(tool).or_insert(line);
                prev_ignore = None;
            }

            let mut lex = Lexer(x);
            if !lex.starts_with("#") {
                continue;
            }
            lex.trim_start();
            if lex.starts_with("pyre-ignore-all-errors") {
                res.entry(Tool::Pyre).or_insert(line);
            } else if let Some(tool) = lex.starts_with_tool() {
                lex.trim_start();
                if lex.starts_with("ignore-errors") && lex.blank() {
                    res.entry(tool).or_insert(line);
                } else if lex.starts_with("ignore") && lex.blank() {
                    prev_ignore = Some((tool, line));
                }
            }
        }
        res
    }

    fn parse_ignores(code: &str) -> SmallMap<LineNumber, Vec<Suppression>> {
        let mut ignores: SmallMap<LineNumber, Vec<Suppression>> = SmallMap::new();
        // If we see a comment on a non-code line, move it to the next non-comment line.
        let mut pending = Vec::new();
        let mut line = LineNumber::default();
        for (idx, x) in code.lines().enumerate() {
            line = LineNumber::from_zero_indexed(idx as u32);
            let mut xs = x.split('#');
            let first = xs.next().unwrap_or("");
            if let Some(supp) = Self::get_suppression_kind(x) {
                if first.trim_start().is_empty() {
                    pending.push(supp);
                } else {
                    ignores.entry(line).or_default().push(supp);
                }
            } else if !pending.is_empty() && (x.is_empty() || !first.trim_start().is_empty()) {
                ignores.entry(line).or_default().append(&mut pending);
            }
        }
        if !pending.is_empty() {
            ignores
                .entry(line.increment())
                .or_default()
                .append(&mut pending);
        }
        ignores
    }

    fn get_suppression_kind(line: &str) -> Option<Suppression> {
        fn match_pyrefly_ignore(line: &str) -> Option<Suppression> {
            let mut words = line.split_whitespace();
            if let Some("pyrefly:") = words.next() {
                if let Some(word) = words.next() {
                    if word == "ignore" {
                        return Some(Suppression {
                            tool: Tool::Pyrefly,
                            kind: None,
                        });
                    }

                    if let Some(word) = word.strip_prefix("ignore[")
                        && let Some(word) = word.strip_suffix(']')
                    {
                        if let Ok(kind) = ErrorKind::from_str(word) {
                            return Some(Suppression {
                                tool: Tool::Pyrefly,
                                kind: Some(kind),
                            });
                        }
                    }
                }
            }
            None
        }

        for l in line.split("#").skip(1) {
            let l = l.trim_start();
            if let Some(l) = l.strip_prefix("type:")
                && l.trim_start().starts_with("ignore")
            {
                return Some(Suppression {
                    tool: Tool::Any,
                    kind: None,
                });
            } else if let Some(value) = match_pyrefly_ignore(l) {
                return Some(value);
            } else if l.starts_with("pyre-ignore") || l.starts_with("pyre-fixme") {
                return Some(Suppression {
                    tool: Tool::Pyre,
                    kind: None,
                });
            }
        }
        None
    }

    pub fn is_ignored(
        &self,
        start_line: LineNumber,
        end_line: LineNumber,
        kind: ErrorKind,
        permissive_ignores: bool,
    ) -> bool {
        if self.ignore_all_strict || (permissive_ignores && self.ignore_all_permissive) {
            return true;
        }

        // We allow an ignore on any line within the range.
        // We convert to/from zero-indexed because LineNumber does not implement Step.
        for line in start_line.to_zero_indexed()..=end_line.to_zero_indexed() {
            if let Some(suppressions) = self.ignores.get(&LineNumber::from_zero_indexed(line)) {
                if suppressions.iter().any(|supp| match supp.tool {
                    // We only check the subkind if they do `# ignore: pyrefly`
                    Tool::Pyrefly => supp.kind.is_none_or(|x| x == kind),
                    Tool::Any => true,
                    _ => permissive_ignores,
                }) {
                    return true;
                }
            }
        }

        false
    }

    /// Get all pyrefly ignores.
    pub fn get_pyrefly_ignores(&self) -> SmallSet<LineNumber> {
        self.ignores
            .iter()
            .filter(|ignore| {
                ignore
                    .1
                    .iter()
                    .any(|s| s.tool == Tool::Pyrefly && s.kind.is_none())
            })
            .map(|(line, _)| *line)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use starlark_map::smallmap;

    use super::*;

    #[test]
    fn test_get_suppression_kind() {
        assert!(Ignore::get_suppression_kind("stuff # type: ignore # and then stuff").is_some());
        assert!(Ignore::get_suppression_kind("more # stuff # type: ignore[valid-type]").is_some());
        assert!(Ignore::get_suppression_kind("# ignore: pyrefly").is_none());
        assert!(Ignore::get_suppression_kind(" pyrefly: ignore").is_none());
        assert!(Ignore::get_suppression_kind("normal line").is_none());
        assert!(
            Ignore::get_suppression_kind("# pyrefly: ignore")
                == Some(Suppression {
                    tool: Tool::Pyrefly,
                    kind: None
                })
        );
        assert!(
            Ignore::get_suppression_kind("# pyrefly: ignore[bad-return]")
                == Some(Suppression {
                    tool: Tool::Pyrefly,
                    kind: Some(ErrorKind::BadReturn)
                })
        );
        assert!(Ignore::get_suppression_kind("# pyrefly: ignore[]").is_none());
        assert!(Ignore::get_suppression_kind("# pyrefly: ignore[bad-]").is_none());
    }

    #[test]
    fn test_parse_ignore_all() {
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# pyrefly: ignore-errors
x = 5
"#
            ),
            smallmap! {Tool::Pyrefly => LineNumber::from_zero_indexed(1)}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# comment
# pyrefly: ignore-errors
x = 5
"#
            ),
            smallmap! {Tool::Pyrefly => LineNumber::from_zero_indexed(2)}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# comment
  # indented comment
# pyrefly: ignore-errors
x = 5
"#
            ),
            smallmap! {Tool::Pyrefly => LineNumber::from_zero_indexed(3)}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
x = 5
# pyrefly: ignore-errors
"#
            ),
            smallmap! {}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# type: ignore

x = 5
"#
            ),
            smallmap! {Tool::Any => LineNumber::from_zero_indexed(1)}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# comment
# type: ignore
# comment
x = 5
"#
            ),
            smallmap! {Tool::Any => LineNumber::from_zero_indexed(2)}
        );
        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# type: ignore
x = 5
"#
            ),
            smallmap! {}
        );

        assert_eq!(
            Ignore::parse_ignore_all(
                r#"
# pyre-ignore-all-errors
x = 5
"#
            ),
            smallmap! {Tool::Pyre => LineNumber::from_zero_indexed(1)}
        );
    }
}
