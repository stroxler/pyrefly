/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use pyrefly_util::arc_id::ArcId;
use pyrefly_util::lined_buffer::LinedBuffer;
use pyrefly_util::lined_buffer::UserPos;
use pyrefly_util::lined_buffer::UserRange;
use ruff_python_ast::ModModule;
use ruff_source_file::OneIndexed;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use vec1::vec1;

use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::module::ignore::Ignore;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::python::ast::Ast;
use crate::python::sys_info::PythonVersion;

pub static GENERATED_TOKEN: &str = concat!("@", "generated");

/// Information about a module, notably its name, path, and contents.
#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct ModuleInfo(ArcId<ModuleInfoInner>);

#[derive(Debug, Clone)]
struct ModuleInfoInner {
    name: ModuleName,
    path: ModulePath,
    ignore: Ignore,
    is_generated: bool,
    contents: LinedBuffer,
}

impl ModuleInfo {
    /// Create a new ModuleInfo. Will NOT read the `path`, but use the value from `contents` instead.
    pub fn new(name: ModuleName, path: ModulePath, contents: Arc<String>) -> Self {
        let ignore = Ignore::new(&contents);
        let is_generated = contents.contains(GENERATED_TOKEN);
        let contents = LinedBuffer::new(contents);
        Self(ArcId::new(ModuleInfoInner {
            name,
            path,
            ignore,
            is_generated,
            contents,
        }))
    }

    pub fn lined_buffer(&self) -> &LinedBuffer {
        &self.0.contents
    }

    pub fn line_count(&self) -> usize {
        // By default we count the empty lines, but sometimes to get stats
        // we might need to only count the non-empty/non-comment lines.
        const COUNT_EMPTY_LINES: bool = true;
        if COUNT_EMPTY_LINES {
            self.0.contents.line_count()
        } else {
            self.0
                .contents
                .lines()
                .filter(|x| {
                    let res = x.trim_start();
                    !res.is_empty() && !res.starts_with('#')
                })
                .count()
        }
    }

    pub fn user_range(&self, range: TextRange) -> UserRange {
        self.0.contents.user_range(range)
    }

    pub fn line_column(&self, offset: TextSize) -> UserPos {
        self.0.contents.line_column(offset)
    }

    pub fn code_at(&self, range: TextRange) -> &str {
        self.0.contents.code_at(range)
    }

    pub fn path(&self) -> &ModulePath {
        &self.0.path
    }

    pub fn is_generated(&self) -> bool {
        self.0.is_generated
    }

    pub fn contents(&self) -> &Arc<String> {
        self.0.contents.contents()
    }

    pub fn parse(&self, version: PythonVersion, errors: &ErrorCollector) -> ModModule {
        let (module, parse_errors) = Ast::parse(self.contents());
        for err in parse_errors {
            errors.add(
                err.location,
                ErrorKind::ParseError,
                None,
                vec1![format!("Parse error: {}", err.error)],
            );
        }
        SemanticSyntaxContext::new(self.contents(), version, errors).visit(&module);
        module
    }

    pub fn name(&self) -> ModuleName {
        self.0.name
    }

    pub fn is_ignored(&self, source_range: &UserRange) -> bool {
        // Extend the range of the error to include comment lines before it.
        // This makes it so that the preceding ignore could "see through" comments.
        let start_line = {
            let mut start_line = source_range.start.line;
            while let Some(earlier_line) = start_line.checked_sub(OneIndexed::MIN) {
                let earlier_line_content = &self
                    .lined_buffer()
                    .content_in_line_range(earlier_line, earlier_line)
                    .trim();
                if Ignore::get_suppression_kind(earlier_line_content).is_some() {
                    break;
                } else if earlier_line_content.starts_with('#') {
                    start_line = earlier_line;
                } else {
                    break;
                }
            }
            start_line
        };
        self.0.ignore.is_ignored(start_line, source_range.end.line)
    }

    pub fn ignore(&self) -> &Ignore {
        &self.0.ignore
    }
}

#[derive(Debug, Clone)]
pub struct TextRangeWithModuleInfo {
    pub module_info: ModuleInfo,
    pub range: TextRange,
}

impl TextRangeWithModuleInfo {
    pub fn new(module_info: ModuleInfo, range: TextRange) -> Self {
        Self { module_info, range }
    }
}

pub struct SemanticSyntaxContext<'me> {
    content: &'me str,
    version: ruff_python_ast::PythonVersion,
    errors: &'me ErrorCollector,
}

impl<'me> SemanticSyntaxContext<'me> {
    pub fn new(content: &'me str, version: PythonVersion, errors: &'me ErrorCollector) -> Self {
        Self {
            content,
            version: ruff_python_ast::PythonVersion {
                major: version.major as u8,
                minor: version.minor as u8,
            },
            errors,
        }
    }

    pub fn visit(&self, module: &ModModule) {
        let mut checker = ruff_python_parser::semantic_errors::SemanticSyntaxChecker::new();
        module.body.iter().for_each(|stmt| {
            checker.visit_stmt(stmt, self);
        });
    }
}

impl<'me> ruff_python_parser::semantic_errors::SemanticSyntaxContext
    for SemanticSyntaxContext<'me>
{
    fn python_version(&self) -> ruff_python_ast::PythonVersion {
        self.version
    }

    fn report_semantic_error(
        &self,
        error: ruff_python_parser::semantic_errors::SemanticSyntaxError,
    ) {
        self.errors.add(
            error.range,
            ErrorKind::InvalidSyntax,
            None,
            vec1![error.to_string()],
        );
    }

    fn future_annotations_or_stub(&self) -> bool {
        false
    }

    fn source(&self) -> &str {
        self.content
    }

    fn global(&self, _: &str) -> Option<TextRange> {
        // TODO: Properly implement this
        None
    }

    fn in_async_context(&self) -> bool {
        // TODO: Properly implement this
        true
    }

    fn in_await_allowed_context(&self) -> bool {
        // TODO: Properly implement this
        true
    }

    fn in_yield_allowed_context(&self) -> bool {
        // TODO: Properly implement this
        true
    }

    fn in_sync_comprehension(&self) -> bool {
        // TODO: Properly implement this
        false
    }

    fn in_module_scope(&self) -> bool {
        // TODO: Properly implement this
        false
    }

    fn in_function_scope(&self) -> bool {
        // TODO: Properly implement this
        true
    }

    fn in_generator_scope(&self) -> bool {
        // TODO: Properly implement this
        false
    }

    fn in_notebook(&self) -> bool {
        // TODO: Properly implement this
        false
    }
}
