/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::LazyLock;

use regex::Regex;

#[expect(dead_code)]
/// Regex for matching a Python executable, copied from MS vscode-python. The correct pattern will be selected
/// for the user's current platform.
///
/// [posix]: https://github.com/microsoft/vscode-python/blob/b0b8aff14bfa25b5072c05e1b75581430ce532c2/src/client/pythonEnvironments/common/posixUtils.ts#L24
/// [windows]: https://github.com/microsoft/vscode-python/blob/b0b8aff14bfa25b5072c05e1b75581430ce532c2/src/client/pythonEnvironments/common/windowsUtils.ts#L32
static PYTHON_INTERPRETER_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    let pattern = if cfg!(windows) {
        r"^python(\d+(.\d+)?)?\.exe$"
    } else {
        r"^python(\d+(\.\d+)?)?$"
    };
    Regex::new(pattern).unwrap()
});

#[expect(dead_code)]
/// A trait for structs with the ability to search for a Python interpreter installation.
pub trait Finder {
    fn find(project_path: &Path) -> Option<PathBuf>;
}
