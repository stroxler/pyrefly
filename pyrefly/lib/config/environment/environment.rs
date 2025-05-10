/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::LazyLock;

use anyhow::Context;
use anyhow::anyhow;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use starlark_map::small_map::SmallMap;
use tracing::error;
#[cfg(not(target_arch = "wasm32"))]
use which::which;

use crate::config::environment::finder::Finder as _;
use crate::config::environment::venv::Venv;
use crate::sys_info::PythonPlatform;
use crate::sys_info::PythonVersion;
use crate::util::lock::Mutex;

static INTERPRETER_ENV_REGISTRY: LazyLock<Mutex<SmallMap<PathBuf, Option<PythonEnvironment>>>> =
    LazyLock::new(|| Mutex::new(SmallMap::new()));

/// Values representing the environment of the Python interpreter.
/// These values are `None` by default, so we can tell if a config
/// overrode them, or if we should query a Python interpreter for
/// any missing values. We can't query a Python interpreter
/// on config parsing, since we also won't know if an executable
/// other than the first available on the path should be used (i.e.
/// should we always look at a venv/conda environment instead?)
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone, Default)]
pub struct PythonEnvironment {
    /// The platform any `sys.platform` check should evaluate against.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub python_platform: Option<PythonPlatform>,

    /// The platform any `sys.version` check should evaluate against.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub python_version: Option<PythonVersion>,

    /// Directories containing third-party package imports, searched
    /// after first checking `search_path` and `typeshed`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub site_package_path: Option<Vec<PathBuf>>,

    /// Is the `site_package_path` here one we got from
    /// querying an interpreter?
    #[serde(skip, default)]
    pub site_package_path_from_interpreter: bool,
}

impl PythonEnvironment {
    const DEFAULT_INTERPRETERS: &[&str] = &["python3", "python"];

    pub fn new(
        python_platform: PythonPlatform,
        python_version: PythonVersion,
        site_package_path: Vec<PathBuf>,
    ) -> Self {
        Self {
            python_platform: Some(python_platform),
            python_version: Some(python_version),
            site_package_path: Some(site_package_path),
            site_package_path_from_interpreter: false,
        }
    }

    fn pyrefly_default() -> Self {
        Self::new(Default::default(), Default::default(), Default::default())
    }

    /// Are any Python environment values `None`?
    pub fn any_empty(&self) -> bool {
        self.python_platform.is_none()
            || self.python_version.is_none()
            || self.site_package_path.is_none()
    }

    /// If any Python environment values are `None`, set them to
    /// Pyrefly's default value.
    pub fn set_empty_to_default(&mut self) {
        if self.python_platform.is_none() {
            self.python_platform = Some(PythonPlatform::default());
        }
        if self.python_version.is_none() {
            self.python_version = Some(PythonVersion::default());
        }
        if self.site_package_path.is_none() {
            self.site_package_path = Some(Vec::new());
            self.site_package_path_from_interpreter = false;
        }
    }

    /// Given another `PythonEnvironment`, override any `None` values
    /// in this `PythonEnvironment` with the other environment's values.
    pub fn override_empty(&mut self, other: Self) {
        if self.python_platform.is_none() {
            self.python_platform = other.python_platform;
        }
        if self.python_version.is_none() {
            self.python_version = other.python_version;
        }
        if self.site_package_path.is_none() {
            self.site_package_path = other.site_package_path;
            self.site_package_path_from_interpreter = other.site_package_path_from_interpreter;
        }
    }

    /// Given a path to a Python interpreter executable, query that interpreter for its
    /// version, platform, and site package path. Return an error in the case of failure during
    /// execution, parsing, or deserializing.
    fn get_env_from_interpreter(interpreter: &Path) -> anyhow::Result<PythonEnvironment> {
        let script = "\
import json, site, sys
platform = sys.platform
v = sys.version_info
version = '{}.{}.{}'.format(v.major, v.minor, v.micro)
packages = site.getsitepackages()
if site.ENABLE_USER_SITE:
    packages.insert(0, site.getusersitepackages())
print(json.dumps({'python_platform': platform, 'python_version': version, 'site_package_path': packages}))
";

        let mut command = Command::new(interpreter);
        command.arg("-c");
        command.arg(script);

        let python_info = command.output()?;

        let stdout = String::from_utf8(python_info.stdout).with_context(|| {
            format!(
                "while parsing Python interpreter (`{}`) stdout for environment configuration",
                interpreter.display()
            )
        })?;
        if !python_info.status.success() {
            let stderr = String::from_utf8(python_info.stderr)
                .unwrap_or("<Failed to parse STDOUT from UTF-8 string>".to_owned());
            return Err(anyhow::anyhow!(
                "Unable to query interpreter {} for environment info:\nSTDOUT: {}\nSTDERR: {}",
                interpreter.display(),
                stdout,
                stderr
            ));
        }

        let mut deserialized: PythonEnvironment = serde_json::from_str(&stdout)?;

        deserialized.python_platform.as_ref().ok_or_else(|| {
            anyhow!("Expected `python_platform` from Python interpreter query to be non-empty")
        })?;
        deserialized.python_version.as_ref().ok_or_else(|| {
            anyhow!("Expected `python_version` from Python interpreter query to be non-empty")
        })?;
        deserialized.site_package_path.as_ref().ok_or_else(|| {
            anyhow!("Expected `site_package_path` from Python interpreter query to be non-empty")
        })?;

        deserialized.site_package_path_from_interpreter = true;

        Ok(deserialized)
    }

    /// Get the first interpreter available on the path by using `which`
    /// and querying for [`Self::DEFAULT_INTERPRETERS`] in order.
    pub fn get_default_interpreter() -> Option<&'static Path> {
        static SYSTEM_INTERP: LazyLock<Option<PathBuf>> = LazyLock::new(|| {
            // disable query with `which` on wasm
            #[cfg(not(target_arch = "wasm32"))]
            for interpreter in PythonEnvironment::DEFAULT_INTERPRETERS {
                if let Ok(interpreter_path) = which(interpreter) {
                    return Some(interpreter_path);
                }
            }
            None
        });
        SYSTEM_INTERP.as_deref()
    }

    /// Given a path to an interpreter, query the interpreter with
    /// [`Self::get_env_from_interpreter()`] and cache the result. If a cached
    /// result already exists, return that.
    ///
    /// In the case of failure, log an error message and return Pyrefly's
    /// [`PythonEnvironment::default()`].
    pub fn get_interpreter_env(interpreter: &Path) -> PythonEnvironment {
        INTERPRETER_ENV_REGISTRY.lock()
        .entry(interpreter.to_path_buf()).or_insert_with(move || {
            Self::get_env_from_interpreter(interpreter).inspect_err(|e| {
                error!("Failed to query interpreter, falling back to default Python environment settings\n{}", e);
            }).ok()
        }).clone().unwrap_or_else(Self::pyrefly_default)
    }

    /// [`Self::get_default_interpreter()`] and [`Self::get_interpreter_env()`] with the resulting value,
    /// or return [`PythonEnvironment::default()`] if `None`.
    pub fn get_default_interpreter_env() -> PythonEnvironment {
        Self::get_default_interpreter()
            .map_or_else(Self::pyrefly_default, Self::get_interpreter_env)
    }

    /// Uses the same logic as [vscode-python] to [find interpreters] that should be used for the given
    /// project. This function will use the same [non-workspace] logic as vscode-python, and
    /// workspace-specific logic will be handled directly by VSCode and provided to our extension.
    ///
    /// [vscode-python]: https://github.com/microsoft/vscode-python
    /// [find interpreters]: https://github.com/microsoft/vscode-python/blob/main/src/client/interpreter/autoselection/index.ts#l240-l242
    /// [non-workspace]: https://github.com/microsoft/vscode-python/blob/main/src/client/pythonEnvironments/index.ts#L173
    pub fn find_interpreter(path: Option<&Path>) -> Option<PathBuf> {
        if let Some(start_path) = path {
            let venv = Venv::find(start_path);
            if venv.is_some() {
                return venv;
            }
        }
        Self::get_default_interpreter().map(|p| p.to_path_buf())
    }
}

impl Display for PythonEnvironment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{python_platform: {}, python_version: {}, site_package_path: [{}], site_package_path_from_interpreter: {}}}",
            self.python_platform
                .as_ref()
                .map_or_else(|| "None".to_owned(), |platform| platform.to_string()),
            self.python_version
                .map_or_else(|| "None".to_owned(), |version| version.to_string()),
            self.site_package_path.as_ref().map_or_else(
                || "".to_owned(),
                |path| path.iter().map(|p| p.display()).join(", ")
            ),
            self.site_package_path_from_interpreter,
        )
    }
}
