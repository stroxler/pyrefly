/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Export-only provenance adapter. The generic analysis frontend does not depend
//! on Pyrefly's binding graph or solved Answers. This adapter owns an immutable
//! source universe, so recorded dependencies need no incremental invalidation.

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;

use pyrefly::export::exports::ExportLocation;
use pyrefly::export::exports::ExportOrigin;
use pyrefly::export::exports::Exports;
use pyrefly::export::exports::LookupExport;
use pyrefly::export::special::SpecialExport;
use pyrefly::state::loader::FindError;
use pyrefly::state::loader::FindingOrError;
use pyrefly_python::ast::Ast;
use pyrefly_python::module::Module;
use pyrefly_python::module_name::ModuleName;
use pyrefly_python::module_path::ModulePath;
use pyrefly_python::sys_info::SysInfo;
use pyrefly_static_analysis::ExportProvider;
use pyrefly_static_analysis::ExportTarget;
use pyrefly_types::function::Deprecation;
use ruff_python_ast::PySourceType;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

mod modules;

type ExportTable = Arc<SmallMap<Name, ExportLocation>>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum Calculation {
    Exports,
    Wildcard,
}

type CalculationKey = (ModuleName, Calculation);

/// A source or stub supplied by the caller; no filesystem discovery is performed.
pub struct Source {
    pub module: String,
    pub path: PathBuf,
    pub contents: String,
}

/// Static declaration provenance is not evidence of runtime purity or identity.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Resolution {
    Found { module: String, name: String },
    Module(String),
    Missing,
    Unknown(String),
}

/// Pyrefly's real export calculation over an explicitly supplied source universe.
pub struct ExportIndex {
    modules: HashMap<ModuleName, Exports>,
    module_values: HashMap<(ModuleName, Name), Option<ModuleName>>,
    dependencies: RefCell<BTreeSet<(String, Option<String>)>>,
    active: RefCell<Vec<CalculationKey>>,
    incomplete: RefCell<HashMap<CalculationKey, BTreeSet<String>>>,
}

impl ExportIndex {
    /// Parse modules and compute declaration metadata without building Bindings.
    pub fn new(sources: Vec<Source>, sys_info: SysInfo) -> Result<Self, String> {
        let mut modules = HashMap::new();
        let mut module_values = HashMap::new();
        for source in sources {
            let name = ModuleName::from_str(&source.module);
            let (parsed, errors, unsupported) =
                Ast::parse_with_version(&source.contents, sys_info.version(), PySourceType::Python);
            if !errors.is_empty() || !unsupported.is_empty() {
                return Err(format!(
                    "cannot parse {}: {errors:?} {unsupported:?}",
                    source.module
                ));
            }
            let path = ModulePath::memory(source.path);
            module_values.extend(
                modules::classify(&source.contents, name, path.is_init())
                    .into_iter()
                    .map(|(symbol, value)| ((name, symbol), value)),
            );
            let info = Module::new(name, path, Arc::new(source.contents));
            let exports = Exports::new(&parsed.into_syntax().body, &info, sys_info, false);
            if modules.insert(name, exports).is_some() {
                return Err(format!("duplicate source module {}", source.module));
            }
        }
        Ok(Self {
            modules,
            module_values,
            dependencies: RefCell::new(BTreeSet::new()),
            active: RefCell::new(Vec::new()),
            incomplete: RefCell::new(HashMap::new()),
        })
    }

    /// Resolve a named declaration through renamed and wildcard re-exports.
    /// Missing/cyclic/synthesized declarations are never reported as found.
    pub fn resolve(&self, module: &str, name: &str) -> Resolution {
        let mut module = ModuleName::from_str(module);
        let mut name = Name::new(name);
        let mut seen = HashSet::new();
        loop {
            self.record(module, Some(&name));
            if !seen.insert((module, name.clone())) {
                return Resolution::Unknown("cyclic re-export".to_owned());
            }
            if !self.modules.contains_key(&module) {
                return Resolution::Missing;
            }
            let Some(table) = self.table(module) else {
                return Resolution::Unknown(self.errors((module, Calculation::Exports)).join("; "));
            };
            if let Some(value) = self.module_values.get(&(module, name.clone())) {
                return match value {
                    Some(module) => Resolution::Module(module.to_string()),
                    None => Resolution::Unknown("ambiguous module-valued export".to_owned()),
                };
            }
            match table.get(&name) {
                Some(ExportLocation::ThisModule(export)) if export.symbol_kind.is_some() => {
                    return Resolution::Found {
                        module: module.to_string(),
                        name: name.to_string(),
                    };
                }
                Some(ExportLocation::ThisModule(_)) => {
                    return Resolution::Unknown("synthesized or invalid declaration".to_owned());
                }
                Some(ExportLocation::OtherModule(next, alias)) => {
                    module = *next;
                    if let Some(alias) = alias {
                        name = alias.clone();
                    }
                }
                None if table.contains_key(&Name::new("__getattr__")) => {
                    return Resolution::Unknown("dynamic module attribute".to_owned());
                }
                None if self.modules.contains_key(&module.append(&name)) => {
                    return Resolution::Module(module.append(&name).to_string());
                }
                None => return Resolution::Missing,
            }
        }
    }

    /// Enumerate `import *` names. An incomplete calculation is distinct from empty.
    pub fn wildcard(&self, module: &str) -> Result<Vec<String>, Vec<String>> {
        let id = ModuleName::from_str(module);
        let names = self.get_wildcard(id);
        let issues = self.errors((id, Calculation::Wildcard));
        if !issues.is_empty() {
            return Err(issues);
        }
        match names {
            Some(names) => {
                let mut names: Vec<_> = names.iter().map(ToString::to_string).collect();
                names.sort();
                Ok(names)
            }
            None => Err(vec![format!("missing module {module}")]),
        }
    }

    /// Dependencies requested by this immutable export calculation.
    pub fn dependencies(&self) -> BTreeSet<(String, Option<String>)> {
        self.dependencies.borrow().clone()
    }

    fn errors(&self, key: CalculationKey) -> Vec<String> {
        self.incomplete
            .borrow()
            .get(&key)
            .map_or_else(Vec::new, |errors| errors.iter().cloned().collect())
    }

    fn issue(&self, key: CalculationKey, error: String) {
        let mut issues = self.incomplete.borrow_mut();
        issues.entry(key).or_default().insert(error.clone());
        for caller in self.active.borrow().iter() {
            issues.entry(*caller).or_default().insert(error.clone());
        }
    }

    /// Cache failure evidence alongside the calculation it belongs to. Cached partial
    /// Pyrefly tables never lose their incompleteness, and unrelated queries remain usable.
    fn calculate<T>(&self, key: CalculationKey, run: impl FnOnce() -> T) -> Option<T> {
        let errors = self.errors(key);
        if !errors.is_empty() {
            for error in errors {
                self.issue(key, error);
            }
            return None;
        }
        if self.active.borrow().contains(&key) {
            self.issue(key, format!("cyclic export calculation at {}", key.0));
            return None;
        }
        self.active.borrow_mut().push(key);
        let result = run();
        assert_eq!(
            self.active.borrow_mut().pop(),
            Some(key),
            "nested calculations unwind in order"
        );
        let errors = self.errors(key);
        if errors.is_empty() {
            Some(result)
        } else {
            for error in errors {
                self.issue(key, error);
            }
            None
        }
    }

    fn table(&self, module: ModuleName) -> Option<ExportTable> {
        let exports = self.modules.get(&module)?;
        self.calculate((module, Calculation::Exports), || exports.exports(self))
    }

    fn record(&self, module: ModuleName, name: Option<&Name>) {
        self.dependencies
            .borrow_mut()
            .insert((module.to_string(), name.map(ToString::to_string)));
    }
}

impl ExportProvider for ExportIndex {
    fn resolve(&self, module: ModuleName, name: &Name) -> ExportTarget {
        match ExportIndex::resolve(self, &module.to_string(), name.as_str()) {
            Resolution::Found { module, name } => {
                ExportTarget::Definition(ModuleName::from_str(&module), Name::new(&name))
            }
            Resolution::Module(module) => ExportTarget::Module(ModuleName::from_str(&module)),
            Resolution::Missing | Resolution::Unknown(_) => ExportTarget::Unknown,
        }
    }
}

impl LookupExport for ExportIndex {
    fn export_exists(&self, module: ModuleName, name: &Name) -> bool {
        self.record(module, Some(name));
        self.table(module)
            .is_some_and(|table| table.contains_key(name))
    }

    fn module_exists(&self, module: ModuleName) -> FindingOrError<()> {
        if self.modules.contains_key(&module) {
            FindingOrError::new_finding(())
        } else {
            // This immutable adapter has no search path. A missing universe entry
            // is not an ignored runtime import; callers receive Missing above.
            FindingOrError::Error(FindError::Ignored)
        }
    }

    fn get_wildcard(&self, module: ModuleName) -> Option<Arc<SmallSet<Name>>> {
        self.record(module, None);
        let key = (module, Calculation::Wildcard);
        let Some(exports) = self.modules.get(&module) else {
            self.issue(key, format!("missing wildcard module {module}"));
            return None;
        };
        if exports.unresolvable_dunder_all_range().is_some() {
            self.issue(key, format!("dynamic __all__ in {module}"));
            return None;
        }
        self.calculate(key, || exports.wildcard(self))
    }

    fn get_every_export_untracked(&self, module: ModuleName) -> Option<SmallSet<Name>> {
        self.table(module)
            .map(|table| table.keys().cloned().collect())
    }

    fn is_submodule_imported_implicitly(&self, module: ModuleName, name: &Name) -> bool {
        self.record(module, Some(name));
        self.modules
            .get(&module)
            .is_some_and(|exports| exports.is_submodule_imported_implicitly(name))
    }

    fn get_deprecated(&self, module: ModuleName, name: &Name) -> Option<Deprecation> {
        self.record(module, Some(name));
        None
    }

    fn reexport_source(&self, module: ModuleName, name: &Name) -> Option<ModuleName> {
        self.record(module, Some(name));
        let table = self.table(module)?;
        match table.get(name) {
            Some(ExportLocation::OtherModule(module, _)) => Some(*module),
            _ => None,
        }
    }

    fn is_implicit_reexport(&self, module: ModuleName, name: &Name) -> bool {
        self.record(module, Some(name));
        self.modules
            .get(&module)
            .is_some_and(|exports| exports.is_implicit_reexport(name))
    }

    fn is_special_export(&self, module: ModuleName, name: &Name) -> Option<SpecialExport> {
        self.record(module, Some(name));
        None
    }

    fn docstring_range(&self, module: ModuleName, name: &Name) -> Option<TextRange> {
        self.record(module, Some(name));
        let table = self.table(module)?;
        match table.get(name) {
            Some(ExportLocation::ThisModule(export)) => export.docstring_range,
            _ => None,
        }
    }

    fn export_origin(&self, module: ModuleName, name: &Name) -> ExportOrigin {
        let origin = match self.resolve(&module.to_string(), name.as_str()) {
            Resolution::Found { module, name } => (ModuleName::from_str(&module), Name::new(&name)),
            Resolution::Module(_) | Resolution::Missing | Resolution::Unknown(_) => {
                (module, name.clone())
            }
        };
        ExportOrigin {
            origin,
            is_final: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pyrefly_python::sys_info::PythonPlatform;
    use pyrefly_python::sys_info::PythonVersion;

    fn index(sources: &[(&str, &str)]) -> ExportIndex {
        ExportIndex::new(
            sources
                .iter()
                .map(|(module, contents)| Source {
                    module: (*module).to_owned(),
                    path: PathBuf::from(format!("{module}.py")),
                    contents: (*contents).to_owned(),
                })
                .collect(),
            SysInfo::new_without_type_checking(PythonVersion::default(), PythonPlatform::default()),
        )
        .unwrap()
    }

    #[derive(Clone, Debug, PartialEq)]
    enum Provenance {
        Target(ExportTarget),
        Unknown,
    }

    struct ProvenanceDomain<'a>(&'a ExportIndex);

    impl pyrefly_static_analysis::Domain for ProvenanceDomain<'_> {
        type Value = Provenance;
        type State = ();
        type Diagnostic = ();

        fn top(&self) -> Provenance {
            Provenance::Unknown
        }

        fn join(&self, a: &Provenance, b: &Provenance) -> Provenance {
            if a == b {
                a.clone()
            } else {
                Provenance::Unknown
            }
        }

        fn join_state(&self, _: &(), _: &()) {}

        fn transfer(
            &self,
            _: pyrefly_static_analysis::OpId,
            kind: &pyrefly_static_analysis::OpKind,
            cx: &mut pyrefly_static_analysis::Cx<'_, Self>,
        ) {
            if let pyrefly_static_analysis::OpKind::Import {
                module,
                item: pyrefly_static_analysis::ImportItem::Name(name),
                dst,
            } = kind
            {
                cx.set_reg(
                    *dst,
                    Provenance::Target(ExportProvider::resolve(self.0, *module, name)),
                );
            }
        }
    }

    #[test]
    fn export_provenance_uses_shared_name_bookkeeping() {
        let exports = index(&[
            ("original", "def work(): pass"),
            ("facade", "from original import work as run"),
        ]);
        let program = pyrefly_static_analysis::analyze_python(
            "from facade import run as f\nalias = f\nf = 0\n",
            ModuleName::from_str("consumer"),
            false,
        );
        let domain = ProvenanceDomain(&exports);
        let result = pyrefly_static_analysis::Engine::new(&program, &domain).analyze(
            pyrefly_static_analysis::Program::MODULE,
            pyrefly_static_analysis::Entry::new(()),
        );
        assert!(!result.is_incomplete());
        let exit = result.exit.unwrap();
        assert_eq!(
            exit.symbol(&program, program.global("alias").unwrap()),
            Some(&Provenance::Target(ExportTarget::Definition(
                ModuleName::from_str("original"),
                Name::new("work")
            )))
        );
        assert_eq!(
            exit.symbol(&program, program.global("f").unwrap()),
            Some(&Provenance::Unknown)
        );
    }

    #[test]
    fn named_and_wildcard_reexports_use_actual_pyrefly_exports() {
        let exports = index(&[
            ("original", "def work(): pass\n__all__ = ['work']"),
            (
                "alias",
                "from original import work as renamed\n__all__ = ['renamed']",
            ),
            ("facade", "from alias import *"),
        ]);
        assert_eq!(
            exports.resolve("facade", "renamed"),
            Resolution::Found {
                module: "original".into(),
                name: "work".into()
            }
        );
        assert_eq!(exports.wildcard("facade"), Ok(vec!["renamed".into()]));
        assert_eq!(exports.resolve("facade", "missing"), Resolution::Missing);
        assert!(
            exports
                .dependencies()
                .contains(&("original".into(), Some("work".into())))
        );
    }

    #[test]
    fn relative_reexport_uses_package_identity() {
        let sources = vec![
            Source {
                module: "pkg".into(),
                path: PathBuf::from("pkg/__init__.py"),
                contents: "from .worker import work as run\n__all__ = ['run']".into(),
            },
            Source {
                module: "pkg.worker".into(),
                path: PathBuf::from("pkg/worker.py"),
                contents: "def work(): pass".into(),
            },
        ];
        let exports = ExportIndex::new(
            sources,
            SysInfo::new_without_type_checking(PythonVersion::default(), PythonPlatform::default()),
        )
        .unwrap();
        assert_eq!(
            exports.resolve("pkg", "run"),
            Resolution::Found {
                module: "pkg.worker".into(),
                name: "work".into()
            }
        );
    }

    #[test]
    fn explicit_all_filters_wildcard_but_not_named_imports() {
        let exports = index(&[("m", "public = 1\nhidden = 2\n__all__ = ['public']")]);
        assert_eq!(exports.wildcard("m"), Ok(vec!["public".into()]));
        assert!(matches!(
            exports.resolve("m", "hidden"),
            Resolution::Found { .. }
        ));
    }

    #[test]
    fn missing_wildcard_and_invalid_all_are_incomplete() {
        let exports = index(&[("facade", "from missing import *")]);
        assert!(exports.wildcard("facade").is_err());
        let exports = index(&[("m", "__all__ = ['not_defined']")]);
        assert!(matches!(
            exports.resolve("m", "not_defined"),
            Resolution::Unknown(_)
        ));
    }

    #[test]
    fn dynamic_wildcards_do_not_poison_other_queries_or_become_complete() {
        let exports = index(&[
            ("original", "def work(): pass"),
            ("dynamic", "__all__ = compute()\ndef work(): pass"),
            ("bad", "from missing import *\nfrom original import work"),
        ]);
        assert!(exports.wildcard("dynamic").is_err());
        assert!(exports.wildcard("dynamic").is_err());
        assert!(matches!(
            exports.resolve("dynamic", "work"),
            Resolution::Found { .. }
        ));
        assert!(exports.wildcard("bad").is_err());
        assert!(matches!(
            exports.resolve("bad", "work"),
            Resolution::Unknown(_)
        ));
        assert!(matches!(
            exports.resolve("original", "work"),
            Resolution::Found { .. }
        ));
        assert!(matches!(
            exports.resolve("bad", "work"),
            Resolution::Unknown(_)
        ));
        assert!(matches!(
            exports.resolve("original", "work"),
            Resolution::Found { .. }
        ));
    }

    #[test]
    fn module_aliases_keep_value_identity_through_rebinding_and_reexports() {
        let exports = index(&[
            ("original", "def work(): pass"),
            ("facade", "import original as x\ny = x\nx = 0"),
            ("reexport", "from facade import y as z"),
        ]);
        assert_eq!(
            exports.resolve("facade", "y"),
            Resolution::Module("original".into())
        );
        assert_eq!(
            ExportProvider::resolve(&exports, ModuleName::from_str("reexport"), &Name::new("z")),
            ExportTarget::Module(ModuleName::from_str("original"))
        );
        assert!(matches!(
            exports.resolve("facade", "x"),
            Resolution::Found { .. }
        ));
        let exports = index(&[("original", ""), ("facade", "import original as x")]);
        assert_eq!(
            exports.resolve("facade", "x"),
            Resolution::Module("original".into())
        );
        let exports = index(&[
            ("original", ""),
            ("other", ""),
            (
                "facade",
                "if flag:\n    import original as x\nelse:\n    import other as x",
            ),
        ]);
        assert!(matches!(
            exports.resolve("facade", "x"),
            Resolution::Unknown(_)
        ));
    }

    #[test]
    fn submodules_and_dynamic_attributes_are_distinct() {
        let sources = vec![
            Source {
                module: "pkg".into(),
                path: PathBuf::from("pkg/__init__.py"),
                contents: "".into(),
            },
            Source {
                module: "pkg.child".into(),
                path: PathBuf::from("pkg/child.py"),
                contents: "".into(),
            },
        ];
        let exports = ExportIndex::new(
            sources,
            SysInfo::new_without_type_checking(PythonVersion::default(), PythonPlatform::default()),
        )
        .unwrap();
        assert_eq!(
            exports.resolve("pkg", "child"),
            Resolution::Module("pkg.child".into())
        );
        let exports = index(&[("m", "def __getattr__(name): return object()")]);
        assert!(matches!(
            exports.resolve("m", "anything"),
            Resolution::Unknown(_)
        ));
    }

    #[test]
    fn missing_and_cyclic_exports_are_not_success() {
        let exports = index(&[("a", "from b import thing"), ("b", "from a import thing")]);
        assert!(matches!(
            exports.resolve("a", "thing"),
            Resolution::Unknown(_)
        ));
        assert_eq!(exports.resolve("missing", "thing"), Resolution::Missing);
        let exports = index(&[("a", "from b import *"), ("b", "from a import *")]);
        assert!(exports.wildcard("a").is_err());
    }
}
