/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod bundled {

    use std::collections::HashMap;
    use std::fs;
    use std::path::Path;
    use std::sync::Arc;
    use std::sync::LazyLock;

    use anyhow::Context as _;
    use dupe::Dupe;
    use dupe::OptionDupedExt;
    use pyrefly_config::config::ConfigFile;
    use pyrefly_config::error::ErrorDisplayConfig;
    use pyrefly_config::error_kind::ErrorKind;
    use pyrefly_config::error_kind::Severity;
    use pyrefly_python::module_name::ModuleName;
    use pyrefly_python::module_path::ModulePath;
    use pyrefly_util::arc_id::ArcId;
    use pyrefly_util::fs_anyhow;

    use crate::module::typeshed::BundledTypeshed;
    use crate::module::typeshed::stdlib_search_path;

    pub fn set_readonly(path: &Path, value: bool) -> anyhow::Result<()> {
        let mut permissions = fs::metadata(path)?.permissions();
        permissions.set_readonly(value);
        fs::set_permissions(path, permissions)?;
        Ok(())
    }

    pub fn find_bundled_stub_module_path(
        bundled_typeshed: BundledTypeshed,
        module: ModuleName,
    ) -> Option<ModulePath> {
        bundled_typeshed
            .find
            .get(&module)
            .map(|path| ModulePath::bundled_typeshed(path.clone()))
    }

    pub fn load_stubs_from_path(
        bundled_typeshed: BundledTypeshed,
        path: &Path,
    ) -> Option<Arc<String>> {
        bundled_typeshed.load.get(path).duped()
    }

    pub fn get_config_file() -> ArcId<ConfigFile> {
        static CONFIG: LazyLock<ArcId<ConfigFile>> = LazyLock::new(|| {
            let mut config_file = ConfigFile::default();
            config_file.python_environment.site_package_path = Some(Vec::new());
            config_file.search_path_from_file = match stdlib_search_path() {
                Some(path) => vec![path],
                None => Vec::new(),
            };
            config_file.root.errors = Some(ErrorDisplayConfig::new(HashMap::from([
                // The stdlib is full of deliberately incorrect overrides, so ignore them
                (ErrorKind::BadOverride, Severity::Ignore),
                (ErrorKind::BadParamNameOverride, Severity::Ignore),
            ])));
            config_file.root.disable_type_errors_in_ide = Some(true);
            config_file.configure();
            ArcId::new(config_file)
        });
        CONFIG.dupe()
    }

    pub fn write_stub_files(
        bundled_typeshed: BundledTypeshed,
        temp_dir: &Path,
    ) -> anyhow::Result<()> {
        fs_anyhow::create_dir_all(temp_dir)?;

        for (relative_path, contents) in &bundled_typeshed.load {
            let mut file_path = temp_dir.to_owned();
            file_path.push(relative_path);

            if let Some(parent) = file_path.parent() {
                fs_anyhow::create_dir_all(parent)?;
            }

            // Write the file and set it as read-only in a single logical operation
            let _ = set_readonly(&file_path, false); // Might fail (e.g. file doesn't exist)
            fs::write(&file_path, contents.as_bytes())
                .with_context(|| format!("When writing file `{}`", file_path.display()))?;

            // We try and make the files read-only, since editing them in the IDE won't update.
            let _ = set_readonly(&file_path, true); // If this fails, not a big deal
        }

        BundledTypeshed::config()
            .as_ref()
            .write_to_toml_in_directory(temp_dir)
            .with_context(|| {
                format!("Failed to write pyrefly config at {:?}", temp_dir.display())
            })?;
        Ok(())
    }
}
