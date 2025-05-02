/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::LazyLock;

use dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::config::config::ConfigFile;
use crate::config::finder::ConfigFinder;
use crate::module::module_path::ModulePathDetails;
use crate::util::arc_id::ArcId;
use crate::util::lock::Mutex;

/// Create a standard `ConfigFinder`. The `configure` function is expected to set any additional options,
/// then call `configure` and `validate`.
/// The `path` to `configure` is a directory, either to the python file or the config file.
/// In the case we can't find a config by walking up, we create a default config, setting the `search_path`
/// to a sensible default if possible.
#[allow(clippy::field_reassign_with_default)] // ConfigFile default is dubious
pub fn standard_config_finder(
    configure: Arc<dyn Fn(Option<&Path>, ConfigFile) -> ConfigFile + Send + Sync>,
) -> ConfigFinder {
    let configure2 = configure.dupe();
    let configure3 = configure.dupe();

    // A cache where path `p` maps to config file with `search_path = [p]`. If we can find the root.
    let cache_one: Mutex<SmallMap<PathBuf, ArcId<ConfigFile>>> = Mutex::new(SmallMap::new());
    // A cache where path `p` maps to config file with `search_path = [p, p/.., p/../.., ...]`.
    let cache_parents: Mutex<SmallMap<PathBuf, ArcId<ConfigFile>>> = Mutex::new(SmallMap::new());

    let empty = LazyLock::new(move || ArcId::new(configure3(None, ConfigFile::default())));

    ConfigFinder::new(
        Box::new(move |file| {
            let config = ConfigFile::from_file(file, false)?;
            Ok(ArcId::new(configure(file.parent(), config)))
        }),
        // Fall back to using a default config, but let's see if we can make the `search_path` somewhat useful
        // based on a few heuristics.
        Box::new(move |name, path| match path.root_of(name) {
            // We were able to walk up `path` and match each component of `name` to a directory until we ran out.
            // That means the resulting path is likely the root of the 'project', and should therefore be its `search_path`.
            Some(path) => cache_one
                .lock()
                .entry(path.clone())
                .or_insert_with(|| {
                    let mut config = ConfigFile::default();
                    config.search_path = vec![path.clone()];
                    ArcId::new(configure2(path.parent(), config))
                })
                .dupe(),

            // We couldn't walk up and find a possible root of the project, so let's try to create a search
            // path that is still useful for this import by including all of its parents.
            None => {
                let path = match path.details() {
                    ModulePathDetails::FileSystem(x) | ModulePathDetails::Memory(x) => x.parent(),
                    ModulePathDetails::Namespace(x) => Some(x.as_path()),
                    ModulePathDetails::BundledTypeshed(_) => None,
                };
                match path {
                    None => empty.dupe(),
                    Some(path) => cache_parents
                        .lock()
                        .entry(path.to_owned())
                        .or_insert_with(|| {
                            let mut config = configure2(path.parent(), ConfigFile::default());
                            // We deliberately set `site_package_path` rather than `search_path` here,
                            // because otherwise a user with `/sys` on their computer (all of them)
                            // will override `sys.version` in preference to typeshed.
                            let additional_site_package_path =
                                path.ancestors().map(|x| x.to_owned()).collect::<Vec<_>>();
                            config.python_environment.site_package_path = config
                                .python_environment
                                .site_package_path
                                .map_or(Some(additional_site_package_path.clone()), |path| {
                                    Some(
                                        additional_site_package_path
                                            .into_iter()
                                            .chain(path)
                                            .collect(),
                                    )
                                });
                            ArcId::new(config)
                        })
                        .dupe(),
                }
            }
        }),
    )
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::sync::Arc;

    use clap::Parser;

    use crate::commands::check::Args;
    use crate::commands::config_finder::standard_config_finder;
    use crate::config::environment::PythonEnvironment;
    use crate::module::module_name::ModuleName;
    use crate::module::module_path::ModulePath;

    #[test]
    fn test_site_package_path_from_environment() {
        let args = Args::parse_from(Vec::<OsString>::new().iter());
        let config = standard_config_finder(Arc::new(move |_, x| args.override_config(x)))
            .python_file(ModuleName::unknown(), &ModulePath::filesystem("".into()));
        let env = PythonEnvironment::get_default_interpreter_env();
        if let Some(paths) = env.site_package_path {
            for p in paths {
                assert!(config.site_package_path().contains(&p));
            }
        }
    }
}
