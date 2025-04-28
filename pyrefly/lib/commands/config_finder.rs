/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;
use std::sync::Arc;
use std::sync::LazyLock;

use dupe::Dupe;

use crate::config::config::ConfigFile;
use crate::config::finder::ConfigFinder;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::util::arc_id::ArcId;

/// Create a standard `ConfigFinder`. The `configure` function is expected to set any additional options,
/// then call `configure` and `valiate`.
pub fn standard_config_finder(
    configure: Arc<dyn Fn(ConfigFile) -> ConfigFile + Send + Sync>,
) -> ConfigFinder {
    let configure2 = configure.dupe();
    let default = LazyLock::new(move || ArcId::new(configure(ConfigFile::default())));
    let fallback: Box<dyn Fn(ModuleName, &ModulePath) -> ArcId<ConfigFile> + Send + Sync> =
        Box::new(move |_, _| default.dupe());
    let load: Box<dyn Fn(&Path) -> anyhow::Result<ArcId<ConfigFile>> + Send + Sync> =
        Box::new(move |config_path| {
            Ok(ArcId::new(configure2(ConfigFile::from_file(
                config_path,
                true,
            )?)))
        });
    ConfigFinder::new(load, fallback)
}
