/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;
use std::sync::LazyLock;

use dupe::Dupe;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::config::config::ConfigFile;
use crate::config::finder::ConfigFinder;
use crate::util::arc_id::ArcId;
use crate::util::lock::Mutex;

/// Create a standard `ConfigFinder`. The `configure` function is expected to set any additional options,
/// then call `configure` and `valiate`.
#[allow(clippy::field_reassign_with_default)] // ConfigFile default is dubious
pub fn standard_config_finder(
    configure: Arc<dyn Fn(ConfigFile) -> ConfigFile + Send + Sync>,
) -> ConfigFinder {
    let configure2 = configure.dupe();
    let configure3 = configure.dupe();

    let cache: Mutex<SmallMap<PathBuf, ArcId<ConfigFile>>> = Mutex::new(SmallMap::new());
    let empty = LazyLock::new(move || ArcId::new(configure3(ConfigFile::default())));

    ConfigFinder::new(
        Box::new(move |file| {
            let config = ConfigFile::from_file(file, false)?;
            Ok(ArcId::new(configure(config)))
        }),
        Box::new(move |name, path| match path.root_of(name) {
            Some(path) => match cache.lock().entry(path.clone()) {
                Entry::Occupied(e) => e.get().dupe(),
                Entry::Vacant(e) => {
                    let mut config = ConfigFile::default();
                    config.search_path = vec![path];
                    e.insert(ArcId::new(configure2(config))).dupe()
                }
            },
            None => empty.dupe(),
        }),
    )
}
