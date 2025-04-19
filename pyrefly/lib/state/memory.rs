/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::path::PathBuf;
use std::sync::Arc;

use crate::state::loader::LoaderId;

#[derive(Debug, Clone, Default)]
pub struct MemoryFiles(HashMap<(LoaderId, PathBuf), Arc<String>>);

#[derive(Debug, Default, Clone)]
pub struct MemoryFilesOverlay(HashMap<(LoaderId, PathBuf), Option<Arc<String>>>);

impl MemoryFiles {
    pub fn apply_overlay(&mut self, overlay: MemoryFilesOverlay) {
        for ((loader_id, path), contents) in overlay.0.into_iter() {
            match contents {
                None => self.0.remove(&(loader_id, path)),
                Some(contents) => self.0.insert((loader_id, path), contents),
            };
        }
    }
}

impl MemoryFilesOverlay {
    pub fn set(&mut self, loader: LoaderId, path: PathBuf, contents: Option<Arc<String>>) {
        self.0.insert((loader, path), contents);
    }
}
