/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;
use dupe::OptionDupedExt;

use crate::state::loader::Loader;
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

pub struct MemoryFilesLookup<'a> {
    base: &'a MemoryFiles,
    overlay: &'a MemoryFilesOverlay,
    loader: &'a LoaderId,
}

impl<'a> MemoryFilesLookup<'a> {
    pub fn new(
        base: &'a MemoryFiles,
        overlay: &'a MemoryFilesOverlay,
        loader: &'a LoaderId,
    ) -> Self {
        Self {
            base,
            overlay,
            loader,
        }
    }

    pub fn get(&self, path: &Path) -> Option<Arc<String>> {
        if let Some(res) = self.loader.load_from_memory(path) {
            return Some(res);
        }
        let key = (self.loader.dupe(), path.to_path_buf());
        match self.overlay.0.get(&key) {
            Some(contents) => contents.dupe(), // Might be a None if deleted
            None => self.base.0.get(&key).duped(),
        }
    }
}
