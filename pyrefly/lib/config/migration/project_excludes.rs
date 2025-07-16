/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use configparser::ini::Ini;
use pyrefly_util::globs::Globs;

use crate::config::config::ConfigFile;
use crate::config::migration::config_option_migrater::ConfigOptionMigrater;
use crate::config::migration::mypy::regex_converter;

/// Configuration option for project excludes
pub struct ProjectExcludes;

impl ConfigOptionMigrater for ProjectExcludes {
    fn migrate_from_mypy(
        &self,
        mypy_cfg: &Ini,
        pyrefly_cfg: &mut ConfigFile,
    ) -> anyhow::Result<()> {
        // https://mypy.readthedocs.io/en/latest/config_file.html#import-discovery
        // excludes can only be set in the top level `[mypy]` global section
        let exclude = mypy_cfg.get("mypy", "exclude");

        if exclude.is_none() {
            return Err(anyhow::anyhow!("No exclude found in mypy config"));
        }

        let exclude_regex = exclude.unwrap();
        let patterns = regex_converter::convert(&exclude_regex)?;

        if patterns.is_empty() {
            return Err(anyhow::anyhow!("No valid patterns found in exclude regex"));
        }

        pyrefly_cfg.project_excludes = Globs::new(patterns);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_migrate_from_mypy() {
        let mut mypy_cfg = Ini::new();
        mypy_cfg.set(
            "mypy",
            "exclude",
            Some("src/include/|other_src/include/".to_owned()),
        );

        let mut pyrefly_cfg = ConfigFile::default();

        let project_excludes = ProjectExcludes;
        let _ = project_excludes.migrate_from_mypy(&mypy_cfg, &mut pyrefly_cfg);

        let expected_excludes = Globs::new(vec![
            "**/src/include/".to_owned(),
            "**/other_src/include/".to_owned(),
        ]);
        assert_eq!(pyrefly_cfg.project_excludes, expected_excludes);
    }

    #[test]
    fn test_migrate_from_mypy_empty() {
        let mypy_cfg = Ini::new();

        let mut pyrefly_cfg = ConfigFile::default();
        let default_excludes = pyrefly_cfg.project_excludes.clone();

        let project_excludes = ProjectExcludes;
        let _ = project_excludes.migrate_from_mypy(&mypy_cfg, &mut pyrefly_cfg);

        assert_eq!(pyrefly_cfg.project_excludes, default_excludes);
    }
}
