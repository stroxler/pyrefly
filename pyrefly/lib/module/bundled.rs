/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub mod bundled {
    use pyrefly_python::module_name::ModuleName;
    use pyrefly_python::module_path::ModulePath;

    use crate::module::typeshed::BundledTypeshed;

    pub fn find_bundled_stub_module_path(
        bundled_typeshed: BundledTypeshed,
        module: ModuleName,
    ) -> Option<ModulePath> {
        bundled_typeshed
            .find
            .get(&module)
            .map(|path| ModulePath::bundled_typeshed(path.clone()))
    }
}
