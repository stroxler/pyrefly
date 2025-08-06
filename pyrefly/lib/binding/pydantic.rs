/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Pydantic metadata that we will later extend to include more fields
// This is different than the PydanticMetadata that goes into the class metadata itself.
// TODO Zeina: look into if we want to store the expr itself or the boolean. Right now,
// this maps 1:1 to PydanticMetadata structure we encounter in the answers phase,
// but this will likely change as we add more fields.
#[derive(Debug, Clone, Default)]
pub struct PydanticMetadataBinding {
    #[allow(dead_code)]
    pub frozen: Option<bool>,
}
