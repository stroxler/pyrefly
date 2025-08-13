/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Expr;
use ruff_python_ast::name::Name;
use starlark_map::Hashed;

use crate::binding::bindings::BindingsBuilder;
use crate::export::special::SpecialExport;

// Pydantic metadata that we will later extend to include more fields
// This is different than the PydanticMetadata that goes into the class metadata itself.
// TODO Zeina: look into if we want to store the expr itself or the boolean. Right now,
// this maps 1:1 to PydanticMetadata structure we encounter in the answers phase,
// but this will likely change as we add more fields.
#[derive(Debug, Clone, Default)]
pub struct PydanticMetadataBinding {
    pub frozen: bool,
}

impl<'a> BindingsBuilder<'a> {
    // The goal of this function is to extract pydantic metadata (https://docs.pydantic.dev/latest/concepts/models/) from expressions.
    // TODO: Consider propagating the entire expression instead of the value
    // in case it is aliased.
    pub fn extract_frozen_pydantic_metadata(&self, e: &Expr, name: Hashed<&Name>) -> Option<bool> {
        if name.as_str() == "model_config"
            && let Some(call) = e.as_call_expr()
            && let Some(special) = self.as_special_export(&call.func)
            && special == SpecialExport::PydanticConfigDict
        {
            let mut frozen = false;
            for kw in &call.arguments.keywords {
                if let Some(arg_name) = &kw.arg
                    && arg_name.id.as_str() == "frozen"
                    && let Expr::BooleanLiteral(bl) = &kw.value
                {
                    frozen = bl.value;
                    break;
                }
            }
            return Some(frozen);
        }
        None
    }

    // TODO Zeina: We should expect to extend this beyond the frozen data.
    pub fn make_pydantic_metadata(&self, frozen: bool) -> PydanticMetadataBinding {
        PydanticMetadataBinding { frozen }
    }
}
