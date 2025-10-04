/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pyrefly_derive::TypeEq;
use ruff_python_ast::Expr;
use ruff_python_ast::Keyword;
use ruff_python_ast::name::Name;
use starlark_map::Hashed;

use crate::binding::bindings::BindingsBuilder;
use crate::export::special::SpecialExport;

// special pydantic constants
pub const FROZEN_DEFAULT: bool = false;
pub const VALIDATION_ALIAS: Name = Name::new_static("validation_alias");
pub const VALIDATE_BY_NAME: Name = Name::new_static("validate_by_name");
pub const VALIDATE_BY_ALIAS: Name = Name::new_static("validate_by_alias");
pub const GT: Name = Name::new_static("gt");
pub const LT: Name = Name::new_static("lt");
pub const GE: Name = Name::new_static("ge");
pub const ROOT: Name = Name::new_static("root");
pub const STRICT: Name = Name::new_static("strict");
const FROZEN: Name = Name::new_static("frozen");
const EXTRA: Name = Name::new_static("extra");

/// If a class body contains a `model_config` attribute assigned to a `pydantic.ConfigDict`, the
/// configuration options from the `ConfigDict`. In the answers phase, this will be merged with
/// configuration options from the class keywords to produce a full Pydantic model configuration.
#[derive(Debug, Clone, Default)]
pub struct PydanticConfigDict {
    pub frozen: Option<bool>,
    pub extra: Option<bool>,
    pub strict: Option<bool>,
    pub validation_flags: PydanticValidationFlags,
}

/// Flags that control whether a Pydantic model's fields are populated by their names or their aliases.
/// See https://docs.pydantic.dev/latest/api/config/#pydantic.config.ConfigDict.validate_by_name.
#[derive(Debug, Clone, PartialEq, Eq, TypeEq)]
pub struct PydanticValidationFlags {
    pub validate_by_name: bool,
    pub validate_by_alias: bool,
}

impl Default for PydanticValidationFlags {
    fn default() -> Self {
        Self {
            validate_by_name: false,
            validate_by_alias: true,
        }
    }
}

impl<'a> BindingsBuilder<'a> {
    // The goal of this function is to extract pydantic metadata (https://docs.pydantic.dev/latest/concepts/models/) from expressions.
    // TODO: Consider propagating the entire expression instead of the value
    // in case it is aliased.
    pub fn extract_pydantic_config_dict(
        &self,
        e: &Expr,
        name: &Hashed<Name>,
        pydantic_config_dict: &mut PydanticConfigDict,
    ) {
        if name.as_str() == "model_config"
            && let Some(call) = e.as_call_expr()
            && let Some(special) = self.as_special_export(&call.func)
            && special == SpecialExport::PydanticConfigDict
        {
            for kw in &call.arguments.keywords {
                if let Some(v) = self.extract_bool_keyword(kw, &FROZEN) {
                    pydantic_config_dict.frozen = Some(v);
                }

                if let Some(arg_name) = &kw.arg
                    && arg_name.id == EXTRA
                {
                    let config_dict_extra = kw.value.clone().string_literal_expr();
                    pydantic_config_dict.extra = match config_dict_extra {
                        Some(extra) => {
                            let val = extra.value.to_str();

                            if val == "allow" || val == "ignore" {
                                Some(true)
                            } else if val == "forbid" {
                                Some(false)
                            } else {
                                None
                            }
                        }
                        None => None,
                    };
                }

                if let Some(v) = self.extract_bool_keyword(kw, &STRICT) {
                    pydantic_config_dict.strict = Some(v);
                }

                if let Some(v) = self.extract_bool_keyword(kw, &VALIDATE_BY_NAME) {
                    pydantic_config_dict.validation_flags.validate_by_name = v;
                }
                if let Some(v) = self.extract_bool_keyword(kw, &VALIDATE_BY_ALIAS) {
                    pydantic_config_dict.validation_flags.validate_by_alias = v;
                }
            }
        }
    }

    fn extract_bool_keyword(&self, kw: &Keyword, target_kw: &Name) -> Option<bool> {
        if let Some(arg_name) = &kw.arg
            && arg_name.id == *target_kw
            && let Expr::BooleanLiteral(bl) = &kw.value
        {
            Some(bl.value)
        } else {
            None
        }
    }
}
