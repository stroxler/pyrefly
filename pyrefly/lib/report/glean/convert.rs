/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::ModModule;
use serde::Serialize;
use serde_json::Value;

use crate::alt::answers::Answers;
use crate::binding::bindings::Bindings;
use crate::module::module_info::ModuleInfo;
use crate::report::glean::schema::*;

fn json(x: impl Serialize) -> Value {
    serde_json::to_value(x).unwrap()
}

fn hash(x: &[u8]) -> String {
    // Glean uses blake3
    blake3::hash(x).to_string()
}

impl Glean {
    #[allow(unused_variables)]
    pub fn new(
        module_info: &ModuleInfo,
        ast: &ModModule,
        bindings: &Bindings,
        answers: &Answers,
    ) -> Self {
        let entries = vec![
            GleanEntry::SchemaId {
                schema_id: PYTHON_SCHEMA_ID.to_owned(),
            },
            GleanEntry::Predicate {
                predicate: "python.Name.4".to_owned(),
                facts: vec![Fact {
                    id: 0,
                    key: json(""),
                    value: None,
                }],
            },
            GleanEntry::Predicate {
                predicate: "python.Module.4".to_owned(),
                facts: vec![Fact {
                    id: 0,
                    key: json(Module {
                        name: Name {
                            id: 0,
                            key: module_info.name().as_str().to_owned(),
                        },
                    }),
                    value: None,
                }],
            },
            GleanEntry::Predicate {
                predicate: "digest.FileDigest.1".to_owned(),
                facts: vec![Fact {
                    id: 0,
                    key: json(FileDigest {
                        file: File {
                            id: 0,
                            key: module_info.path().to_string(),
                        },
                        digest: Digest {
                            hash: hash(module_info.contents().as_bytes()),
                            size: module_info.len() as u64,
                        },
                    }),
                    value: None,
                }],
            },
        ];

        // TODO: Add many more predicates here.

        Glean { entries }
    }
}
