/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::pydantic_testcase;

pydantic_testcase!(
    bug = "BaseSettings fields can be populated from environment variables but pyrefly doesn't know this",
    test_base_settings_no_defaults,
    r#"
import os

from pydantic_settings import BaseSettings # E: Could not find import of `pydantic_settings`


class AppConfig(BaseSettings):
    database_url: str
    api_key: str
    port: int


# Set environment variables
os.environ["DATABASE_URL"] = "postgres://localhost:5432/mydb"
os.environ["API_KEY"] = "my-secret-api-key"
os.environ["PORT"] = "8080"

# This should work - fields come from environment variables
config = AppConfig()
"#,
);
