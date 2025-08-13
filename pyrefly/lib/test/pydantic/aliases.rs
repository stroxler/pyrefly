/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

fn pydantic_env() -> TestEnv {
    let path = std::env::var("PYDANTIC_TEST_PATH").expect("PYDANTIC_TEST_PATH must be set");
    TestEnv::new_with_site_package_path(&path)
}

testcase!(
    test_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field

class Model(BaseModel):
    x: int = Field(alias="y")


m = Model(y=0)
m.x
m.y # E: Object of class `Model` has no attribute `y`
"#,
);

testcase!(
    bug = "We only should error on Model(z=0) since validation_alias takes precedence. Only m.x should succeed.",
    test_validation_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field

class Model(BaseModel):
    x: int = Field(validation_alias="y", alias="z")

m = Model(y=0) # E: Missing argument `z` in function `Model.__init__` # E: Unexpected keyword argument `y` in function `Model.__init__
m = Model(z=0)

m.x
m.y  # E: Object of class `Model` has no attribute `y`
m.z  # E: Object of class `Model` has no attribute `z`
"#,
);
