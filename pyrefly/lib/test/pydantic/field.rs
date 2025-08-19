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
    bug = "we could support ranges, but this is not for v1",
    test_field_right_type,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
   x: int = Field(gt=0, lt=10)

Model(x=5) 
Model(x=0)  
Model(x=15)
"#,
);

testcase!(
    bug = "gt/lt values should have types that are be coercible to an int in this case. So, we should raise an error",
    test_field_wrong_type,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field

class Model(BaseModel):
    x: int = Field(gt="A", lt="B")

Model(x=5)
"#,
);
