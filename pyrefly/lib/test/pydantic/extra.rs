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
    bug = "y should be allowed because extra is allowed",
    test_extra_allow,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelAllow(BaseModel, extra="allow"):
    x: int 

ModelAllow(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelAllow.__init__`

"#,
);

testcase!(
    test_extra_forbid,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelForbid(BaseModel, extra="forbid"):
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    bug = "should not raise an error for ignore",
    test_extra_ignore,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelForbid(BaseModel, extra="ignore"):
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    bug = "should not raise an error for default, which is ignore",
    test_extra_default,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelForbid(BaseModel, extra="ignore"):
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);
