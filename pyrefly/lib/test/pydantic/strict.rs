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
    bug = "strict is false so we should not raise an error on those fields",
    test_pyrefly_strict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
    x: int = Field(strict=False)  # this is the default
    y: int = Field(strict=True)
Model(x='0', y=1)  # E: Argument `Literal['0']` is not assignable to parameter `x` with type `int` in function `Model.__init__ 
Model(x='0', y='1') # E: Argument `Literal['0']` is not assignable to parameter `x` with type `int` in function `Model.__init__ # E: Argument `Literal['1']` is not assignable to parameter `y` with type `int` in function `Model.__init__`
"#,
);
