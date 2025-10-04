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
    test_pyrefly_strict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
    x: int = Field(strict=False)  # this is the default
    y: int = Field(strict=True)
Model(x='0', y=1) 
Model(x='0', y='1') # E: Argument `Literal['1']` is not assignable to parameter `y` with type `int` in function `Model.__init__` 
"#,
);

// Note: mypy does not support strict=false. Everything is strict.
testcase!(
    test_pyrefly_strict_default,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
    x: int = Field()
    y: int = Field(strict=True)
Model(x='0', y=1) 
Model(x='0', y='1') # E: Argument `Literal['1']` is not assignable to parameter `y` with type `int` in function `Model.__init__` 
"#,
);

testcase!(
    test_class_keyword,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field

class Model1(BaseModel, strict=True):
    x: int = Field(0)
    y: int = Field(0, strict=False)
# `x` is strict
Model1(x=0)
Model1(x='0')  # E: `Literal['0']` is not assignable to parameter `x`
# `y` is lax
Model1(y=0)
Model1(y='0')

class Model2(BaseModel, strict=False):
    x: int = Field(0)
    y: int = Field(0, strict=True)
# `x` is lax
Model2(x=0)
Model2(x='0')
# `y` is strict
Model2(y=0)
Model2(y='0')  # E: `Literal['0']` is not assignable to parameter `y`
    "#,
);
