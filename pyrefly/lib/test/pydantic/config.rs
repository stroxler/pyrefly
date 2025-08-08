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
    test_config_conditional_extra,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class User(BaseModel):
    name: str
    age: int

u = User(name="Alice", age=30)
print(u)
"#,
);

testcase!(
    test_model_config,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class Model(BaseModel):
    model_config = ConfigDict(frozen=True)
    x: int = 42


m = Model()
m.x = 10 # E: Cannot set field `x`
"#,
);

testcase!(
    bug = "This is the wrong usage of the model. 
    model_config is a class variable. y is not.",
    test_model_config_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class Model(BaseModel):
    y: ConfigDict = ConfigDict(frozen=True)
    model_config = y
    x: int = 42

m = Model() # E:  Missing argument `y` in function `Model.__init__`
m.x = 10 # E:  Cannot set field `x`

"#,
);

testcase!(
    bug = "The field should not be readonly here. we need to use model_config specifically since its a ClassVar.
     Investigate raising an error on y due to missing annotation.",
    test_model_config_y,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class Model(BaseModel):
    y = ConfigDict(frozen=True)
    x: int = 42

m = Model()
m.x = 10 # E: Cannot set field `x`
"#,
);

// Only the last config is considered
testcase!(
    test_model_two_config,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class Model(BaseModel):
    model_config = ConfigDict(frozen=True)
    model_config = ConfigDict(frozen=False)
    x: int = 42

m = Model()
m.x = 10
"#,
);
