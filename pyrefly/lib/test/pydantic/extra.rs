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

class ModelForbid(BaseModel):
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    test_extra_wrong_type,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelForbid(BaseModel, extra=True): # E: Invalid value for `extra`. Expected one of 'allow', 'ignore', or 'forbid'
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    test_extra_wrong_literal,
    pydantic_env(),
    r#"
from pydantic import BaseModel

class ModelForbid(BaseModel, extra="123"): # E: Invalid value for `extra`. Expected one of 'allow', 'ignore', or 'forbid'
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    bug = "y should be allowed because extra is allowed",
    test_extra_allow_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelAllow(BaseModel):
    ConfigDict(extra="allow")
    x: int 

ModelAllow(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelAllow.__init__` 

"#,
);

testcase!(
    test_extra_forbid_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelForbid(BaseModel):
    ConfigDict(extra="forbid")
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    bug = "should not raise an error for ignore",
    test_extra_ignore_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelForbid(BaseModel):
    ConfigDict(extra="ignore")
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    bug = "should not raise an error for default, which is ignore",
    test_extra_default_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelForbid(BaseModel):
    ConfigDict()
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

// Note, Zeina: For ConfigDict, we extra can take any value and it won't raise an error
// the list of valid keywords is still 'allow', 'forbid' and 'ignore' (as well as None).
// Values like True/False seem legitimate and won't cause an error but won't actually apply to the model
// The errors here are raised directly from the stub definitions
// I think the unexpected keyword error is less relevant since the program already won't typecheck statically
// unless the correct keyword is provided, but we can choose to remove it
testcase!(
    test_extra_wrong_type_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelForbid(BaseModel):
    ConfigDict(extra=False) # E: Argument `Literal[False]` is not assignable to parameter `extra` with type `Literal['allow', 'forbid', 'ignore'] | None`
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);

testcase!(
    test_extra_wrong_literal_config_dict,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict

class ModelForbid(BaseModel):
    ConfigDict(extra="123") # E: Argument `Literal['123']` is not assignable to parameter `extra` with type `Literal['allow', 'forbid', 'ignore'] | None`
    x: int

ModelForbid(x=1, y=2) # E: Unexpected keyword argument `y` in function `ModelForbid.__init__`
"#,
);
