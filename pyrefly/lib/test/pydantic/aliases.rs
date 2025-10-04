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
    test_validation_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field

class Model(BaseModel):
    x: int = Field(validation_alias="y", alias="z")

m = Model(y=0)
m = Model(z=0) # E: Missing argument `y` in function `Model.__init__`

m.x
m.y  # E: Object of class `Model` has no attribute `y`
m.z  # E: Object of class `Model` has no attribute `z`
"#,
);

testcase!(
    test_validation_by_alias_and_name,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel, validate_by_name=True, validate_by_alias=True):
    x: int = Field(alias='y')
Model(x=0) 
Model(y=0)
"#,
);

testcase!(
    test_validation_by_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel, validate_by_alias=True):
    x: int = Field(alias='y')
Model(x=0) # E: Missing argument `y` in function `Model.__init__` 
Model(y=0)
"#,
);

testcase!(
    test_validation_by_name,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel, validate_by_name=True):
    x: int = Field(alias='y')
Model(x=0)
Model(y=0)
"#,
);

testcase!(
    test_validation_by_name_only,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel, validate_by_name=True, validate_by_alias=False):
    x: int = Field(alias='y')
Model(x=0)
Model(y=0) # E: Missing argument `x` in function `Model.__init__` 
"#,
);

testcase!(
    bug = "when both validation keys are true, aliases should be required. mypy doesn't support the right behavior here either. This requires generating overloads. We might shelve this for v2",
    test_validation_defaults,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel, validate_by_name=True, validate_by_alias=True):
    x: int = Field(alias='y')
Model()
"#,
);

testcase!(
    test_configdict_validate_by_name,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field, ConfigDict
class Model(BaseModel):
    x: str = Field(..., alias="y")
    model_config = ConfigDict(validate_by_name=True)
Model(y="123")
Model(x="123")  
    "#,
);

testcase!(
    test_configdict_validate_by_alias,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field, ConfigDict
class Model(BaseModel):
    x: str = Field(..., alias="y")
    model_config = ConfigDict(validate_by_name=True, validate_by_alias=False)
Model(y="123") # E: Missing argument `x` in function `Model.__init__`
Model(x="123")   
    "#,
);

testcase!(
    test_configdict_validate_by_alias_optional,
    pydantic_env(),
    r#"
from pydantic import BaseModel, Field, ConfigDict
class Example(BaseModel):
    id: str
    some_attribute: str = Field("", alias="someAttribute")
    optional_attribute: str | None = Field(None, alias="optionalAttribute")

    model_config = ConfigDict(validate_by_name=True, validate_by_alias=True)

x1 = Example(id="1", some_attribute="value")
x2 = Example(id="1", someAttribute="value")  
x3 = Example(id="1", someAttribute123="value")  
    "#,
);

testcase!(
    bug = "For some reason, when we actually define the field, we don't properly propagate the validation info though inhertance",
    test_validation_inheritance,
    pydantic_env(),
    r#"
from pydantic import BaseModel, ConfigDict, Field

class Model(BaseModel):
    x: str = Field(..., alias="y")
    model_config = ConfigDict(validate_by_name=True, validate_by_alias=False)

class Model2(Model): ...

Model2(y="123") # E: Missing argument `x` in function `Model2.__init__`
Model2(x="123")

class Model3(Model2):
    x: str = Field(..., alias="y")

Model3(y="123")
Model3(x="123") # E: Missing argument `y` in function `Model3.__init__`
    "#,
);
