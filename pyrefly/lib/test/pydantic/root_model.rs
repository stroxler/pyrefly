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
    bug = "We should error m2 only because the argument type is inconsistent with int",
    test_root_model_basic,
    pydantic_env(),
    r#"
from pydantic import RootModel
class IntRootModel(RootModel[int]):
   pass
m1 = IntRootModel(123) # E: Expected argument `root` to be passed by name in function `IntRootModel.__init__`
m2 = IntRootModel("abc") # E: Expected argument `root` to be passed by name in function `IntRootModel.__init__`
"#,
);
