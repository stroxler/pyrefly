/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

fn django_env() -> TestEnv {
    let path = std::env::var("DJANGO_TEST_PATH").expect("DJANGO_TEST_PATH must be set");
    TestEnv::new_with_site_package_path(&path)
}

testcase!(
    bug = "Add view stubs so that the test can pass",
    test_view,
    django_env(),
    r#"
from django.db import models
from django.views.generic.detail import SingleObjectMixin # E: Could not find import of `django.views.generic.detail`
from typing_extensions import assert_type

class MyModel(models.Model): ...

class MyDetailView(SingleObjectMixin[MyModel]): ...

detail_view = MyDetailView()
assert_type(detail_view.model, type[MyModel]) # E: assert_type(Any, type[MyModel])
assert_type(detail_view.queryset, models.QuerySet[MyModel, MyModel] | None) # E: assert_type(Any, QuerySet[MyModel, MyModel] | None)
assert_type(detail_view.get_context_object_name(MyModel()), str) # E: assert_type(Any, str) failed
assert_type(detail_view.get_context_object_name(1), str | None) # E: assert_type(Any, str | None)
"#,
);

testcase!(
    bug = "Add view stubs so that the test can pass.",
    test_list_view,
    django_env(),
    r#"
from django.db import models
from django.views.generic.list import ListView # E: Could not find import of `django.views.generic.list`
from typing_extensions import assert_type

class MyModel(models.Model): ...

class MyListView(ListView[MyModel]): ...

list_view = MyListView()
assert_type(list_view.model, type[MyModel] | None) # E: assert_type(Any, type[MyModel] | None) 
assert_type(list_view.queryset, models.QuerySet[MyModel, MyModel] | None) # E: assert_type(Any, QuerySet[MyModel, MyModel] | None) 
assert_type(list_view.get_context_object_name(models.QuerySet[MyModel]()), str)  # E: assert_type(Any, str)  
assert_type(list_view.get_context_object_name(MyModel()), str | None) # E: assert_type(Any, str | None)  
assert_type(list_view.get_context_object_name(1), str | None) # E: assert_type(Any, str | None)  
"#,
);
