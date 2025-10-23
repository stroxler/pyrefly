/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::django_testcase;

django_testcase!(
    bug = "infer pk and id field types",
    test_auto_generated_id_field,
    r#"
from typing import assert_type

from django.db import models

class Reporter(models.Model):
    name = models.CharField(max_length=100)

reporter = Reporter()
assert_type(reporter.id, int)
assert_type(reporter.pk, int) # E: assert_type(Any, int) failed 
"#,
);

django_testcase!(
    test_existing_field,
    r#"
from typing import assert_type

from django.db import models

class Reporter(models.Model):
    id : str = "id"

reporter = Reporter()
assert_type(reporter.id, str)
"#,
);
