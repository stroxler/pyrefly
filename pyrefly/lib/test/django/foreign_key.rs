/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::django_testcase;

django_testcase!(
    bug = "infer ForeignKey field type",
    test_foreign_key_basic,
    r#"
from typing import assert_type

from django.db import models

class Reporter(models.Model): ...

class Article(models.Model):
    reporter = models.ForeignKey(Reporter, on_delete=models.CASCADE)

article = Article()
assert_type(article.reporter, Reporter) # E: assert_type(Any, Reporter) failed 
"#,
);
