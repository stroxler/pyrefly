/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::django_testcase;

django_testcase!(
    bug = "null=True not detected in django TextField",
    test_textfield_nullable,
    r#"
from django.db import models

class Group(models.Model):
    name = models.TextField()

class Customer(models.Model):
    name = models.TextField(null=True)

def test():
    c = Customer.objects.create()
    c.name = None # E: `None` is not assignable to attribute `name` with type `str`
"#,
);
