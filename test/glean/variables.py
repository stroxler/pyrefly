# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class B:
    name: str = ""


class A:
    name: str = "a"
    b: B = B()


x: int = 1
a: A = A()
b_name = a.b.name
a.name.capitalize()
b_name.capitalize()
