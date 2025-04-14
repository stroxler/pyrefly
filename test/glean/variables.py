# (c) Meta Platforms, Inc. and affiliates. See MIT LICENSE file at root.


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
