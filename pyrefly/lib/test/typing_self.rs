/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_use_self,
    r#"
from typing import assert_type
from typing import Self
import typing
from typing import Self as Myself

class C:
    def m(self, x: Self, y: Myself) -> list[typing.Self]:
        return [self, x, y]

assert_type(C().m(C(), C()), list[C])
"#,
);

testcase!(
    test_typing_self_return,
    r#"
from typing import Self, assert_type
class A:
    def f(self) -> Self:
        return self
    @classmethod
    def g(cls) -> type[Self]:
        return cls
class B(A):
    pass
assert_type(B().f(), B)
assert_type(B().g(), type[B])
    "#,
);

testcase!(
    test_typing_self_param,
    r#"
from typing import Self
class A:
    def f(self, x: Self):
        pass
class B(A):
    pass
def f(a: A, b: B):
    b.f(b)  # OK
    b.f(a)  # E:
    "#,
);

testcase!(
    test_typing_self_new_param,
    r#"
from typing import Self
class A:
    def __new__(cls, x: type[Self]):
        return super().__new__(cls)
class B(A):
    pass
B(B)  # OK
B(A)  # E:
    "#,
);

testcase!(
    test_assert_type,
    r#"
from typing import Self, assert_type
class A:
    def f(self: Self) -> Self:
        assert_type(self, Self)
        return self
    "#,
);

testcase!(
    bug = "We are unable to understand `typing.Self` literals",
    test_instance_attr,
    r#"
from typing import Self, assert_type
class A:
    x: Self
    y: int
    def f(self):
        # This is also an error... the assert passes because we get both sides wrong
        # in the same way (both are being treated as the `typing.Self` special form, not
        # as a "bound" Self)
        assert_type(self.x, Self)
        # Here we can see what's really happening
        assert_type(self.x.y, int)  # E: Expr::attr_infer_for_type attribute base undefined for type: Self  # E: assert_type(Any, int)
class B(A):
    pass
assert_type(A().x, A)  # E: assert_type(Self, A)
assert_type(B().x, B)  # E: assert_type(Self, B)
    "#,
);

testcase!(
    bug = "We are unable to understand `typing.Self` literals",
    test_class_attr,
    r#"
from typing import ClassVar, Self, assert_type
class A:
    x: ClassVar[Self]
class B(A):
    pass
assert_type(A.x, A)  # E: assert_type(Self, A)
assert_type(B.x, B)  # E: assert_type(Self, B)
    "#,
);
