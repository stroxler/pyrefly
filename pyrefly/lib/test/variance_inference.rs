/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    bug = "T is only used in covariant positions so it should be inferred as covariant",
    test_covariance_inference,
    r#"
from typing import Sequence, reveal_type
def id[T](x: Sequence[T]) -> Sequence[T]:
    return x
def test(x: Sequence[int] | Sequence[str]):
    reveal_type(id(x))  # E: revealed type: Sequence[int] | int  # E: Argument `Sequence[int] | Sequence[str]` is not assignable to parameter `x` with type `Sequence[int]` in function `id`
"#,
);

testcase!(
    bug = "T is covariant, so the first assignment is ok and the second assignment should be an error",
    test_covariance_inference_class,
    r#"
from typing import Sequence
class ShouldBeCovariant[T](Sequence[T]):
    pass

vco2_1: ShouldBeCovariant[float] = ShouldBeCovariant[int]()  # E:
vco2_2: ShouldBeCovariant[int] = ShouldBeCovariant[float]()  # E:
"#,
);

testcase!(
    bug = "T and U are bivariant so there should be no errors",
    test_bivariant,
    r#"
class A[T]:
    def f(self, x: B[T]) -> B[T]:
        return x

class B[U]:
    def g(self, x: A[U]) -> A[U]:
        return x

a = A[int]()
b = B[int]()

x : A[float] = b.g(a) # E:
"#,
);

testcase!(
    test_invariant_callable,
    r#"
from typing import Callable

class ShouldBeInvariant[T]:

    def f (self, x: Callable[[T], T]):
        return x

square: Callable[[int], int] = lambda x: x ** 2

a: Callable[[int], int] = ShouldBeInvariant[int]().f(square)  
b: Callable[[float], int]= ShouldBeInvariant[float]().f(square)  # E: # E:
"#,
);

testcase!(
    test_invariant_dict,
    r#"
class ShouldBeInvariant[K, V](dict[K, V]):
    pass

vinv3_1: ShouldBeInvariant[float, str] = ShouldBeInvariant[int, str]()  # E:
"#,
);
