# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import jax
import jax.numpy as jnp
import numpy as np
from jax.typing import ArrayLike, DTypeLike
from shape_extensions import assert_shape, IntTuple


def asarray(x: ArrayLike) -> jax.Array:
    return jnp.asarray(x)


def rejects_sequence(x: list[int]) -> None:
    asarray(x)  # E: not assignable to parameter `x`


def accepts_numpy_bool(x: np.bool_) -> None:
    asarray(x)


def accepts_dtype(dtype: DTypeLike) -> None:
    pass


def test_array_like() -> None:
    assert_shape(asarray(jnp.ones((2, 3))).shape, IntTuple, runtime=(2, 3))
    assert_shape(asarray(np.ones((4,))).shape, IntTuple, runtime=(4,))
    assert_shape(asarray(1).shape, IntTuple, runtime=())
    assert_shape(asarray(np.int32(1)).shape, IntTuple, runtime=())


def test_dtype_like() -> None:
    accepts_dtype("float32")
    accepts_dtype(jnp.float32)
    accepts_dtype(np.dtype("float32"))
    assert_shape(jnp.ones((2,), dtype=jnp.float32).shape, (2,))
