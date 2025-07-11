# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def decorator(func, *args, **kargs):
    """
    Docstring
    """
    return func


@decorator
async def b(x: int, /, y, *, z) -> None | str:
    class C:
        pass
