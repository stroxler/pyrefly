# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools


def cached(func):
    @functools.wraps(func)
    def wrapper(*args):
        return func(*args)

    return wrapper


@cached
def double(x):
    return [x, x]
