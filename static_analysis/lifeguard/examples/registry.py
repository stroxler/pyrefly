# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

REGISTRY = []


def register(handler):
    REGISTRY.append(handler)
    return handler


register(print)
