#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dotslash

dotslash.export_fbcode_build(
    target="fbcode//pyrefly/pyrefly:fbpyrefly",
    oncall="pyre",
)
