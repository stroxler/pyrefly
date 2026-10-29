# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import spmd_types as s
import torch as t


def mlp(x, w1, w2):
    x = s.invariant_to_replicate(x, "tp")
    h = t.matmul(x, w1)
    h = t.relu(h)
    p = t.matmul(h, w2)
    p.stride()
    return s.all_reduce(p, "tp", src=s.P, dst=s.I)
