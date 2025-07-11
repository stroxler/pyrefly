# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from dataclasses import dataclass


@dataclass(frozen=True)
class A:
    pass


class B(A):
    def method(self) -> None:
        pass
