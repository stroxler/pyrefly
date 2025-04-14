# (c) Meta Platforms, Inc. and affiliates. See MIT LICENSE file at root.


from dataclasses import dataclass


@dataclass(frozen=True)
class A:
    pass


class B(A):
    def method(self) -> None:
        pass
