# (c) Meta Platforms, Inc. and affiliates. See MIT LICENSE file at root.


def decorator(func, *args, **kargs):
    """
    Docstring
    """
    return func


@decorator
async def b(x: int, /, y, *, z) -> None | str:
    class C:
        pass
