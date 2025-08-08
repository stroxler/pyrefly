# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from abc import ABCMeta
from typing import Any, ClassVar, Dict, Optional, Tuple, Type

from typing_extensions import dataclass_transform

from . import ConfigDict

class PydanticModelField: ...
class PydanticModelPrivateAttr: ...
class NoInitField: ...

@dataclass_transform(
    kw_only_default=True,
    field_specifiers=(PydanticModelField, PydanticModelPrivateAttr, NoInitField),
)
class ModelMetaclass(ABCMeta):
    def __new__(
        mcs: Type["ModelMetaclass"],
        cls_name: str,
        bases: Tuple[Type[Any], ...],
        namespace: Dict[str, Any],
        __pydantic_generic_metadata__: Optional[Any] = None,
        __pydantic_reset_parent_namespace__: bool = True,
        _create_model_module: Optional[str] = None,
        **kwargs: Any,
    ) -> Type[Any]: ...

class BaseModel(metaclass=ModelMetaclass):
    model_config: ClassVar[ConfigDict] = ConfigDict()

    def __init__(self, /, **data: Any) -> None: ...
    def __repr__(self) -> str: ...
    def __str__(self) -> str: ...
    def __getattr__(self, item: str) -> Any: ...
