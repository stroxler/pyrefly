/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::pydantic_testcase;

pydantic_testcase!(
    test_pyrefly_strict,
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
    x: int = Field(strict=False)  # this is the default
    y: int = Field(strict=True)
Model(x='0', y=1) 
Model(x='0', y='1') # E: Argument `Literal['1']` is not assignable to parameter `y` with type `int` in function `Model.__init__` 
"#,
);

// Note: mypy does not support strict=false. Everything is strict.
pydantic_testcase!(
    test_pyrefly_strict_default,
    r#"
from pydantic import BaseModel, Field
class Model(BaseModel):
    x: int = Field()
    y: int = Field(strict=True)
Model(x='0', y=1) 
Model(x='0', y='1') # E: Argument `Literal['1']` is not assignable to parameter `y` with type `int` in function `Model.__init__` 
"#,
);

pydantic_testcase!(
    test_class_keyword,
    r#"
from pydantic import BaseModel, Field

class Model1(BaseModel, strict=True):
    x: int = Field(0)
    y: int = Field(0, strict=False)
# `x` is strict
Model1(x=0)
Model1(x='0')  # E: `Literal['0']` is not assignable to parameter `x`
# `y` is lax
Model1(y=0)
Model1(y='0')

class Model2(BaseModel, strict=False):
    x: int = Field(0)
    y: int = Field(0, strict=True)
# `x` is lax
Model2(x=0)
Model2(x='0')
# `y` is strict
Model2(y=0)
Model2(y='0')  # E: `Literal['0']` is not assignable to parameter `y`
    "#,
);

pydantic_testcase!(
    test_configdict,
    r#"
from pydantic import BaseModel, ConfigDict
class Model(BaseModel):
    x: int
    model_config = ConfigDict(strict=True)
Model(x=0)
Model(x='0')  # E: `Literal['0']` is not assignable to parameter `x`
    "#,
);

pydantic_testcase!(
    test_multiple_strict_values,
    r#"
from pydantic import BaseModel, ConfigDict

# When `strict` appears in both the class keywords and model_config, the keyword wins
class Model1(BaseModel, strict=True):
    x: int
    model_config = ConfigDict(strict=False)
Model1(x=0)
Model1(x='0')  # E: `Literal['0']` is not assignable to parameter `x`
class Model2(BaseModel, strict=False):
    x: int
    model_config = ConfigDict(strict=True)
Model2(x=0)
Model2(x='0')
    "#,
);

pydantic_testcase!(
    test_inherit,
    r#"
from pydantic import BaseModel
class Model1(BaseModel, strict=True):
    pass
class Model2(Model1):
    x: int
Model2(x=0)
Model2(x='0')  # E: `Literal['0']` is not assignable to parameter `x`
    "#,
);

pydantic_testcase!(
    test_lax_mode_coercion,
    r#"
from pydantic import BaseModel
from typing import Callable, reveal_type
from decimal import Decimal
from datetime import date, datetime

class Model(BaseModel):
    x: int = 0

reveal_type(Model.__init__)  # E: revealed type: (self: Model, *, x: Decimal | bool | bytes | float | int | str = ..., **Unknown) -> None

# int field accepts: int, bool, float, str, bytes, Decimal
Model(x=1)
Model(x=True)
Model(x=1.0)
Model(x='123')
Model(x=b'123')
Model(x=Decimal('123'))


class Model2(BaseModel):
    x: bytes

reveal_type(Model2.__init__)  # E: revealed type: (self: Model2, *, x: bytearray | bytes | str, **Unknown) -> None

class Model3(BaseModel):
    func: Callable[[int], str]

reveal_type(Model3.__init__)  # E: revealed type: (self: Model3, *, func: Any, **Unknown) -> None

class Model4(BaseModel):
    d: date

reveal_type(Model4.__init__)  # E: revealed type: (self: Model4, *, d: Decimal | bytes | date | datetime | float | int | str, **Unknown) -> None

class Model5(BaseModel):
    dt: datetime

reveal_type(Model5.__init__)  # E: revealed type: (self: Model5, *, dt: Decimal | bytes | date | datetime | float | int | str, **Unknown) -> None
    "#,
);

pydantic_testcase!(
    bug = "An error should be raised here",
    test_lax_mode_coercion_literals,
    r#"
from typing import Literal
from pydantic import BaseModel

class Model1(BaseModel):
    status: Literal[1]

m = Model1(status="1")
    "#,
);

pydantic_testcase!(
    test_lax_mode_coercion_container,
    r#"
from typing import List, reveal_type
from collections import deque

from pydantic import BaseModel

class Model(BaseModel):
    x: List[int] = [0, 1]

reveal_type(Model.__init__) # E: revealed type: (self: Model, *, x: list[Decimal | bool | bytes | float | int | str] = ..., **Unknown) -> None

class Model2(BaseModel):
    q: deque[int]

reveal_type(Model2.__init__) # E: revealed type: (self: Model2, *, q: deque[Decimal | bool | bytes | float | int | str] | frozenset[Decimal | bool | bytes | float | int | str] | list[Decimal | bool | bytes | float | int | str] | set[Decimal | bool | bytes | float | int | str] | tuple[Decimal | bool | bytes | float | int | str, ...], **Unknown) -> None

class Model3(BaseModel):
    d: dict[str, int]

reveal_type(Model3.__init__) # E: revealed type: (self: Model3, *, d: Mapping[bytearray | bytes | str, Decimal | bool | bytes | float | int | str] | dict[bytearray | bytes | str, Decimal | bool | bytes | float | int | str], **Unknown) -> None

class Model4(BaseModel):
    f: frozenset[int]

reveal_type(Model4.__init__) # E: revealed type: (self: Model4, *, f: deque[Decimal | bool | bytes | float | int | str] | dict_keys[Decimal | bool | bytes | float | int | str, Decimal | bool | bytes | float | int | str] | dict_values[Decimal | bool | bytes | float | int | str, Decimal | bool | bytes | float | int | str] | frozenset[Decimal | bool | bytes | float | int | str] | list[Decimal | bool | bytes | float | int | str] | set[Decimal | bool | bytes | float | int | str] | tuple[Decimal | bool | bytes | float | int | str, ...], **Unknown) -> None
    "#,
);

pydantic_testcase!(
    test_lax_mode_coercion_union,
    r#"
from typing import List, reveal_type
from decimal import Decimal

from pydantic import BaseModel

class Model(BaseModel):
    y: int | bool

reveal_type(Model.__init__)  # E: revealed type: (self: Model, *, y: Decimal | bool | bytes | float | int | str, **Unknown) -> None
    "#,
);
