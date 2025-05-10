use crate::config::base::UntypedDefBehavior;
use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    test_function_check_and_inference_with_mode_infer_return_type,
    TestEnv::new_with_untyped_def_behavior(UntypedDefBehavior::CheckAndInferReturnType),
    r#"
from typing import assert_type, Any, Callable, Coroutine, Generator, AsyncGenerator

x: int = ...  # E:

def f():
    oops: int = "oops"  # E:
    return x
assert_type(f, Callable[[], int])

async def async_f():
    oops: int = "oops"  # E:
    return x
assert_type(async_f, Callable[[], Coroutine[Any, Any, int]])

def gen():
    oops: int = "oops"  # E:
    yield x
assert_type(gen, Callable[[], Generator[int, Any, None]])

def gen_w_return():
    oops: int = "oops"  # E:
    yield x
    return x
assert_type(gen_w_return, Callable[[], Generator[int, Any, int]])

async def async_gen():
    oops: int = "oops"  # E:
    yield x
assert_type(async_gen, Callable[[], AsyncGenerator[int, Any]])
"#,
);

// TODO(stroxler): Should we do best-effort inference for awaitable and generator
// return types here? At the moment we infer `Any` without any analysis of async or yield.
testcase!(
    test_function_check_and_inference_with_mode_infer_return_any,
    TestEnv::new_with_untyped_def_behavior(UntypedDefBehavior::CheckAndInferReturnAny),
    r#"
from typing import assert_type, Any, Callable, Coroutine, Generator, AsyncGenerator

x: int = ...  # E:

def f():
    oops: int = "oops"  # E:
    return x
assert_type(f, Callable[[], Any])

async def async_f():
    oops: int = "oops"  # E:
    return x
assert_type(async_f, Callable[[], Any])

def gen():
    oops: int = "oops"  # E:
    yield x
assert_type(gen, Callable[[], Any])

def gen_w_return():
    oops: int = "oops"  # E:
    yield x
    return x
assert_type(gen_w_return, Callable[[], Any])

async def async_gen():
    oops: int = "oops"  # E:
    yield x
assert_type(async_gen, Callable[[], Any])
"#,
);

testcase!(
    bug = "We do not yet implement Skip behavior for untyped function defs",
    test_function_check_and_inference_with_mode_skip,
    TestEnv::new_with_untyped_def_behavior(UntypedDefBehavior::Skip),
    r#"
from typing import assert_type, Any, Callable, Coroutine, Generator, AsyncGenerator

x: int = ...  # E:

def f():
    oops: int = "oops"  # E:
    return x
assert_type(f, Callable[[], Any])

async def async_f():
    oops: int = "oops"  # E:
    return x
assert_type(async_f, Callable[[], Any])

def gen():
    oops: int = "oops"  # E:
    yield x
assert_type(gen, Callable[[], Any])

def gen_w_return():
    oops: int = "oops"  # E:
    yield x
    return x
assert_type(gen_w_return, Callable[[], Any])

async def async_gen():
    oops: int = "oops"  # E:
    yield x
assert_type(async_gen, Callable[[], Any])
"#,
);
