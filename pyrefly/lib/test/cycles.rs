use crate::test::util::TestEnv;
use crate::testcase;

/*
Leaky loop tests: Some of these are genuinely nondeterministic in cargo (they
normally pass, but might not when run in the full test suite with threading
because the check of `main` can race the check of `leaky_loop`) and so they are
commented out.

They demonstrate that we get nondeterminism from loop recursion, even with
no placeholder types involved. The loop creates a cycle in the definition of
`x`, and depending where we start the cycle we can get different answers.
*/

fn env_leaky_loop() -> TestEnv {
    TestEnv::one(
        "leaky_loop",
        r#"
x = None
def f(_: str | None) -> tuple[str, str]: ...
def g(_: int | None) -> tuple[int, int]: ...
while True:
    y, x = f(x)
    z, x = g(x)
"#,
    )
}

testcase!(
    bug = "If we don't force anything, x will come back as `int`.",
    try_leaky_loop_and_import_x,
    env_leaky_loop(),
    r#"
from typing import assert_type, Any
from leaky_loop import x
assert_type(x, int | None)
"#,
);

testcase!(
    bug = "Forcing `y` first gives us `int` for `x`",
    try_leaky_loop_and_import_y,
    env_leaky_loop(),
    r#"
from typing import assert_type, Any
from leaky_loop import y
assert_type(y, str)
from leaky_loop import x
assert_type(x, int | None)
"#,
);

/*
The variant of this test that exercises an actual race condition can potentially
give nondeterministic output so it is commented for CI stability.

testcase!(
    bug = "Forcing `z` first gives us `Any` for `x`",
    try_leaky_loop_and_import_z,
    env_leaky_loop(),
    r#"
from typing import assert_type, Any
from leaky_loop import z
assert_type(z, int)
from leaky_loop import x
assert_type(x, Any | None)
"#,
);
*/
