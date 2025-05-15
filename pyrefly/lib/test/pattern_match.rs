use crate::testcase;

testcase!(
    bug = "The error we produce here is an internal error, we ought to explain the actual issue",
    test_double_name_match,
    r#"
match 42:
    case x:  # E: name capture `x` makes remaining patterns unreachable
        pass
    case y:  # E: Could not find flow binding for `y`
        pass
# If we uncomment this, Pyrefly will panic.
# print(y)
    "#,
);
