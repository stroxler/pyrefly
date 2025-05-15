use crate::testcase;

testcase!(
    test_double_name_match,
    r#"
match 42:
    case x:  # E: name capture `x` makes remaining patterns unreachable
        pass
    case y:
        pass
# Eventually, this should be an uninitialized-local error.
print(y)
    "#,
);
