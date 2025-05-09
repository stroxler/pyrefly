# Simple CLI tests

## No errors on the empty file

```scrut {output_stream: stderr}
$ echo "" > $TMPDIR/empty.py && $PYREFLY check --python-version 3.13.0 $TMPDIR/empty.py -a
 INFO 0 errors* (glob)
[0]
```

## No errors on our test script

```scrut {output_stream: stderr}
$ $PYREFLY check $TEST_PY -a
 INFO 0 errors* (glob)
[0]
```

## Text output on stdout

```scrut
$ echo "x: str = 42" > $TMPDIR/test.py && $PYREFLY check $TMPDIR/test.py
*/test.py:1:* (glob)
[1]
```

## JSON output on stdout

```scrut
$ echo "x: str = 42" > $TMPDIR/test.py && $PYREFLY check $TMPDIR/test.py --output-format json | $JQ '.[] | length'
1
[0]
```

## Error on a non-existent file

```scrut {output_stream: stderr}
$ $PYREFLY check $TMPDIR/does_not_exist --python-version 3.13.0
No Python files matched pattern `*/does_not_exist` (glob)
[1]
```

## Error on a non-existent search path

```scrut {output_stream: stderr}
$ echo "" > $TMPDIR/empty.py && $PYREFLY check $TMPDIR/empty.py --search-path $TMPDIR/does_not_exist
Invalid --search-path: `*/does_not_exist` does not exist (glob)
[1]
```

## We can typecheck two files with the same name

```scrut
$ echo "x: str = 12" > $TMPDIR/same_name.py && \
> echo "x: str = True" > $TMPDIR/same_name.pyi && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/same_name.py $TMPDIR/same_name.pyi
*/same_name.py*:1:10-* (glob)
*/same_name.py*:1:10-* (glob)
[1]
```

## We don't report from nested files

```scrut
$ echo "x: str = 12" > $TMPDIR/hidden1.py && \
> echo "import hidden1; y: int = hidden1.x" > $TMPDIR/hidden2.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/hidden2.py
*/hidden2.py:1:26-35: `str` is not assignable to `int` [bad-assignment] (glob)
[1]
```

## We can find adjacent modules for unknown paths

```scrut
$ mkdir -p $TMPDIR/inner/foo && mkdir -p $TMPDIR/inner/baz && \
> echo "x: str = 12" > $TMPDIR/inner/foo/bar.py && \
> echo "import foo.bar; y: int = foo.bar.x" > $TMPDIR/inner/baz/quux.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/inner/baz/quux.py
*/inner/baz/quux.py:1:26-35: `str` is not assignable to `int` [bad-assignment] (glob)
[1]
```

## We do report from nested with --check-all

```scrut
$ echo "x: str = 12" > $TMPDIR/shown1.py && \
> echo "import shown1; y: int = shown1.x" > $TMPDIR/shown2.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/shown2.py --check-all
*/shown*.py:1:* (glob)
*/shown*.py:1:* (glob)
[1]
```

## We can do our own globbing

```scrut
$ echo "x: str = 12" > $TMPDIR/glob1.py && \
> echo "x: str = 12" > $TMPDIR/glob2.py && \
> $PYREFLY check --python-version 3.13.0 "$TMPDIR/glob*.py"
*/glob*.py:1:* (glob)
*/glob*.py:1:* (glob)
[1]
```

## We return an error when all files are filtered by project_excludes

```scrut {output_stream: stderr}
$ echo "x: str = 12" > $TMPDIR/excluded.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/excluded.py --project-excludes="$TMPDIR/*"
All found `project_includes` files were filtered by `project_excludes` patterns.
`project_includes`:* (glob)
`project_excludes`:* (glob)
[1]
```

## --search-path takes precedence over default typeshed

```scrut
$ PYREFLY_STDLIB_SEARCH_PATH=$TYPESHED_ROOT/typeshed/stdlib $PYREFLY check --python-version 3.13.0 $TYPESHED_ROOT/typeshed/stdlib/builtins.pyi --search-path $TYPESHED_ROOT/typeshed/stdlib 2>&1 | grep -v "overrides"
 INFO * errors* (glob)
[0]
```

## Src layout

```scrut {output_stream: stderr}
$ mkdir -p $TMPDIR/src_project/src/foo && echo "x: int = 0" > $TMPDIR/src_project/src/foo/bar.py && echo "from foo.bar import x" > $TMPDIR/src_project/src/foo/baz.py && cd $TMPDIR/src_project && $PYREFLY check
 INFO Checking current directory with default configuration
 INFO 0 errors* (glob)
[0]
```

## Relative --project-excludes

```scrut {output_stream: stderr}
$ mkdir $TMPDIR/foo && touch $TMPDIR/foo/bar.py && echo "x: str = 0" > $TMPDIR/foo/problem.py && cd $TMPDIR/foo && $PYREFLY check *.py --project-excludes=problem.py
 INFO 0 errors* (glob)
[0]
```

## We can find a venv interpreter, even when not sourced

```scrut {output_stream: stderr}
$ python3 -m venv $TMPDIR/venv && \
> echo "import third_party.test2" > $TMPDIR/test.py && \
> export site_packages=$($TMPDIR/venv/bin/python -c "import site; print(site.getsitepackages()[0])") && \
> mkdir $site_packages/third_party && \
> echo "x = 1" > $site_packages/third_party/test2.py && \
> touch $TMPDIR/pyrefly.toml && \
> $PYREFLY check $TMPDIR/test.py
 INFO 0 errors* (glob)
[0]
```
