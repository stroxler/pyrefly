# Tests that return errors

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

## We do report from nested with --check-all

```scrut
$ echo "x: str = 12" > $TMPDIR/shown1.py && \
> echo "import shown1; y: int = shown1.x" > $TMPDIR/shown2.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/shown2.py --check-all
*/shown*.py:1:* (glob)
*/shown*.py:1:* (glob)
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
