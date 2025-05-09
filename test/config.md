# Tests for pyrefly configuration files

## Error on a non-existent search-path/site-package-path

```scrut {output_stream: stderr}
$ echo "" > $TMPDIR/empty.py && echo -e "project_includes = [\"$TMPDIR/empty.py\"]\nsite_package_path = [\"$TMPDIR/abcd\"]\nsearch_path = [\"$TMPDIR/abcd\"]" > $TMPDIR/pyrefly.toml && $PYREFLY check -c $TMPDIR/pyrefly.toml --python-version 3.13.0 && rm $TMPDIR/pyrefly.toml
 WARN Invalid site_package_path in `*/pyrefly.toml`: * does not exist (glob)
 WARN Invalid search_path in `*/pyrefly.toml`: * does not exist (glob)
 INFO Checking project configured at `*/pyrefly.toml` (glob)
 INFO * errors* (glob)
[0]
```

## Dump config

```scrut
$ touch $TMPDIR/foo.py && mkdir $TMPDIR/bar && touch $TMPDIR/bar/baz.py && touch $TMPDIR/bar/qux.py && $PYREFLY dump-config $TMPDIR/foo.py $TMPDIR/bar/*.py
Default configuration
  Covered files:
    */bar/baz.py (glob)
    */bar/qux.py (glob)
  Search path: * (glob)
  Site package path: * (glob)
Default configuration
  Covered files:
    */foo.py (glob)
  Search path: * (glob)
  Site package path: * (glob)
[0]
```

## Specify both files and config

```scrut {output_stream: stderr}
$ echo "x: str = 0" > $TMPDIR/oops.py && echo "errors = { bad-assignment = false }" > $TMPDIR/pyrefly.toml && $PYREFLY check -c $TMPDIR/pyrefly.toml $TMPDIR/oops.py && rm $TMPDIR/pyrefly.toml
 INFO 0 errors* (glob)
[0]
```

## Error in implicit config (project mode)

```scrut {output_stream: stderr}
$ mkdir $TMPDIR/implicit && touch $TMPDIR/implicit/empty.py && echo "oops oops" > $TMPDIR/implicit/pyrefly.toml && cd $TMPDIR/implicit && $PYREFLY check
 INFO Checking project configured at `*/pyrefly.toml` (glob)
ERROR Failed to parse configuration* (glob)
* (glob*)
[1]
```

## Error in implicit config (file mode)

<!-- Reusing implicit dir with bad pyrefly.toml set up in "Error in implicit config (project mode)" -->

```scrut {output_stream: stderr}
$ $PYREFLY check $TMPDIR/implicit/empty.py
ERROR Failed to parse configuration* (glob)
* (glob*)
[1]
```
