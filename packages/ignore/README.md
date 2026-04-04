# ignore

Ignore-aware recursive filesystem walking.

`ignore` is the package to use when you need to walk a tree the way developers
expect tools to walk a tree: respecting ignore files, pruning ignored
subdirectories early, and keeping the traversal predictable.

## Install

```sh
riot add ignore
```

## Why this package exists

Tooling code almost always needs a filesystem walker, but a naive recursive
walk is wrong for real projects. `ignore` bakes in the parts that matter:

- gitignore-style matching and precedence;
- subtree pruning so ignored directories are not traversed unnecessarily;
- a reusable surface for build tools, linters, and scanners.

## Start here

- `src/Ignore.mli` is the public entrypoint.
- `examples/ignore_find.ml` is the best quick example.
- `tests/ignore_tests.ml` captures the precedence and pruning behavior.

## Related packages

- `riot-fix`, `riot-fmt`, and other tooling packages rely on this kind of
  traversal behavior.
