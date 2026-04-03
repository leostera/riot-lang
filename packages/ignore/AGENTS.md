# ignore AGENTS

`ignore` owns ignore-aware recursive file walking layered above `std`.

## Rules

1. Keep raw filesystem walking in `Std.Fs.Walker`; `ignore` should own precedence, matching, and pruning decisions.
2. Keep gitignore-style parsing explicit and local. Avoid leaking half-finished syntax trees through the public API.
3. Keep pruning pre-descent. If a directory is ignored, skip its subtree before scanning children.
4. Keep higher-level ignore sources configurable through filenames and override globs instead of hard-coding one tool's policy.
5. Keep the hot path cheap. Reuse compiled glob matchers and avoid rebuilding `Path.t` values or globsets more often than necessary.
6. `Ignore.Walker` is parallel by default. Default concurrency should track `Std.System.available_parallelism`, and callers that need deterministic sibling ordering must opt back down to `~concurrency:1`.
7. Keep parallel traversal semantics simple: callback order may be nondeterministic when concurrency is above one, but pruning decisions must still happen before descending into child directories.

## Validate

`timeout 30 ./riot run riot -- build ignore`
