# ignore AGENTS

`ignore` owns ignore-aware recursive file walking layered above `std`.

## Rules

1. Keep raw filesystem walking in `Std.Fs.Walker`; `ignore` should own precedence, matching, and pruning decisions.
2. Keep gitignore-style parsing explicit and local, with public APIs exposing stable parsed patterns.
3. Keep pruning pre-descent. If a directory is ignored, skip its subtree before scanning children.
4. Keep higher-level ignore sources configurable through filenames, caller-supplied `ignore_patterns`, and override globs.
5. Keep the hot path cheap by reusing compiled glob matchers and constructing `Path.t` values only when needed.
6. `Ignore.Walker` is parallel by default. Default concurrency should track `Std.System.available_parallelism`, and callers that need deterministic sibling ordering must opt back down to `~concurrency:1`.
7. Keep `ignore` parallelism actor-native by coordinating directory tasks through `Std.WorkerPool.DynamicWorkerPool`.
8. Keep parallel traversal semantics simple: callback order may be nondeterministic when concurrency is above one, pruning decisions must still happen before descending into child directories, and the callback contract is explicitly thread-safe-by-construction from the caller side.
9. Keep `Ignore.Walker.to_list` as a convenience over the configured traversal, not as a separate sequential mode. If a walker is configured for parallel traversal, `to_list` should still traverse in parallel and collect safely.
10. When `ignore` needs shared coordination inside actor-driven traversal, use public `Std.Sync` actor primitives.
