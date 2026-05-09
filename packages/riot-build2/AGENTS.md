# riot-build2 Contributor Notes

`riot-build2` is the greenfield incremental build engine rewrite. Keep the
generic graph/worklist executor independent from Riot-specific planner,
analysis, toolchain, fetch, and action services.

The initial execution slice is intentionally concrete: user intent expands to
goals, goals expand to package work, and build-library work dynamically queues
toolchain, source-analysis, module-planning, action-execution, and package
finalization nodes. Keep those Riot-specific services outside `Executor`; the
executor owns work-node scheduling, dependency registration, and event emission.

Tests live under `tests/` and are discovered by the workspace test runner. Do
not add manual `[[bin]]` test entries to `riot.toml`.

Benchmarks live under `bench/` with names ending in `_bench.ml` and are
discovered by `riot bench`; do not add manual benchmark binary entries unless
auto-discovery stops being viable.
