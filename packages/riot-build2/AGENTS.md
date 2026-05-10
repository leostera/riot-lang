# riot-build2 Contributor Notes

`riot-build2` is the greenfield incremental build engine rewrite. Keep the
generic graph/worklist executor independent from Riot-specific planner,
analysis, toolchain, fetch, and action services.

The execution slice is intentionally concrete: selector-shaped user intent
expands to package-specific goals, and `plan_dependencies` grows a
rule-shaped work graph. `Goal.BuildPackage` waits on `PackageArtifact`;
`PackageArtifact` waits on the package archive rule when there is no package
cache hit; `OCamlArchive` fans out to `OCamlInterface`, `OCamlImplementation`,
`OCamlGenerated`, `CObject`, `ModuleDependencies`, and toolchain readiness as
needed. Keep Riot-specific services outside `Executor`; the executor owns
work-node scheduling, dependency registration, readiness checks, and event
emission.

Tests live under `tests/` and are discovered by the workspace test runner. Do
not add manual `[[bin]]` test entries to `riot.toml`.

Benchmarks live under `bench/` with names ending in `_bench.ml` and are
discovered by `riot bench`; do not add manual benchmark binary entries unless
auto-discovery stops being viable.
