# tusk-deps AGENTS

`tusk-deps` owns dependency solving, registry/index/archive/src cache interaction, lock refresh/unlock behavior, and projecting resolved package graphs back into `tusk-model`.

## Rules

1. Keep the package manager logic here, not in `tusk-model`.
2. `tusk-model` remains the source of truth for shared data types like `Package`, `Lockfile`, and PM events.
3. Do not rewrite downloaded manifests into path manifests.
4. Bubble errors up instead of hiding them behind fallback behavior.
5. Prefer small slices with tests; phase 1 may be naive operationally, but it should stay structurally honest.
6. Keep publish orchestration out of this package. `tusk-deps` should expose low-level `Publisher` primitives; the command-level `fmt -> fix -> build -> metadata -> artifact -> upload` flow belongs in `tusk-publish`.
7. Keep low-level publish planning honest: validate metadata and artifact inputs here, but do not take a dependency on `tusk-build`.

## Validate

`timeout 30 tusk build tusk-deps`
`timeout 30 tusk test -p tusk-deps`
