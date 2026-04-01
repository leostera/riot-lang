# tusk-pm AGENTS

`tusk-pm` owns dependency solving, registry/index/archive/src cache interaction, lock refresh/unlock behavior, and projecting resolved package graphs back into `tusk-model`.

## Rules

1. Keep the package manager logic here, not in `tusk-model`.
2. `tusk-model` remains the source of truth for shared data types like `Package`, `Lockfile`, and PM events.
3. Do not rewrite downloaded manifests into path manifests.
4. Bubble errors up instead of hiding them behind fallback behavior.
5. Prefer small slices with tests; phase 1 may be naive operationally, but it should stay structurally honest.
6. Keep the reusable publish command surface at the top level as `Tusk_pm.publish` with `publish_request` / `publish_event` / `publish_error`, even if implementation details live in submodules.

## Validate

`timeout 30 tusk build tusk-pm`
`timeout 30 tusk test -p tusk-pm`
