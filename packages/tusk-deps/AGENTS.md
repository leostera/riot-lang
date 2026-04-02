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
8. Package-management commands (`add`, `remove`, `update`) belong here. They should take a full `Workspace.t`, mutate manifests, reload the workspace, and refresh or unlock `tusk.lock` from the new workspace state.
9. Root `tusk.toml` dependency sections are part of the refresh contract. A stale-lock check must treat the workspace root manifest the same way it treats member manifests.
10. `add`/`remove` operate on the dependencies explicitly declared in the target manifest section, not the effective dependency set after workspace inheritance. Do not remove or rewrite dependencies that only come from the workspace root.
11. `update` should surface concrete registry version changes as typed events so the CLI can say `Updated foo (old -> new)` without diffing lockfiles itself.

## Validate

`timeout 30 tusk build tusk-deps`
`timeout 30 tusk test -p tusk-deps`
