# riot-deps AGENTS

`riot-deps` owns dependency solving, registry/index/archive/src cache interaction, lock refresh/unlock behavior, and projecting resolved package graphs back into `riot-model`.

## Rules

1. Keep the package manager logic here, not in `riot-model`.
2. `riot-model` remains the source of truth for shared data types like `Package`, `Lockfile`, and PM events.
3. Do not rewrite downloaded manifests into path manifests.
4. Bubble errors up instead of hiding them behind fallback behavior.
5. Prefer small slices with tests; phase 1 may be naive operationally, but it should stay structurally honest.
6. Keep publish orchestration out of this package. `riot-deps` should expose low-level `Publisher` primitives; the command-level `fmt -> fix -> build -> metadata -> artifact -> upload` flow belongs in `riot-publish`.
7. Keep low-level publish planning honest: validate metadata and artifact inputs here, but do not take a dependency on `riot-build`.
8. Package-management commands (`add`, `remove`, `update`) belong here. They should take a full `Workspace.t`, mutate manifests, reload the workspace, and refresh or unlock `riot.lock` from the new workspace state.
9. Root `riot.toml` dependency sections are part of the refresh contract. A stale-lock check must treat the workspace root manifest the same way it treats member manifests, and `riot.lock` staleness is decided by comparing the stored `dependency_hash` against a hash of the raw `[dependencies]`, `[build-dependencies]`, and `[dev-dependencies]` sections from every manifest in scope.
10. `add`/`remove` operate on the dependencies explicitly declared in the target manifest section, not the effective dependency set after workspace inheritance. Do not remove or rewrite dependencies that only come from the workspace root.
11. `update` should surface concrete registry version changes as typed events so the CLI can say `Updated foo (old -> new)` without diffing lockfiles itself.
12. `add` should accept local path specs by loading the target `riot.toml`, discovering the real package name, and writing the dependency entry under that discovered name.
13. Git-backed source dependencies belong here. Normalize `github.com/...` and `https://github.com/...` specs into source locators, materialize them through the local Git cache under `~/.riot/registry/...`, and keep that source provenance in `riot.lock`.
14. `Dep_solver.lock_deps` is pubgrub-backed. Keep local package discovery, lock reconstruction, and refresh-preservation policy here, but route version choice and incompatibility resolution through `packages/pubgrub` instead of reintroducing ad-hoc recursive selection logic.
15. Feed pubgrub typed `Std.Version` requirements, not reparsed requirement strings. Prefix requirements like `"0"` and `"0.2"` need to survive all the way into solver ranges.

## Validate

`timeout 30 riot build riot-deps`
`timeout 30 riot test -p riot-deps`
