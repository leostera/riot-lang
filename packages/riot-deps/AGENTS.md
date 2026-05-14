# riot-deps AGENTS

`riot-deps` owns dependency solving, registry/index/archive/src cache interaction, lock refresh/unlock behavior, and projecting resolved package graphs back into `riot-model`.

## Rules

1. Keep the package manager logic here, not in `riot-model`.
2. `riot-model` remains the source of truth for shared data types like `Package`, `Lockfile`, and PM events.
3. Preserve downloaded manifests as registry/source manifests.
4. Bubble errors up with their original context.
5. Prefer small slices with tests; phase 1 may be naive operationally, but it should stay structurally honest.
6. Keep publish orchestration out of this package. `riot-deps` should expose low-level `Publisher` primitives; the command-level `fmt -> fix -> build -> metadata -> artifact -> upload` flow belongs in `riot-publish`.
7. Keep low-level publish planning honest: validate metadata and artifact inputs here while build orchestration stays in `riot-build`.
8. Package-management commands (`add`, `remove`, `update`) belong here. They should take a full `Workspace.t`, mutate manifests, reload the workspace, and refresh or unlock `riot.lock` from the new workspace state.
9. Root `riot.toml` dependency sections are part of the refresh contract. A stale-lock check must treat the workspace root manifest the same way it treats member manifests, and `riot.lock` staleness is decided by comparing the stored `dependency_hash` against a hash of the raw `[dependencies]`, `[build-dependencies]`, and `[dev-dependencies]` sections from every manifest in scope.
10. `add`/`remove` operate on the dependencies explicitly declared in the target manifest section. Workspace-root inherited dependencies stay owned by the workspace root. Multi-dependency add/remove should update the manifest once, emit one event per dependency changed, and refresh the lock once from the reloaded workspace.
11. `update` should surface concrete registry version changes as typed events so the CLI can say `Updated foo (old -> new)` without diffing lockfiles itself. Zero update targets means refresh everything; explicit package targets should preserve unrequested locked registry versions and unlock only the requested packages.
12. `add` should accept local path specs by loading the target `riot.toml`, discovering the real package name, and writing the dependency entry under that discovered name.
13. Git-backed source dependencies belong here. Normalize `github.com/...` and `https://github.com/...` specs into source locators, materialize them through the local Git cache under `~/.riot/registry/...`, and keep that source provenance in `riot.lock`.
14. `Dep_solver.lock_deps` is pubgrub-backed. Keep local package discovery, lock reconstruction, and refresh-preservation policy here, and route version choice and incompatibility resolution through `packages/pubgrub`.
15. Feed pubgrub typed `Std.Version` requirements, not reparsed requirement strings. Prefix requirements like `"0"` and `"0.2"` need to survive all the way into solver ranges.
16. When a registry package exists but no release matches the requested requirement, report that explicitly and include the available versions.
17. `search` belongs here too. Reuse the registry client, return structured package results, and keep query parsing/rendering in `riot-cli`.
18. `add`/`remove`/`update` progress should flow through `Riot_model.Event.kind` so package-management lifecycle events share the workspace event surface.
19. Registry package materialization should stay cache-first and on-demand. Lock solving may materialize selected registry packages when the real manifest is needed to capture build/dev dependency scopes.
20. `path` dependencies that also declare a publishable fallback (`version` or `source`) should prefer the local package only when the local manifest is actually present. In isolated published-artifact contexts, missing local paths fall back to the external dependency during resolution.
21. External workspace loading for `riot install` / `riot run` belongs here. Keep GitHub shorthand parsing, source materialization, registry release materialization, and package-selection policy in `riot-deps`, then return a normal `Workspace.t + package_name` pair to `riot-build`.
22. Bare `owner/repo[/subdir]` source specs are remote-looking shorthand for the workspace-free install/run path. Keep `riot add` path-dependency parsing explicit.
23. Remote source materialization is cache-first by default for `riot run` / `riot install`. Reuse the cached checkout when it already exists, and only fetch/refresh it when the caller explicitly passes `~update:true`.
24. Public external-install/run boundaries should prefer typed parsed specs (`Git_dependency.spec`, registry package spec values) over raw strings. Keep string parsing at the CLI edge or inside compatibility wrappers.
25. Package-manager request types should prefer `Riot_model.Package_name.t` over raw strings whenever the value is semantically a package name. Parse names at the CLI edge and only stringify them again for manifest text, registry APIs, paths, or JSON/error rendering.
26. `add` / `remove` / `update` should take an explicit caller-owned `Workspace_manager.t`, clear its cache after manifest rewrites, rescan, and rebuild `riot.lock` from the fresh workspace state. Refresh should keep only dependencies still present in the fresh workspace state.
27. Lockfile-only dependency projection for `riot build --deps` should not refresh or fingerprint workspace manifests. It may use the scanned root workspace metadata, but it must get third-party packages from `riot.lock` and ignore workspace/path lock packages.
