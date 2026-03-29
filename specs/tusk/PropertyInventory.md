# Tusk Property Inventory

This file is the extraction backlog for `tusk`.

The goal is not to list every helper function. The goal is to enumerate the
current semantic properties that look worth modeling in TLA+, grouped into
small slices we can audit and evolve with the codebase.

## How To Use This

- Treat each section as one candidate spec module or one tightly related spec
  family.
- Keep the slice names stable so we can mark progress over time.
- When a slice is modeled, add the `.tla` and `.cfg` files next to
  `ActionCache.tla` and update the status here.
- Prefer extracting laws from the current implementation and tests, not from
  what we wish the system looked like.
- If a slice exposes a likely bug, record that property in `BugInventory.md`.
- Do not add new OCaml regression tests during this phase. First build up the
  bug inventory from the specs.

## Slice Backlog

### 1. `ActionCache`

Status: `modeled`

Primary sources:
- `packages/tusk-planner/src/action.ml`
- `packages/tusk-planner/src/action_node.ml`
- `packages/tusk-executor/src/action_executor.ml`
- `packages/tusk-store/src/store.ml`
- `packages/tusk-executor/tests/caching_tests.ml`
- `packages/tusk-executor/tests/action_executor_source_copy_tests.ml`
- `packages/tusk-store/tests/store_tests.ml`

Properties:
- Action cache keys are content-derived, not execution-order-derived.
- Action-node hashes form a Merkle-style invalidation boundary over action
  fields, source contents, declared outputs, and dependency hashes.
- A cache hit materializes immutable stored outputs into the sandbox instead of
  re-running the action.
- A cache miss executes the action, verifies required outputs, and stores the
  result in the immutable artifact store.
- Different hashes stay isolated.
- Rewriting the same hash does not replace the first stored artifact.
- Current bug: `BuildForeignDependency.build_cmd` order is normalized away by
  the current hash, so two different command sequences can collide.

### 2. `WorkspaceGraph`

Status: `next`

Primary sources:
- `packages/tusk-planner/src/package_graph.mli`
- `packages/tusk-planner/src/workspace_planner.ml`
- `packages/tusk-planner/tests/workspace_like_graph_tests.ml`
- `packages/tusk-planner/tests/workspace_planner_target_tests.ml`

Properties:
- Package graphs are scoped by `Build`, `Runtime`, or `Dev`.
- Runtime graphs include runtime dependencies.
- Build graphs include build dependencies.
- Dev graphs do not accidentally inherit build-only dependencies.
- A runtime node with build dependencies depends on its own build-scope node.
- Package targeting keeps exactly the transitive closure needed for the selected
  package or set of packages.
- Dependencies appear before dependents in topological order.
- Missing workspace dependencies are reported before sorting.
- Unknown requested packages report the available package names.
- Cycles are reported explicitly.
- `get_unplanned_dependencies` only reports still-unplanned dependencies for the
  relevant scoped node.

### 3. `PackagePlanning`

Status: `next`

Primary sources:
- `packages/tusk-planner/src/package_planner.ml`
- `packages/tusk-planner/src/Tusk_planner.mli`
- `packages/tusk-planner/tests/package_planning_tests.ml`

Properties:
- A package cannot be fully planned until its dependency artifacts are
  available.
- Dependency state is classified as `MissingDependencies` vs
  `FailedDependencies`.
- Dependency summaries are built from immutable store locations, not mutable
  out directories.
- Package input hashes include build context, package metadata, workspace-local
  dependency details, and transitive dependency hashes.
- Planner bundle cache hits restore the module graph and action graph instead of
  rebuilding them.
- Stale planner artifact versions are ignored and force a rebuild of the plan
  graphs.
- Rehydrated plan bundles preserve precomputed action hashes instead of
  recomputing them.

### 4. `ModuleGraphBuilder`

Status: `partially modeled`

Primary sources:
- `packages/tusk-planner/src/module_scanner.ml`
- `packages/tusk-planner/src/module_graph.ml`
- `packages/tusk-planner/src/module_planner.ml`
- `packages/tusk-planner/src/library_definition.ml`
- `packages/tusk-planner/src/library_interface.ml`
- `packages/tusk-planner/src/alias_module.ml`
- `packages/tusk-planner/tests/dependency_resolution_tests.ml`
- `packages/tusk-planner/tests/module_scanner_tests.ml`

Properties:
- Source scanning is deterministic.
- Scanner ordering puts `.mli` before `.ml`, files before directories, and
  keeps directories last.
- Scanned paths are relative to the package root or planning root, not absolute.
- Only allowed source files survive filtering into the builder.
- Binary source files are excluded from the library module graph and handled
  separately.
- A directory becomes a library namespace boundary.
- Directories with no OCaml content do not create synthetic library-interface
  nodes.
- Concrete library interface files like `foo/foo.ml` and `foo/foo.mli` take
  precedence over generated ones.
- The library interface file itself is excluded from its own child-module set.
- A subdirectory is ignored as a child module when a same-named file already
  exists.
- Alias nodes are inserted for namespace flattening.
- Generated library interfaces depend on all child modules, while concrete
  interfaces depend only on child files to avoid cycles with sublibraries.
- Implementation nodes depend on their matching interface nodes.
- Wiring via `ocamldep` adds module-reference edges after scanning-time
  structural edges are already in place.
- `MLI -> ML` dependency edges are filtered out.
- Self-edges are filtered out during dependency wiring.
- Dependency resolution uses the file's own nested namespace, not only the
  package root namespace.
- When both interface and implementation exist for a referenced module, the
  graph prefers the implementation node for downstream compilation edges.

Implemented slice:
- `ModuleGraphStructure.tla` covers the structural pre-`ocamldep` rules around
  binary exclusion, self-file exclusion, file-over-directory precedence,
  synthetic library-node creation, and concrete-vs-generated child dependency
  selection.
- `ModuleGraphWiring.tla` covers the current post-`ocamldep` registry wiring
  rules around self-edge filtering, `MLI -> ML` filtering, and the current
  "add every surviving registry candidate" behavior.
- `ModuleGraphNamespaceResolution.tla` covers the current nested-namespace
  reconstruction step before alias handling and shows that the reconstructed
  qualified dependency name is not currently used to narrow registry lookup.
- `ModuleGraphAliasResolution.tla` covers the gap between scan-time alias
  context and wire-time dependency lookup, and shows that current lookup does
  not use alias-exposed targets to narrow registry matches.
- `ModuleScannerPipeline.tla` covers scan-time entry tagging, relative-path
  storage, filter pruning, and canonical ordering for the single-directory
  scanner path.

Still open:
- nested-namespace dependency resolution
- downstream preference for implementation nodes
- alias insertion details and generated alias shape
- scanner recursion over nested directories

Current bug found:
- The current `wire_dependencies` behavior does not satisfy the existing
  planner expectation that downstream references prefer the implementation node
  when both interface and implementation exist. `Bar.mli -> Foo` currently
  keeps `Foo.mli` and drops `Foo.ml`.
- The current namespace-reconstruction step does not constrain registry lookup.
  A source that reconstructs `Pkg__Sub__Foo` can still wire both `Pkg__Foo` and
  `Pkg__Sub__Foo` because lookup is keyed only by `Foo`.
- The current alias-context step does not constrain registry lookup either.
  Even when `open_modules` alias context points at `Pkg__Util__Foo`, wiring can
  still add both `Pkg__Foo` and `Pkg__Util__Foo`.
- The current scanner pipeline drops allowed native source files. `.c` and `.h`
  extensions are tagged as `Other`, so `filter_entries` never keeps them as
  `C` / `H` entries.

### 5. `ActionGraph`

Status: `next`

Primary sources:
- `packages/tusk-planner/src/action_graph.mli`
- `packages/tusk-planner/src/action_node.ml`
- `packages/tusk-planner/tests/action_graph_tests.ml`
- `packages/tusk-planner/tests/action_json_roundtrip_tests.ml`
- `packages/tusk-planner/tests/dependency_resolution_tests.ml`

Properties:
- The action graph preserves dependency structure from the module graph.
- Action-node hashes change when package-relative source contents change.
- Action graph JSON round-trips preserve edges.
- Action graph JSON round-trips preserve package paths and serialized hashes.
- Action JSON round-trips preserve compiler flags, linker flags, foreign build
  env, and outputs.
- Shared-library link actions always include `stdlib.cmxa` even without an
  explicit dependency.
- Shared-library link actions include transitive package libraries in
  dependency-first order.
- Platform-specific linker flags are injected deterministically.
- Dependency closure order is deduplicated and dependency-first.
- Module planning prefers an implementation when both interface and
  implementation are present.

### 6. `ActionScheduler`

Status: `next`

Primary sources:
- `packages/tusk-executor/src/action_queue.ml`
- `packages/tusk-executor/src/action_executor.ml`
- `packages/tusk-executor/tests/action_queue_workspace_graph_tests.ml`
- `packages/tusk-executor/tests/executor_behavior_tests.ml`

Properties:
- An action is ready only when all of its dependencies succeeded or were
  satisfied from cache.
- Failed dependencies cause dependents to be skipped instead of executed.
- Requeueing a blocked node also requeues the missing dependencies it is still
  waiting on.
- Completion means every node is accounted for, with no ready, later, or busy
  work left behind.
- Empty graphs terminate immediately.
- Independent actions can continue even when other independent actions fail.
- Dependent actions do not run after an upstream failure.
- Parallel execution is owned by the action scheduler, not by a competing
  package-level worker pool.

### 7. `SandboxExecution`

Status: `next`

Primary sources:
- `packages/tusk-executor/src/sandbox.ml`
- `packages/tusk-executor/src/action_executor.ml`
- `packages/tusk-executor/tests/action_executor_source_copy_tests.ml`
- `packages/tusk-executor/tests/sandbox_tests.ml`

Properties:
- Sandboxes copy declared inputs before execution.
- Nested input paths are preserved when copied into the sandbox.
- Source copy resolution accepts both package-relative and workspace-relative
  source paths.
- Missing expected outputs fail the execution.
- Sandboxes are cleaned up after use.
- Foreign dependency actions skip normal output verification in the executor and
  rely on their own explicit output checks.
- The sandbox directory is stable enough to act as the execution root for all
  actions in a package build.

### 8. `PackageCoordinator`

Status: `next`

Primary sources:
- `packages/tusk-executor/src/coordinator.ml`
- `packages/tusk-executor/src/package_builder.ml`
- `packages/tusk-executor/src/coordinator.mli`
- `packages/tusk-executor/tests/coordinator_tests.ml`
- `packages/tusk-server/tests/cache_tests.ml`

Properties:
- Workspace builds plan packages lazily and only activate a package when its
  package-level dependencies are satisfied.
- Package orchestration is dependency-aware and respects scoped package keys.
- Package cache short-circuiting can skip action execution when the package hash
  artifact already exists.
- When package exports are missing from the out directory but present in the
  store, they are rematerialized instead of rebuilding the package.
- A package is reported as cached only when every completed action was cached.
- Failed package dependencies cause dependents to be skipped or failed with a
  dependency reason.
- Package export manifests deduplicate exported names by keeping the first
  producer.
- Serial orchestration still succeeds when available parallelism is one.
- Workspace result accounting tracks built, cached, and failed packages.

### 9. `ArtifactStore`

Status: `next`

Primary sources:
- `packages/tusk-store/src/store.mli`
- `packages/tusk-store/src/store.ml`
- `packages/tusk-store/tests/store_tests.ml`

Properties:
- Artifact storage is content-addressed by hash.
- A cache entry only exists if its manifest exists.
- Stored artifact paths preserve nested relative output paths.
- Promotion recreates the relative directory structure under the target
  directory.
- Saving the same hash twice keeps the first writer's contents.
- Plan bundles live in a separate namespace from artifact directories.
- Package export manifests store metadata only: export name, relative path, and
  owning action hash.
- Resolving a named export rejects absolute export paths.
- Malformed export manifests are treated as absent.
- Materializing package exports copies from `cache/<action_hash>/<path>` to
  `target_dir/<name>`.
- `hash_dir_of` gives a stable immutable location for dependency summaries even
  before materialization.

### 10. `BuildSession`

Status: `later`

Primary sources:
- `packages/tusk-server/src/build_server.ml`
- `packages/tusk-server/src/protocol.mli`
- `packages/tusk-cli/src/local_session.ml`
- `packages/tusk-server/tests/server_tests.ml`
- `packages/tusk-server/tests/concurrent_tests.ml`

Properties:
- Build sessions are one-shot and local, not daemonized.
- Telemetry events are filtered by `session_id` so concurrent sessions do not
  cross-contaminate client streams.
- Server-side build stats track package cache hits/misses and action cache
  hits/misses separately.
- Concurrent builds of different packages do not interfere with each other.
- Concurrent builds of the same package remain safe.
- Shared cache entries can be reused across concurrent builds.
- Final build responses separate successful built results from error results.

### 11. `BuildLock`

Status: `later`

Primary sources:
- `packages/tusk-cli/src/local_session.ml`
- `packages/tusk-cli/tests/build_lock_tests.ml`

Properties:
- Only one process should hold the workspace build lock at a time.
- Reentrant acquisition within the same process should complete promptly.
- The lock is released even if the protected callback raises an exception.
- Waiters retry instead of failing immediately when the lock is already held.

### 12. `CLIRequestSurface`

Status: `later`

Primary sources:
- `packages/tusk-cli/src/build.ml`
- `packages/tusk-cli/tests/build_tests.ml`
- `packages/tusk-cli/tests/test_selection_tests.ml`

Properties:
- `tusk build` accepts zero, one, or many package arguments.
- Multi-package builds become an explicit `Packages` target, not a sequence of
  unrelated single-package requests.
- Test-selection parsing keeps the user query intact while separating package
  narrowing from free-text filtering.

## Suggested Extraction Order

1. `WorkspaceGraph`
2. `PackagePlanning`
3. `ModuleGraphBuilder`
4. `ActionGraph`
5. `ActionScheduler`
6. `ArtifactStore`
7. `PackageCoordinator`
8. `BuildSession`
9. `BuildLock`

That order keeps us moving from the most structural, easiest-to-bound slices
into the more concurrent and orchestration-heavy ones.

## Open Questions

- We probably want one future integration spec that composes
  `WorkspaceGraph + PackagePlanning + PackageCoordinator + ArtifactStore`.
- Some CLI parsing properties are real behavior, but they may not deserve TLA+
  unless they interact with concurrency or caching.
- A few executor behavior tests are currently commented out. They still suggest
  intended laws, but we should treat them as weaker evidence than active tests
  until they are re-enabled.
