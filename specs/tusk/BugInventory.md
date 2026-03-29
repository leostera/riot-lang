# Tusk Bug Inventory

This file is the running list of `tusk` properties that currently look buggy
under the extracted specs.

The goal here is triage, not fixes and not OCaml tests yet.

Workflow:

- extract one small spec slice from the current implementation
- run TLC against a passing smoke model and, when useful, a failing bug model
- if the spec exposes a likely design bug, add the property here
- only after this list feels complete enough do we switch to writing OCaml tests

## Status Meanings

- `suspected`: the property looks wrong from code reading or partial modeling,
  but we do not yet have a tight failing spec
- `spec-failing`: the current extracted TLA+ slice produces a counterexample
- `spec-failing + impl-reproduced`: the spec fails and we have also reproduced
  the same behavior directly against the current implementation

## Current Bug Properties

### 1. Action Cache Must Respect Command Order

Status: `spec-failing`

Spec slice:
- `ActionCache.tla`
- `ActionCacheCommandOrderBug.cfg`

Property:
- Two `BuildForeignDependency` actions with different ordered command sequences
  must not share the same cache entry.

Why it looks buggy:
- The current action hash normalizes `build_cmd` as a multiset, so
  `["Prep", "Compile"]` and `["Compile", "Prep"]` collide.
- The cache machine can then reuse a stored artifact for the wrong command
  order.

Primary source area:
- `packages/tusk-planner/src/action.ml`
- `packages/tusk-executor/src/action_executor.ml`

Counterexample shape:
- `FirstBuild` stores the result of `<<"Prep", "Compile">>`
- `SecondBuild` with `<<"Compile", "Prep">>` hits cache instead of rebuilding

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 2. Module Wiring Must Prefer Implementation Nodes When Both Exist

Status: `spec-failing + impl-reproduced`

Spec slice:
- `ModuleGraphWiring.tla`
- `ModuleGraphWiringPreferenceBug.cfg`

Property:
- When both `Foo.mli` and `Foo.ml` exist for a referenced module, downstream
  graph edges should prefer the implementation node instead of the interface
  node.

Why it looks buggy:
- Registry lookup returns both candidates for `Foo`
- implementation registration is prepended ahead of interface registration
- the current `MLI -> ML` filter then drops `Foo.ml` for interface consumers
- the remaining edge points only to `Foo.mli`

Primary source area:
- `packages/tusk-planner/src/module_graph.ml`
- `packages/tusk-planner/src/module_registry.ml`

Implementation reproduction:
- `_build/debug/aarch64-apple-darwin/out/tusk-planner/dependency_resolution_tests run-tests "module graph prefers implementation when interface exists"`
- current result: `expected bar.mli to depend on foo.ml implementation node only`

Counterexample shape:
- `BarMLI` processes `FooML` first and skips it
- `BarMLI` then keeps only `FooMLI`

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 3. Module Dependency Resolution Must Respect The Source File's Namespace

Status: `spec-failing`

Spec slice:
- `ModuleGraphNamespaceResolution.tla`
- `ModuleGraphNamespaceResolutionBug.cfg`

Property:
- When a source file reconstructs a qualified dependency name in its own nested
  namespace, resolution should prefer the matching namespaced module over
  same-simple-name modules from other namespaces.

Why it looks buggy:
- `wire_dependencies` reconstructs a namespaced `Module_name` for each
  dependency based on the source file's subdirectory
- the next lookup step converts that dependency back to its simple name
- the registry is keyed only by simple names, so lookup returns every `Foo`
  regardless of namespace
- the current machine then wires every returned candidate

Primary source area:
- `packages/tusk-planner/src/module_graph.ml`
- `packages/tusk-planner/src/module_registry.ml`

Counterexample shape:
- the source reconstructs `Pkg__Sub__Foo`
- simple-name lookup for `Foo` returns both `Pkg__Foo` and `Pkg__Sub__Foo`
- both edges are added instead of preferring only `Pkg__Sub__Foo`

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 4. Module Dependency Resolution Must Respect Alias-Exposed Targets

Status: `spec-failing`

Spec slice:
- `ModuleGraphAliasResolution.tla`
- `ModuleGraphAliasResolutionBug.cfg`

Property:
- When a module has alias context that exposes a specific qualified target for a
  simple dependency name, dependency wiring should prefer the alias-matched
  target over unrelated same-simple-name modules.

Why it looks buggy:
- scan-time graph construction records alias nodes in `open_modules`
- action generation already turns those alias nodes into `-open` compiler flags
- but `wire_dependencies` never consults alias context when adding graph edges
- simple-name registry lookup therefore returns every `Foo`, even when the open
  alias context points specifically at `Pkg__Util__Foo`

Primary source area:
- `packages/tusk-planner/src/module_graph.ml`
- `packages/tusk-planner/src/alias_module.ml`
- `packages/tusk-planner/src/action_graph.ml`

Counterexample shape:
- alias context says `Foo` should mean `Pkg__Util__Foo`
- simple-name lookup for `Foo` returns both `Pkg__Foo` and `Pkg__Util__Foo`
- both edges are added instead of preferring only `Pkg__Util__Foo`

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 5. Allowed Native Source Files Must Survive Scanner Tagging And Filtering

Status: `spec-failing`

Spec slice:
- `ModuleScannerPipeline.tla`
- `ModuleScannerNativeTagBug.cfg`

Property:
- Allowed `.c` and `.h` source files should survive the scanner + filter
  pipeline as dedicated `C` / `H` entries.

Why it looks buggy:
- `module_scanner.ml` defines first-class `C` and `H` entry kinds
- `filter_entries` has explicit cases that retain `C` and `H` when allowed
- but the current `scan_directory` tagging logic only recognizes `.ml` and
  `.mli`
- every other extension becomes `Other`, so allowed `.c` / `.h` files are
  dropped before the planner can use them

Primary source area:
- `packages/tusk-planner/src/module_scanner.ml`
- `packages/tusk-planner/src/module_graph.ml`

Counterexample shape:
- `src/stubs.c` and `src/api.h` are both in the allowed source set
- current scanner tags both as `Other`
- filtering drops both instead of keeping typed native-source entries

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 6. Plan Bundle Round-Trip Must Preserve Module Open Context

Status: `spec-failing`

Spec slice:
- `PlanBundleModuleGraphRoundTrip.tla`
- `PlanBundleModuleGraphOpenModulesBug.cfg`

Property:
- Persisted plan bundles should preserve a module graph's `open_modules`
  context across save/load round-trips.

Why it looks buggy:
- `module_graph_to_json` serializes every node with `("opens", Array [])`
- `module_graph_of_json` reconstructs every node with `open_modules = []`
- any non-empty open-module context is therefore erased on a warm-plan cache
  hit
- that is a lossy round-trip for the module graph value returned by the planner

Primary source area:
- `packages/tusk-planner/src/package_planner.ml`

Counterexample shape:
- the original module graph gives `Main` a non-empty `open_modules` set
- serialization writes an empty opens list anyway
- deserialization restores `Main` with `open_modules = []`

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 7. Plan Bundle Cache Key Must Invalidate On Toolchain Change

Status: `spec-failing`

Spec slice:
- `PlanBundleToolchainInvalidation.tla`
- `PlanBundleToolchainInvalidationBug.cfg`

Property:
- A persisted plan bundle must not be reused across toolchain changes unless
  the restored action hashes are recomputed for the new toolchain.

Why it looks buggy:
- `compute_input_hash` decides the planner bundle cache key
- that key currently includes build context, package metadata, workspace-local
  dependency details, and transitive dependency hashes, but not the toolchain
- `Action_node.make` does include the toolchain hash in every action-node hash
- so a warm-plan cache hit can restore an action graph whose stored hashes were
  computed under an older toolchain

Primary source area:
- `packages/tusk-planner/src/package_planner.ml`
- `packages/tusk-planner/src/action_node.ml`

Counterexample shape:
- the first plan stores an action hash derived from `toolchain-v1`
- the second plan uses `toolchain-v2` but computes the same plan-bundle key
- the planner takes a cache hit and restores the old `toolchain-v1` action hash
  instead of replanning or rehashing

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 8. Action JSON Round-Trip Must Preserve Combined Warning Flags

Status: `spec-failing`

Spec slice:
- `ActionJsonWarningFlagsRoundTrip.tla`
- `ActionJsonWarningFlagsRoundTripBug.cfg`

Property:
- Action JSON round-trips must preserve `Ocamlc.Warning [...]` flag lists,
  including combined warning payloads.

Why it looks buggy:
- `flags_to_string` serializes `Warning [...]` into one `-w` payload that can
  contain multiple warning codes
- the current `Action.from_json` warning parser only recognizes `-w -a` and
  `-w -49`
- any combined payload such as `-w -a-49` falls through to `Warning []`
- a warm plan-cache hit can therefore restore compile actions with weaker
  warning configuration than the original planned graph

Primary source area:
- `packages/tusk-toolchain/src/ocamlc.ml`
- `packages/tusk-planner/src/action.ml`

Counterexample shape:
- the original action carries `Warning [All; NoCmiFile]`
- serialization emits the combined warning payload `<<"a", "49">>`
- deserialization restores `Warning []` instead of the original warning list

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 9. Action Scheduler Must Count Skipped Nodes Toward Completion

Status: `spec-failing`

Spec slice:
- `ActionSchedulerCompletionAccounting.tla`
- `ActionSchedulerCompletionAccountingBug.cfg`

Property:
- When a dependent node is marked `Skipped` because an upstream dependency
  failed, the scheduler/executor interaction must still count that node toward
  global completion.

Why it looks buggy:
- `Action_queue.next` can mark a node `Skipped` immediately after seeing a
  failed dependency
- `action_executor.execute` increments `completed_count` only on worker
  `ActionCompleted` messages
- skipped nodes are therefore recorded in `queue.completed` without increasing
  `completed_count`
- the executor can become quiescent with no ready work and no busy workers, but
  still believe it is waiting for more completions

Primary source area:
- `packages/tusk-executor/src/action_queue.ml`
- `packages/tusk-executor/src/action_executor.ml`

Counterexample shape:
- `A` runs and fails
- dependent node `B` is marked `Skipped` inside `Action_queue.next`
- `queue.completed` contains both `A` and `B`
- `completed_count` is still `1`, so the executor is under-counted at
  quiescence

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 10. Package Cache Short-Circuit Must Materialize Every Export

Status: `spec-failing`

Spec slice:
- `PackageCoordinatorCacheShortCircuit.tla`
- `PackageCoordinatorCacheShortCircuitBug.cfg`

Property:
- A package-level cache short-circuit must not report the package as cached
  unless every declared export is present in the target directory afterwards.

Why it looks buggy:
- `maybe_short_circuit_cached_package` short-circuits on the package hash
  artifact and then asks the store to materialize missing exports
- `materialize_package_exports` currently logs a warning and returns `Ok ()`
  when an export source is missing from the action-level store
- the coordinator treats that `Ok ()` as successful rematerialization and marks
  the package `Cached`
- the target directory can therefore stay incomplete even though the package is
  reported as a cache hit

Primary source area:
- `packages/tusk-executor/src/coordinator.ml`
- `packages/tusk-store/src/store.ml`

Counterexample shape:
- the package hash artifact exists
- `lib.cmxa` can be materialized but `lib.cmxs` is missing from the store
- the coordinator still sets the package status to `Cached`
- the target directory ends with only `{"lib.cmxa"}` instead of all exports

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

### 11. Pending Package Failure Propagation Must Update The Package Graph

Status: `spec-failing`

Spec slice:
- `PackageCoordinatorPendingFailurePropagation.tla`
- `PackageCoordinatorPendingFailurePropagationBug.cfg`

Property:
- Once a pending package is resolved to a failed result because one of its
  dependencies failed, the returned `package_graph` must no longer report that
  package as `Unplanned`.

Why it looks buggy:
- `try_plan_pending_packages` revisits packages parked in `pending_planning`
- in the `deps_failed` branch it inserts a failed `package_results` entry and
  removes the package from `pending_planning`
- but it does not update the corresponding `package_graph` node
- the result table and returned graph can therefore disagree about the same
  package's state

Primary source area:
- `packages/tusk-executor/src/coordinator.ml`
- `packages/tusk-planner/src/package_graph.ml`

Counterexample shape:
- `Dep` is already failed
- pending package `Pkg` is revisited and resolved to a failed result
- `Pkg` leaves `pending_planning`
- `package_graph_state["Pkg"]` still says `unplanned`

Deferred follow-up:
- write an OCaml regression test only after we finish the broader bug-inventory
  pass

## Next Candidates To Model

These are not yet bug entries. They are the next properties most likely to
surface design issues once modeled.

- Planner bundle cache rehydration and stale-version invalidation
- Action-scheduler readiness, skip propagation, and completion accounting
- Package-level cache rematerialization versus rebuild decisions
- Package-export manifest deduplication and rematerialization rules
- Shared package-planner bundle cache hash/version invalidation rules
