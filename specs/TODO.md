# Specs TODO

This is the continuation plan for the `tusk` spec pass in
`specs/tusk/`.

The current strategy is:

1. keep extracting small, readable TLA+ / PlusCal slices from the current
   implementation
2. use those slices to build a bug inventory
3. only after that inventory feels complete enough, write narrow OCaml
   regression tests for the confirmed properties
4. only then start fixing behavior

This file is meant to answer three questions:

- what to spec next
- how to evaluate each spec slice
- which implementation tests to write once the inventory is frozen

## Current State

- `specs/tusk/PropertyInventory.md` is the full extraction backlog
- `specs/tusk/BugInventory.md` is the current list of bug-shaped properties
- `specs/tusk/README.md` documents the modeled slices and TLC commands
- the spec pass has already produced multiple failing bug configs
- no new OCaml regression tests should be added until the inventory phase is
  explicitly closed

## Ground Rules

- Prefer PlusCal when it makes the behavior easier to audit.
- Keep every slice small and named after one semantic concern.
- For each slice, keep one smoke config and add a `*Bug.cfg` only when the
  current implementation-shaped semantics look wrong.
- Update `specs/tusk/README.md`, `PropertyInventory.md`, and `BugInventory.md`
  whenever a slice changes the current picture.
- Commit spec work often with conventional commits such as
  `spec(tusk): model scheduler readiness requeue`.

## Standard Workflow For Each New Slice

1. Read the relevant production files and the nearest existing tests.
2. Pick one narrow law, not a whole subsystem.
3. Write the readable version first in PlusCal when possible.
4. Add a smoke config that should pass under the extracted semantics.
5. Add a bug config when there is a strong reason to believe the current design
   is wrong.
6. Regenerate the translated TLA after editing the PlusCal block.
7. Run TLC on the smoke config first.
8. Run TLC on the bug config separately.
9. If the bug config fails, write down the property in `BugInventory.md`.
10. If the bug config passes, either the property is not a bug or the model is
    missing the right interaction. Tighten the slice before moving on.
11. Commit just the spec work.

## TLC Evaluation Loop

Run these from the repo root.

For a changed PlusCal spec:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  pcal.trans specs/tusk/<Slice>.tla
```

For the smoke config:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/<Slice>.tla \
  -config specs/tusk/<Slice>.cfg
```

For the bug config:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/<Slice>.tla \
  -config specs/tusk/<Slice>Bug.cfg
```

Treat the results like this:

- Smoke config passes: the extracted baseline is internally consistent.
- Bug config fails with a clean counterexample: record the property in
  `BugInventory.md`.
- Bug config passes unexpectedly: do not force it into the bug inventory. First
  decide whether the property is actually fine, or whether the slice is still
  too weak.
- TLC state blow-up: shrink constants, split the property, or add an explicit
  model bound and document it in `README.md`.

## Exit Criteria For The Spec Phase

Do not switch into implementation-test-writing mode until most of these are
true:

- `WorkspaceGraph`, `PackagePlanning`, `ActionGraph`, `ActionScheduler`,
  `PackageCoordinator`, `ArtifactStore`, and `SandboxExecution` each have at
  least one current, readable slice
- each major orchestration area has at least one interaction-heavy slice, not
  only round-trip or hashing slices
- the bug inventory has been de-duplicated and each bug entry points at an
  owner package and a likely test home
- there is at least one bounded integration spec that composes planner,
  scheduler, coordinator, and store behavior

## Immediate Next Specs

These are the highest-value next slices.

### 1. `ActionSchedulerReadinessAndRequeue`

Why next:

- this is the most likely place for interaction bugs after the completion
  accounting issue
- it sits directly on the execution boundary where blocking, ready work, cache
  hits, and failure propagation meet

Primary sources:

- `packages/tusk-executor/src/action_queue.ml`
- `packages/tusk-executor/src/action_executor.ml`
- `packages/tusk-executor/tests/action_queue_workspace_graph_tests.ml`
- `packages/tusk-executor/tests/executor_behavior_tests.ml`

Property to model:

- a blocked node must only become ready once all of its unsatisfied
  dependencies are either completed successfully or satisfied from cache
- requeueing a blocked node must not lose missing dependencies
- failed dependencies must skip dependents instead of requeueing them forever

Done means:

- a readable PlusCal queue machine exists
- the smoke config covers success, cache-hit, and failure paths
- the model either proves the basic law or produces a tight counterexample

### 2. `ActionSchedulerLaterQueueTermination`

Why next:

- the queue has `ready`, `later`, `busy`, and `completed` structure, which is
  exactly the kind of multi-state interaction that produces quiescence bugs

Primary sources:

- `packages/tusk-executor/src/action_queue.ml`
- `packages/tusk-executor/src/action_executor.ml`

Property to model:

- when no further dependency changes are possible, work must not remain parked
  in `later`
- independent actions should continue even if one unrelated branch fails
- empty graphs should terminate immediately

Done means:

- the model covers at least one mixed graph with independent and dependent
  branches
- quiescent termination is expressed as a semantic invariant, not just a type
  invariant

### 3. `PackageCoordinatorFinalStatusDerivation`

Why next:

- `PackageCoordinatorCacheShortCircuit` and
  `PackageCoordinatorPendingFailurePropagation` already found issues
- the next suspicious area is the package-level built-vs-cached-vs-failed
  status summary

Primary sources:

- `packages/tusk-executor/src/coordinator.ml`
- `packages/tusk-executor/src/package_builder.ml`
- `packages/tusk-executor/tests/coordinator_tests.ml`

Property to model:

- a package is `cached` only when every completed action was a cache hit
- a package with zero actions is not accidentally marked `cached` on first
  build unless that is explicitly intended
- failed dependency reasons survive into final package results

Done means:

- final package result derivation is modeled separately from planning and
  scheduling
- the model either validates the current rules or captures a concrete status
  mismatch

### 4. `PackageCoordinatorLazyActivation`

Why next:

- this is the coordinator’s main orchestration law
- it is easy to accidentally over-activate packages too early or leave them
  pending forever

Primary sources:

- `packages/tusk-executor/src/coordinator.ml`
- `packages/tusk-planner/src/package_graph.ml`
- `packages/tusk-executor/tests/coordinator_tests.ml`

Property to model:

- packages activate only when their package-level dependencies are satisfied
- waking a dependency-satisfied package eventually moves it out of the pending
  set
- serial execution with parallelism `1` still makes progress across a chain

### 5. `PackagePlanningDependencyStateClassification`

Why next:

- it is small, central, and likely to affect package-level error reporting

Primary sources:

- `packages/tusk-planner/src/package_planner.ml`
- `packages/tusk-planner/tests/package_planning_tests.ml`

Property to model:

- missing dependency artifacts must produce `MissingDependencies`
- failed dependency results must produce `FailedDependencies`
- the planner must not collapse those two cases into one opaque failure mode

### 6. `PackagePlanningDependencySummaryPaths`

Why next:

- immutable-store path choice is part of the planner’s cache soundness story

Primary sources:

- `packages/tusk-planner/src/package_planner.ml`
- `packages/tusk-store/src/store.ml`
- `packages/tusk-planner/tests/package_planning_tests.ml`

Property to model:

- dependency summaries are built from store paths, not mutable target paths
- rematerialization into `out/` must not perturb planner hashes

### 7. `ActionGraphJsonRoundTrip`

Why next:

- `ActionJsonWarningFlagsRoundTrip` already found one lossy field
- there is likely more fidelity risk in action JSON than just warnings

Primary sources:

- `packages/tusk-planner/src/action.ml`
- `packages/tusk-planner/src/action_graph.ml`
- `packages/tusk-planner/tests/action_json_roundtrip_tests.ml`
- `packages/tusk-planner/tests/action_graph_tests.ml`

Property to model:

- action JSON preserves edges, package paths, serialized hashes, outputs, and
  non-warning compiler/linker fields

### 8. `WorkspaceGraphScopeAndTargeting`

Why next:

- this is the planner entry boundary, and mistakes here poison every later
  phase

Primary sources:

- `packages/tusk-planner/src/workspace_planner.ml`
- `packages/tusk-planner/src/package_graph.mli`
- `packages/tusk-planner/tests/workspace_like_graph_tests.ml`
- `packages/tusk-planner/tests/workspace_planner_target_tests.ml`
- `packages/tusk-planner/tests/workspace_planning_tests.ml`

Property to model:

- `Build`, `Runtime`, and `Dev` scopes stay distinct
- targeted builds include exactly the needed transitive closure
- build-scope self-dependencies for runtime nodes remain ordered correctly

### 9. `ArtifactStoreExportMaterialization`

Why next:

- store semantics are already implicated in one coordinator bug
- this is the next place where immutable vs mutable path confusion can leak in

Primary sources:

- `packages/tusk-store/src/store.ml`
- `packages/tusk-store/tests/store_tests.ml`

Property to model:

- export manifests stay relative
- malformed or absolute export paths are rejected
- promotion and rematerialization preserve nested relative structure
- saving the same action hash twice preserves first-writer contents

### 10. `SandboxExecutionCopyAndVerify`

Why next:

- sandbox correctness determines whether cache and scheduler results are
  meaningful

Primary sources:

- `packages/tusk-executor/src/sandbox.ml`
- `packages/tusk-executor/src/action_executor.ml`
- `packages/tusk-executor/tests/sandbox_tests.ml`
- `packages/tusk-executor/tests/action_executor_source_copy_tests.ml`

Property to model:

- declared inputs are copied into the sandbox with the right relative layout
- missing required outputs fail the action
- foreign dependency actions use their specialized output checks instead of the
  generic verifier

### 11. One Bounded Integration Spec

Suggested name:

- `BuildPipelineWarmCacheVsColdBuild`

Purpose:

- compose a tiny `workspace -> package planner -> action scheduler ->
  coordinator -> store` path
- prove that a warm cache hit and a cold build converge on the same final
  package result and exported outputs

This should stay tiny. The point is not completeness. The point is to have one
cross-boundary spec where interaction bugs can emerge.

## After The Spec Phase: Implementation Tests To Add

Do not write these yet. This is the test-writing plan once the inventory is
declared complete enough.

### Planner Regression Tests

- `packages/tusk-planner/tests/dependency_resolution_tests.ml`
  - prefer implementation when both `Foo.ml` and `Foo.mli` exist
  - respect nested namespace when resolving same-simple-name modules
  - respect alias-exposed targets when `open_modules` narrows lookup

- `packages/tusk-planner/tests/module_scanner_tests.ml`
  - keep allowed `.c` and `.h` entries as typed native-source nodes
  - preserve relative paths and canonical ordering under nested directories

- `packages/tusk-planner/tests/package_planning_tests.ml`
  - reject warm plan-bundle reuse on toolchain change
  - preserve `open_modules` across plan-bundle round-trip
  - distinguish `MissingDependencies` from `FailedDependencies`
  - use immutable store paths in dependency summaries

- `packages/tusk-planner/tests/action_json_roundtrip_tests.ml`
  - preserve combined warning flags
  - preserve linker flags, package paths, hashes, and outputs across JSON
    round-trip

- `packages/tusk-planner/tests/action_graph_tests.ml`
  - preserve dependency edges and dependency-first closure order in link actions
  - keep implicit shared-library requirements such as `stdlib.cmxa`

### Executor / Coordinator Regression Tests

- `packages/tusk-executor/tests/caching_tests.ml`
  - action cache must miss when `BuildForeignDependency.build_cmd` order
    changes

- `packages/tusk-executor/tests/action_queue_workspace_graph_tests.ml`
  - blocked-node requeue preserves missing dependencies
  - later-queue work drains once prerequisites resolve

- `packages/tusk-executor/tests/executor_behavior_tests.ml`
  - skipped nodes count toward global completion
  - independent branches continue after an unrelated branch fails
  - empty action graphs terminate immediately

- `packages/tusk-executor/tests/coordinator_tests.ml`
  - package cache short-circuit fails or rebuilds when any export cannot be
    rematerialized
  - pending package failure propagation updates package-graph state
  - final package status derives correctly from completed-action cache hits
  - lazy package activation only starts dependency-satisfied packages

- `packages/tusk-executor/tests/sandbox_tests.ml`
  - missing outputs fail execution
  - sandbox cleanup happens after success and failure

- `packages/tusk-executor/tests/action_executor_source_copy_tests.ml`
  - nested source copies preserve relative layout
  - package-relative and workspace-relative source paths both resolve

### Store Regression Tests

- `packages/tusk-store/tests/store_tests.ml`
  - export manifests reject absolute paths
  - rematerialization preserves nested relative structure
  - duplicate writes keep first-writer contents

### Server / CLI Regression Tests

- `packages/tusk-server/tests/cache_tests.ml`
  - package cache and action cache stats stay distinct in final build results

- `packages/tusk-server/tests/concurrent_tests.ml`
  - concurrent sessions do not cross-contaminate telemetry by `session_id`
  - same-package concurrent builds remain safe around shared cache entries

- `packages/tusk-cli/tests/build_lock_tests.ml`
  - one process holds the workspace lock at a time
  - reentrant acquisition inside one process remains prompt

## How To Evaluate The Implementation Tests Later

First discover suite names from the workspace root:

```sh
timeout 30 tusk completions --tests | rg 'tusk-(planner|executor|store|server|cli):'
```

Then run only the narrow suite you changed:

```sh
timeout 180 tusk test tusk-planner:dependency_resolution_tests
timeout 180 tusk test tusk-executor:coordinator_tests
timeout 180 tusk test tusk-store:store_tests
```

For a single case inside a suite, forward a pattern to `run-tests`:

```sh
timeout 180 tusk test tusk-planner:dependency_resolution_tests -- \
  --pattern "prefers implementation"
```

Evaluate implementation tests against the spec like this:

- If the spec smoke config passes and the regression test fails, the
  implementation is out of line with the extracted design.
- If the bug config fails and the regression test also fails, that is strong
  confirmation of a real bug.
- If the bug config fails but the implementation test passes, either the spec
  is too strong or the test does not exercise the same path.
- If the implementation behavior changes intentionally, update the spec and the
  test together.

## Suggested Order Once We Switch To Tests

Write implementation tests in this order:

1. action cache command order
2. module resolution cluster
3. module scanner native sources
4. plan-bundle round-trip and toolchain invalidation
5. action JSON round-trip fidelity
6. scheduler completion and readiness
7. package coordinator cache and pending-failure behavior
8. store export materialization
9. workspace graph targeting and scope
10. server concurrency and build lock

That order keeps us on the highest-value correctness boundaries first: cache
soundness, module resolution, scheduler correctness, and package orchestration.

## Definition Of Done For This TODO

This file can be considered complete when:

- the remaining spec slices above are either modeled or consciously deferred
- each entry in `BugInventory.md` has a concrete test home
- there is one small integration spec in place
- the team can pick any one bug property here and know exactly which spec,
  which future OCaml suite, and which evaluation command to use
