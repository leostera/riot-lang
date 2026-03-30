# Tusk Specs

This directory contains readable TLA+ models for the current `tusk` build
system design.

The spec set is intentionally narrow now: only stateful, interaction-heavy, or
cache-sensitive behavior stays here. Static graph-shape laws and direct
serialization contracts belong in normal OCaml tests.

## Layout

- `ActionCache.tla`: action-level cache semantics for `BuildForeignDependency`
  actions.
- `ActionCache.cfg`: smoke config for the action-cache slice.
- `ActionCacheCommandOrderBug.cfg`: failing config for the command-order cache
  collision.
- `PlanBundleVersionGate.tla`: warm-plan cache acceptance rules for persisted
  plan bundles.
- `PlanBundleVersionGate.cfg`: smoke config for the accepted bundle path.
- `PlanBundleVersionGateStaleVersion.cfg`: stale-version rebuild config.
- `PlanBundleToolchainInvalidation.tla`: planner-cache invalidation versus
  toolchain-sensitive action hashes.
- `PlanBundleToolchainInvalidation.cfg`: smoke config for the no-toolchain
  change baseline.
- `PlanBundleToolchainInvalidationBug.cfg`: failing config for stale hashes
  surviving a toolchain change.
- `ActionSchedulerCompletionAccounting.tla`: scheduler/executor completion
  accounting when dependents are skipped.
- `ActionSchedulerCompletionAccounting.cfg`: smoke config for the one-node
  completion baseline.
- `ActionSchedulerCompletionAccountingBug.cfg`: failing config for skipped
  nodes not advancing global completion.
- `PackageCoordinatorCacheShortCircuit.tla`: package-level cache hits and
  export rematerialization.
- `PackageCoordinatorCacheShortCircuit.cfg`: smoke config for the fully
  rematerializable package-cache-hit path.
- `PackageCoordinatorCacheShortCircuitBug.cfg`: failing config for incomplete
  export rematerialization.
- `PackageCoordinatorPendingFailurePropagation.tla`: pending-package wakeup
  when dependency results become available.
- `PackageCoordinatorPendingFailurePropagation.cfg`: smoke config for a
  successful pending-package wakeup.
- `PackageCoordinatorPendingFailurePropagationBug.cfg`: failing config for a
  pending package that stays stale in the graph after dependency failure.
- `BugInventory.md`: the running list of bug-shaped properties found by the
  extracted specs.
- `PropertyInventory.md`: the current TLA+ backlog for the remaining
  stateful/cached coordination slices.

## Why Start Here

The current cache path is split across a few packages:

- `tusk-planner` computes action hashes and plan-bundle cache keys.
- `tusk-executor` decides cache hit vs miss and materializes cached outputs.
- `tusk-store` keeps the immutable action artifact store.

That makes action-level caching a good first slice: it is small enough to
model readably, but central enough that a hash-design mistake becomes a real
build correctness bug.

## How To Work On The Spec

- Keep the model readable first. Use PlusCal when it helps make the
  state changes obvious.
- Keep each slice small and named after one semantic concern.
- For each slice, keep one smoke config and add a `*Bug.cfg` only when the
  current implementation-shaped semantics look wrong.
- Update `PropertyInventory.md` and `BugInventory.md` whenever a slice changes
  the current picture.
- Commit spec work often with conventional commits such as
  `spec(tusk): model scheduler readiness requeue`.

## Validation Commands

Run these from the repo root:

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  pcal.trans specs/tusk/<Slice>.tla
```

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/tusk/<Slice>.tla -config specs/tusk/<Slice>.cfg
```

```sh
timeout 60 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC specs/tusk/<Slice>.tla -config specs/tusk/<Slice>Bug.cfg
```

## Current Findings

- `ActionCacheCommandOrderBug.cfg` exposes the command-order hash collision in
  `packages/tusk-planner/src/action.ml`.
- `PlanBundleToolchainInvalidationBug.cfg` exposes the planner-cache key
  mismatch between toolchain-insensitive bundle reuse and toolchain-sensitive
  action hashes.
- `ActionSchedulerCompletionAccountingBug.cfg` exposes skipped nodes not being
  counted toward executor completion.
- `PackageCoordinatorCacheShortCircuitBug.cfg` exposes incomplete export
  rematerialization being treated as a cache hit.
- `PackageCoordinatorPendingFailurePropagationBug.cfg` exposes a stale
  package-graph node after dependency failure resolution.
