# Specs TODO

This is the continuation plan for the `tusk` spec pass in `specs/tusk/`.

The current strategy is:

1. keep extracting small, readable TLA+ / PlusCal slices from the current
   implementation
2. keep the spec set limited to stateful, interaction-heavy, or
   cache-sensitive behavior
3. use those slices to build a bug inventory
4. only after that inventory feels complete enough, write narrow OCaml
   regression tests for the confirmed properties
5. only then start fixing behavior

This file is meant to answer three questions:

- what to spec next
- how to evaluate each spec slice
- which implementation tests to write once the inventory is frozen

## Current State

- `specs/tusk/PropertyInventory.md` is the remaining TLA+ backlog
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
5. Add a bug config when there is a strong reason to believe the current
   design is wrong.
6. Regenerate the translated TLA after editing the PlusCal block.
7. Run TLC on the smoke config first.
8. Run TLC on the bug config separately.
9. If the bug config fails, write down the property in `BugInventory.md`.
10. If the bug config passes, either the property is not a bug or the model is
    missing the right interaction. Tighten the slice before moving on.
11. Commit just the spec work.

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
- failed dependency reasons survive into final package results
- package graph state and package result state must stay in sync

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
