# tusk-executor

`tusk-executor` runs the planned build graph.

## Current shape

There are three execution layers:

1. `Coordinator`
   Schedules scoped package nodes (`pkg.build`, `pkg.runtime`, `pkg.dev`) in
   dependency order.
2. `Package_builder`
   Plans and materializes one scoped package node.
3. `Action_executor`
   Executes the action graph with dependency-aware parallelism.

## Invariants

- Package scheduling is scope-aware and keyed by `Package.key`
- Postponed package nodes are retried only after a dependency completes
- `pkg.runtime -> pkg.build` is an ordering edge, not a library dependency
- `Action_executor` is the single action execution path
- `concurrency = 1` is the serialized/debugging mode

## Notes

The older package coordinator and the extra action executor variants were
removed because they duplicated behavior without owning a distinct role.
