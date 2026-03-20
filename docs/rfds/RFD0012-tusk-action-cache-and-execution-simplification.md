# RFD0012 - Simplify Tusk Around Action-Level Caching

- Feature Name: `tusk_action_cache_and_workspace_execution`
- Start Date: `2026-03-20`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes making the action node, not the package, the primary cache
unit of the `tusk` build system. `tusk-planner` should always produce full
action graphs, `tusk-executor` should schedule and reuse action nodes under a
single workspace-level concurrency budget, and `tusk-store` should atomically
persist action artifacts plus package export manifests. The existing one-shot
local session model and streamed CLI output remain in place.

The goal is to let large packages rebuild incrementally without forcing users to
split them into many packages early, while also deleting the current duplicate
package-level and action-level scheduling and caching paths.

## Motivation
[motivation]: #motivation

The current implementation already contains most of the ingredients for
action-level caching, but they are not the active architecture.

Today:

- `tusk-planner` computes deterministic `Action_node` hashes.
- `tusk-executor` has action-level telemetry types and a parallel action queue.
- `tusk-store` is already a generic hash-addressed artifact store.

But the live build path still treats the package as the only durable cache
boundary:

- `package_planner.ml` computes a package input hash and skips full planning
  when that package hash already exists in the store.
- `package_builder.ml` checks the same package hash again and only saves the
  final package outputs.
- `parallel_action_executor.ml` executes action nodes in parallel, but it does
  not consult the store per action node.

The result is an architecture with two models:

1. a package-level cache model that is real
2. an action-level cache model that is implied by the types, but mostly dormant

This creates several concrete problems.

### 1. Large packages still rebuild too much

If one module changes inside a large package, the package-level cache misses and
the package rebuild path runs again. The system already knows how to identify
individual action nodes, but it does not use those hashes to skip unchanged
actions.

That means larger packages are penalized. Contributors are pushed toward
package-splitting for performance reasons even when the right architectural
boundary is still one package.

### 2. Planning and execution are coupled through built artifacts

`Dependency.t` currently carries a built `Artifact.t`. `package_planner.ml`
therefore treats dependency packages as plannable only after they have already
been built and promoted to the store.

This couples:

- planning
- execution state
- store layout

That coupling is one of the reasons `tusk` has to plan packages during
execution, rather than producing a clean workspace plan first and then
executing it.

### 3. The scheduler is duplicated

The build path currently has two readiness schedulers:

- `coordinator2.ml` schedules packages
- `parallel_action_executor.ml` schedules actions inside a package

Both keep queues, both track dependencies, and both compete for concurrency
control. `package_builder.ml` currently gives each package action executor
`System.available_parallelism`, which means package-level parallelism and
action-level parallelism can oversubscribe the machine.

That is complexity without a clean ownership boundary.

### 4. Streamed builds exist, but action streaming does not really escape

The local-session protocol already streams build events. That is good and should
stay.

But `parallel_action_executor.ml` emits action events with a synthetic session
id instead of the real build session id. `build_server.ml` filters events by
session id, so these action events are not reliably part of the actual build
stream.

The system has the shape of streamed action execution, but not a coherent
implementation of it.

### 5. The store contract is too weak for granular caching

`tusk-store` currently creates the final hash directory in place. For
action-level caching this is not good enough:

- multiple workers may race to write the same artifact
- readers may observe a partially written entry
- promotion currently assumes a flat directory copy

Granular caching requires atomic `put-if-absent` semantics and recursive path
handling.

### Use cases this RFD addresses

- A contributor edits one leaf module in a large library package and expects
  only the affected compile and relink actions to run.
- A contributor changes link flags or one foreign dependency and expects
  recompilation to stop at the correct boundary.
- Two independent packages should build in parallel without nested worker pools
  oversubscribing the machine.
- `tusk build`, `tusk run`, and `tusk test` should continue to stream progress
  as work happens.
- A future remote cache or remote execution story should build on the same
  action identity model instead of replacing package-level cache semantics
  later.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

After this change, contributors should still think about `tusk` in package
terms, but `tusk` itself should think in action terms.

Packages remain:

- the authoring unit
- the dependency declaration unit
- the user-facing unit for `tusk build --package`, `tusk run`, and output
  reporting

Actions become:

- the cache unit
- the execution unit
- the dependency unit inside the executor

### Contributor model

When a contributor runs `tusk build`, the system should behave like this:

1. resolve the requested packages
2. plan those packages into action graphs
3. schedule ready action nodes across the whole workspace under one concurrency
   budget
4. for each action node, either restore it from the cache or execute it
5. materialize package exports into the usual `out/` directory
6. stream progress while this is happening

The important shift is that packages are no longer the thing that must be fully
fresh or fully cached. A package can complete from a mix of:

- cached action nodes
- freshly executed action nodes
- skipped action nodes because a dependency failed

### Example: editing one module in a large library package

Assume package `foo` contains 120 modules and one binary.

Today, one changed implementation file causes a package cache miss and `foo`
re-enters the full package build flow.

With this proposal:

- the changed module compile action misses
- downstream archive/shared-library/link actions that depend on it miss
- unaffected module compile actions hit the cache
- the package still shows up as one package in the UI

The contributor keeps one package. The build system still behaves incrementally.

### Example: repeated no-op build

On a no-op rebuild:

- `tusk` still performs planning
- almost every action node is a cache hit
- the executor materializes only what is needed for the requested package
  outputs
- the CLI still streams progress and finishes quickly

This intentionally prefers architectural simplicity over preserving the current
package-level planning fast path.

### Example: independent packages

If packages `a` and `b` do not depend on each other, and package `c` depends on
both:

- ready actions from `a` and `b` can execute in parallel
- `c` actions become ready only after their dependencies are complete
- one executor owns the full concurrency budget

This gives both package-level and intra-package parallelism without nesting
multiple worker pools that each believe they own the whole machine.

### Streamed build output

The existing local-session streaming model stays:

- the CLI still gets a stream of build events
- package-level start and completion messages still exist
- action-level events become real session events instead of internal-only noise

The default CLI output should stay package-oriented and quiet. Action-level
streaming is primarily for correctness, telemetry, and future verbose or UI
modes.

### Final shape

```mermaid
flowchart TD
  A[tusk command] --> B[plan workspace packages]
  B --> C[build workspace action graph]
  C --> D[workspace action executor]
  D --> E{action hash in store?}
  E -->|yes| F[materialize cached action outputs]
  E -->|no| G[execute action and save artifact]
  F --> H[mark action complete]
  G --> H
  H --> I[materialize package exports]
  I --> J[stream package and action events]
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Core invariants

The proposed steady-state architecture has four invariants:

1. `Action_node.hash` is the primary build artifact identity.
2. Planning does not consult the store to decide whether to build.
3. Execution owns cache lookup, cache write, and readiness tracking.
4. Package outputs are a materialized view over action artifacts, not the
   primary cache entry.

This keeps responsibility boundaries clear:

- `tusk-planner` decides graph shape and action identity
- `tusk-executor` decides what runs and what is restored
- `tusk-store` decides how artifacts are persisted

## 2. Planner changes

### 2.1 `Workspace_planner` remains package-oriented

`workspace_planner.ml` should keep doing what it is good at:

- resolve the requested package subset
- validate package dependency edges
- produce deterministic topological package order

That remains the right boundary for workspace-level package selection.

### 2.2 `Package_planner` becomes pure planning

`package_planner.ml` should stop doing planning-time cache lookup.

The current package input hash fast path should be removed from the primary
path. Planning should always produce:

- the module graph
- the action graph
- the package export description
- the package summary hash derived from planned exports

This means:

- no `Store.exists` lookup during planning
- no dummy empty graphs on package cache hit
- no "planned only if dependency packages are already built" rule

### 2.3 Replace built dependency artifacts with planned dependency summaries

`Dependency.t` is currently a built-artifact shape:

- package metadata
- `Artifact.t`
- transitive depset
- hash

That should be replaced with a planned dependency summary, for example:

- package metadata
- exported library filenames
- exported include directories
- foreign-link inputs
- package summary hash

The planner should consume dependency summaries from already planned dependency
packages, not from already built package graph nodes.

This is the change that decouples planning from execution state.

### 2.4 Package exports become explicit

The planner should describe which outputs are package exports, for example:

- `.cmxa`
- `.a`
- `.cmxs`
- requested binaries
- command binaries

That export description is what later drives:

- dependent package planning
- final materialization to `out/`
- `FindArtifact` and `tusk run`

### 2.5 Action hashing stays, but must become semantically correct

The existing `Action.hash` and `Action_node.make` logic is the right starting
point. It already forms a Merkle-style action graph.

But the final design should preserve ordering when order is semantically
meaningful, especially for:

- link object order
- library order
- command argument order

Only order-insensitive sets should be normalized before hashing.

## 3. Store changes

The store becomes the durable home of action artifacts.

### 3.1 Action artifact contract

Each stored action artifact should contain:

- the action hash
- the relative output paths produced by that action
- per-file hashes and sizes
- enough metadata to materialize the artifact into another directory

The current generic manifest format is close to this already.

### 3.2 Atomic writes

The store should grow an atomic `put_if_absent`-style write path:

1. write into a temporary directory
2. write the manifest there
3. rename the temp directory into the final hash directory
4. if another writer already won the race, discard the temp directory and use
   the existing entry

Readers must never treat "directory exists" as sufficient proof that the entry
is complete.

### 3.3 Recursive materialization

Store promotion/materialization should preserve relative paths recursively.

That means:

- nested output paths must be representable in the manifest
- materialization must create parent directories
- action artifacts and package export manifests can reuse the same primitive

### 3.4 Package export manifests

Package-level artifact lookup is still useful for:

- `tusk run`
- `FindArtifact`
- build summaries

But these should no longer be package cache entries that duplicate all package
outputs under a separate hash. Instead, the store should optionally record a
package export manifest that maps:

- package name
- profile
- target

to:

- exported output names
- the action hashes that produced them

This keeps package lookup without making package caching the primary execution
model.

## 4. Executor changes

The executor should own one readiness scheduler for the whole workspace build.

### 4.1 Replace nested schedulers with one workspace executor

The current steady-state path is:

- package queue in `coordinator2.ml`
- per-package execution in `package_builder.ml`
- per-package action queue in `parallel_action_executor.ml`

The target architecture is one `Workspace_executor` that operates on planned
workspace actions and package export boundaries.

This single executor owns:

- one ready queue
- one completed set
- one dependency-failure propagation rule
- one concurrency budget

### 4.2 Action dispatch algorithm

For each ready action node:

1. compute or read its hash from the plan
2. check the store
3. on cache hit:
   materialize the artifact into the package scratch directory and mark the
   action completed as cached
4. on cache miss:
   execute the action in the package scratch directory, verify outputs, save the
   artifact to the store, and mark it completed as fresh

If an action dependency failed, mark the dependent action as skipped.

### 4.3 Package lifecycle becomes derived state

Package-level events should be derived from action execution:

- `BuildStarted` when the package first becomes active
- `CompilationStarted` when the first uncached action in that package starts
- `BuildCompleted` when all exported outputs for that package are available
- `BuildSkipped` or `BuildFailed` when export completion becomes impossible

This removes the need for package execution to be its own inner state machine.

### 4.4 One concurrency budget

The executor should use one concurrency budget derived from
`Build_ctx.available_parallelism`.

This budget should be shared across:

- actions from different packages
- actions within one package

The machine should not be oversubscribed by nested schedulers each believing
they own full parallelism.

## 5. Streaming and protocol

The transport shape can stay as it is.

`tusk-cli`, `local_session.ml`, `tusk-server`, and the one-shot local session
model remain valid.

The required changes are:

- thread the real `session_id` into action execution
- emit action events with that real session id
- keep the current `BuildStarted`, `BuildEvent`, `BuildCompleted`, and
  `BuildFailed` protocol messages

`BuildStats` should be extended to report both:

- package counts
- action cache hits and misses

The default CLI formatter should remain package-oriented. This RFD does not
require noisy per-action printing in the common path.

## 6. Scratch directories and materialization

Same-package action dependencies still need a local working area. The proposal
keeps package scratch directories, but changes what they contain.

The target shape is:

- concrete source files are read from the workspace directly
- generated files and compiled outputs live in the package scratch directory
- cached action artifacts are materialized into the scratch directory on hit
- dependent packages read exported artifacts from immutable store paths

This means the scratch directory stops being a full package mirror.

That allows two simplifications:

- remove "copy the whole package inputs into sandbox" behavior
- remove per-action source copying for concrete source files

The scratch directory becomes a place for generated and built outputs, not a
shadow copy of the source tree.

## 7. Migration plan

This RFD is intentionally staged. Action-level caching should land before the
largest executor simplification, but the stages should point at the same final
architecture.

### Stage 1: Make action caching real

Implement action cache lookup and save inside the action executor using the
existing action hashes.

This stage should include:

- real `session_id` threading for action events
- atomic store writes
- recursive artifact materialization
- action cache hit and miss telemetry

The existing package queue may remain temporarily.

### Stage 2: Delete package cache from the hot path

Remove:

- planner-time package cache lookup
- package-builder package artifact save as the primary cache path

Add:

- package export manifests built from action results
- planner dependency summaries that no longer require built artifacts

At the end of this stage, packages are no longer the primary cache entry.

### Stage 3: Collapse execution into one workspace action scheduler

Replace:

- `Coordinator2`
- the package-level readiness handoff in `Package_builder`
- the per-package nested action executor ownership of full parallelism

with:

- one workspace action executor

At this point, `Build_queue` and most package-level execution state should be
deletable.

### Stage 4: Add plan memoization only if needed

Always-planning is the intentional simplification. If no-op planning cost later
proves too high, a separate package-plan cache can be added.

That should be a follow-up optimization, not a reason to preserve the current
package cache architecture.

## Drawbacks
[drawbacks]: #drawbacks

- No-op builds will pay full planning cost until a plan cache exists.
- The cache will contain many more entries than a package-only cache and will
  need garbage collection strategy.
- The migration touches planner, executor, and store contracts at once.
- Package export manifests add a second layer of metadata, even though they are
  much smaller than duplicating package artifacts.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design is the best simplification because it picks one primary execution
unit and one primary cache unit.

Alternatives considered:

- Keep package caching and add action caching underneath it.
  This preserves two invalidation models and two places where cache decisions
  happen. It is not a simplification.

- Keep the current nested package scheduler and just cap concurrency harder.
  This reduces oversubscription symptoms, but not the duplicated readiness
  logic.

- Force contributors to split packages earlier.
  That treats a build-system limitation as a package-design rule. It is the
  wrong trade.

- Keep the package planning fast path and bolt action caching onto misses only.
  This keeps planning entangled with store state and prevents the planner from
  being a pure graph builder.

If this RFD is not implemented, `tusk` will keep carrying both package-level and
action-level execution concepts, while only one of them is actually used for
durable caching.

## Prior art
[prior-art]: #prior-art

The strongest prior art here is already inside the repository:

- `Action_node.make` already computes Merkle-style hashes.
- `tusk-executor/README.md` already sketches an action-graph-first executor.
- `RFD0001` already simplified `tusk` into a one-shot local tool.
- `RFD0003` documents the current package-cache-oriented steady state that this
  RFD now proposes to evolve.

More generally, build systems that scale well tend to make the command or action
the cache unit and treat package or target outputs as a higher-level view over
those results. `tusk` already has the right raw pieces for that direction.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Do we need a package-plan cache immediately, or is always-planning acceptable
  for the first rollout?
- Should package export manifests live inside `tusk-store`, inside `out/`, or
  both?
- What cache-retention and garbage-collection policy should exist once action
  artifacts become much more numerous?
- Should action-level output remain silent by default in the CLI, or should
  there be an opt-in verbose mode in the same rollout?

## Future possibilities
[future-possibilities]: #future-possibilities

Once action-level caching and a unified executor exist, several follow-ups get
simpler:

- remote cache support
- remote execution
- watch mode with true incremental rebuilds
- package-plan memoization
- richer progress UIs that consume streamed action events

The main value of this RFD is not only faster large-package rebuilds. It is
that it gives `tusk` one coherent build model instead of a package model on top
of an unused action model.
