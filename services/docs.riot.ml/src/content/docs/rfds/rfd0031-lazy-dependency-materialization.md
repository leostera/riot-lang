---
title: "RFD0031 - Lazy Dependency Materialization in the Build Graph"
description: "Riot Request for Discussion · rejected"
---

> Canonical source: `docs/rfds/RFD0031-lazy-dependency-materialization.md`

> Status: **Rejected**

- Feature Name: `riot_lazy_dependency_materialization`
- Start Date: `2026-04-03`
- Status: `rejected`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes making external package materialization an explicit build
prerequisite instead of a pre-build/package-preparation concern.

In that model, a resolved external package would behave like any other package
node in the package graph, except that its first prerequisite would be an
explicit action such as:

- `EnsurePackageMaterialized`

That action would ensure the exact resolved package exists in the shared
registry cache under `~/.riot/registry/...` before normal package planning and
compilation proceed.

This document records the design, but rejects it for Riot's current build
system. Riot will keep materialization outside the planner/executor action graph
because that keeps the build runtime simpler, more deterministic, and easier to
operate.

## Motivation
[motivation]: #motivation

There is an appealing architectural idea behind lazy dependency materialization:

- resolved external packages would become explicit build prerequisites
- the executor could satisfy heterogeneous actions, not only compile/link work
- package download/materialization would become visible in the build graph
- future build pipelining could overlap planning with prerequisite repair

That model is conceptually cleaner than hiding package repair inside dependency
projection or other pre-build setup stages.

It also makes a tempting future story possible:

1. resolve the lockfile
2. project exact package metadata
3. submit build prerequisites as actions
4. let the executor satisfy them while planning continues elsewhere

The question this RFD answers is not whether that model is elegant. It is.
The real question is whether Riot should adopt it now.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

### The proposed model

Under the proposed design, a registry package such as `gooey@1.2.3` would be
treated as a normal package in the dependency graph, except that it would first
need to be materialized into the shared registry source cache:

```text
~/.riot/registry/pkgs.ml/src/gooey/1.2.3/...
```

Its flow would look like:

1. `riot-deps` resolves `gooey@1.2.3`
2. the package graph contains `gooey` as a dependency package node
3. planner/executor sees that its source root is missing
4. executor runs `EnsurePackageMaterialized(gooey@1.2.3)`
5. normal package planning/build then proceeds

Dependents such as `minttea` would stay pending until `gooey` had advanced far
enough to be planned and built.

This is attractive because it makes package availability an explicit build
dependency instead of hidden setup work.

### The chosen model

Riot intentionally does **not** adopt that design right now.

Instead, Riot keeps external package materialization outside the normal build
action graph:

- exact versions are resolved first
- external package metadata is projected into build-ready package structures
- missing package roots are repaired before normal package build planning
  proceeds

That means the planner/executor can remain focused on:

- package planning
- action scheduling
- compile/link execution
- artifact caching

without also becoming a general dependency bootstrapping runtime.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

### Proposed design

The proposed design has three main pieces:

1. projection must be able to construct external package metadata without
   requiring a fully materialized source tree
2. planner must represent external package materialization as an explicit
   prerequisite
3. executor must run a new exact materialization action such as
   `EnsurePackageMaterialized`

The ideal flow would be:

1. resolve `riot.lock`
2. load external package manifest metadata from registry metadata/cache
3. create resolved package nodes with expected `materialized_root`
4. when that root is missing, emit `EnsurePackageMaterialized`
5. only after it succeeds, plan and compile the package normally

That design is structurally honest, but it increases complexity in several
layers at once:

- `pkgs-ml` needs manifest-level fetch/cache support separate from source
  archive extraction
- `riot-deps` projection must split metadata loading from source materialization
- `riot-planner` must gain a new blocked/prelude state
- `riot-executor` must learn a new class of non-compiler action
- the coordinator must retry planning on dependency materialization state
  changes

### Rejected implementation direction

Riot rejects moving dependency materialization into the action graph for the
current system.

The current/preferred model is:

1. dependency resolution establishes exact external package identities
2. external package availability is repaired before normal build planning for
   that package continues
3. the build runtime only executes normal build actions for already-available
   package roots

This keeps the build pipeline simpler:

- the planner does not need to represent "package exists logically but not yet
  physically"
- the executor does not need to coordinate download/materialization actions with
  compile actions
- package planning remains a deterministic function of package inputs and
  available dependency artifacts

Most importantly, it keeps the build logic dumb.

That is a feature, not a limitation.

## Drawbacks
[drawbacks]: #drawbacks

Rejecting this design means Riot gives up some potential future benefits:

- less overlap between package planning and dependency repair
- less explicit representation of external package availability inside the build
  graph
- a weaker path toward a single general-purpose executor for all prerequisites

It also means some dependency preparation remains outside the most visible part
of the build graph.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why reject it?

Because the operational cost is too high for the current system.

The proposed design is architecturally elegant, but the present Riot build
system benefits more from keeping dependency preparation outside the normal
action runtime:

- the control flow is simpler
- failure modes are easier to reason about
- the build coordinator stays smaller
- build behavior remains deterministic and easier to debug

In other words:

- explicit materialization actions are nicer conceptually
- pre-build materialization is nicer operationally

Riot chooses the operationally simpler model for now.

### Alternative: eager materialize the entire resolved graph

Rejected as the primary wording for the current implementation.

Riot does not need to eagerly materialize every external package before doing
any useful work. The chosen model can still repair external package roots lazily
before package planning/build preparation, without pushing that work into the
normal action graph.

### Alternative: treat remote packages as action-only packages

Rejected.

External packages still need normal package planning and build actions after
their sources exist. A materialization action is only a prerequisite, not a
replacement for compiling and caching those packages.

## Prior art
[prior-art]: #prior-art

- Bazel-style repository rules and fetch phases
  - These systems make external dependency acquisition explicit and schedulable.
  - That is architecturally clean, but it also pushes more lifecycle management
    into the build runtime.
- Nix and content-addressed fetchers
  - These systems model acquisition as part of the derivation graph, which is
    powerful but significantly more complex than Riot's current needs.
- Riot's current package-management model
  - Riot already has a shared registry cache and exact lockfile identities.
  - The missing question was only whether materialization itself should move
    into the executor. This RFD says no.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- If Riot later grows a more incremental or long-lived executor, should this
  decision be revisited?
- If external package metadata becomes independently cached from extracted
  source trees, does that change the complexity tradeoff enough to reconsider?

## Future possibilities
[future-possibilities]: #future-possibilities

If Riot later adopts a more always-on or more heterogeneous executor, the
proposed `EnsurePackageMaterialized` design can be revived.

That future design should preserve one rule from this RFD:

- the shared registry cache under `~/.riot/registry/...` remains the source of
  truth for downloaded/materialized package sources

so external packages are still reused across workspaces on the same machine.
