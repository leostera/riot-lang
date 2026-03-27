# RFD0014 - Tusk Dependency Classes

- Feature Name: `tusk_dependency_classes`
- Start Date: `2026-03-20`
- Status: `implemented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD adds first-class dependency classes to `tusk` manifests:

- `[dependencies]`
- `[dev-dependencies]`
- `[build-dependencies]`

But the important requirement is not just new manifest sections. `tusk` must
also treat those classes as distinct dependency graphs:

- a build graph
- a runtime graph
- a dev graph

Those graphs should be selected by command target, not collapsed into one
package graph with labels.

That distinction is required so cases like this are legal:

- `propane` runtime-depends on `std`
- `std` dev-depends on `propane`

That must not be rejected as a normal build cycle, because `std` does not need
`propane` to build its checked-in runtime artifact. It only needs `propane`
when building tests or other dev-only targets.

## Motivation
[motivation]: #motivation

There are two concrete pressures on the build model now.

The first is package-provided `tusk-fix` rules.

`std` wants to own `std:no-stdlib`, and provider authoring wants shared types
from `fixme`. But `fixme` depends on `syn`, `syn` depends on
`ceibo`, and `ceibo` depends on `std`. If `std` models `fixme` as a
normal dependency, the normal package graph gets a false cycle:

- `std -> fixme -> syn -> ceibo -> std`

That dependency is real for build-time tooling, but not for the runtime `std`
library artifact.

The second is test tooling.

`propane` is a real package dependency for tests, and `std` should be able to
use it in its own tests. At the same time, `propane` quite reasonably has a
normal dependency on `std`.

So we need all of the following to be expressible at once:

- runtime dependencies needed by checked-in package artifacts
- dev dependencies needed by tests/examples/bench/dev-only outputs
- build dependencies needed only by generators, providers, macros, and other
  tooling

Without separate dependency graphs, the package model remains too coarse and
produces false cycles.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Packages can declare three dependency sections:

```toml
[dependencies]
std = { path = "../std" }

[dev-dependencies]
propane = { path = "../propane" }

[build-dependencies]
fixme = { path = "../fixme" }
```

The intended meaning is:

- `build-dependencies`: required only for build-time tools, codegen, fused
  providers, and future `build.ml` hooks
- `dependencies`: required for runtime package artifacts
- `dev-dependencies`: required only for tests, examples, benchmarks, and other
  dev-only package outputs

The critical semantic rule is:

- every package has three build phases:
  - `pkg.build`
  - `pkg.runtime`
  - `pkg.dev`
- `pkg.runtime` implicitly depends on `pkg.build`
- `pkg.dev` implicitly depends on `pkg.runtime`
- `pkg.dev` should reuse the package's runtime artifact instead of rebuilding
  the package library in a different interface universe
- `tusk build`, `tusk install`, and `tusk run` target the `Runtime` phase
- `tusk test` and `tusk bench` target the `Dev` phase
- generated tooling such as fused `tusk-fix` uses the `Build` phase when
  resolving package-owned tooling code

That means this should be valid:

```text
propane -(runtime)-> std
std -(dev)-> propane
```

because there is no runtime cycle, only a dev-only edge back into `propane`.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Manifest shape

Package manifests support:

```toml
[dependencies]
foo = { path = "../foo" }

[dev-dependencies]
bar = { path = "../bar" }

[build-dependencies]
baz = { path = "../baz" }
```

Workspace manifests support the same three sections. A package dependency entry
with `{ workspace = true }` resolves against the matching workspace section:

- package `[dependencies]` resolves against workspace `[dependencies]`
- package `[dev-dependencies]` resolves against workspace `[dev-dependencies]`
- package `[build-dependencies]` resolves against workspace `[build-dependencies]`

## 2. Package model

`tusk-model` should represent all three dependency classes explicitly:

- `dependencies`
- `dev_dependencies`
- `build_dependencies`

It should also expose helpers for the three projections:

- `runtime_dependencies`
- `dev_dependencies_for_target = dependencies @ dev_dependencies`
- `build_tool_dependencies = build_dependencies`

and, when needed for metadata traversal:

- `all_dependencies = dependencies @ dev_dependencies @ build_dependencies`

The important part is that these are no longer treated as one implicit graph
with three labels attached. The planner must choose a scope explicitly.

## 3. Package phases

The planner should model three package phases:

- `Build`
- `Runtime`
- `Dev`

Each phase has distinct semantics:

- `pkg.build`
  - represents build-time preparation for the package
  - should consume `build_dependencies` when the selected target graph is
    `Build`
  - does not implicitly depend on `pkg.runtime`
- `pkg.runtime`
  - consumes `dependencies`
  - implicitly depends on `pkg.build`
- `pkg.dev`
  - consumes `dev_dependencies`
  - implicitly depends on `pkg.runtime`
  - should reuse `pkg.runtime` artifacts for the package library instead of
    recompiling that library as part of `pkg.dev`

This gives every package a phase chain:

```text
pkg.build -> pkg.runtime -> pkg.dev
```

The important consequence is that `pkg.build` happens first, even when the
user only asks for a runtime or dev target.

`pkg.build` should be treated as a build-tooling phase, not yet as a fully
separate package source surface. In particular:

- `pkg.build` is where build-only dependencies become available
- `pkg.runtime` must wait for `pkg.build`
- `pkg.build` should not yet produce a normal library artifact that other
  package phases try to link against
- `pkg.dev` should add dev-only sources and targets on top of `pkg.runtime`
  rather than rebuilding the runtime package library

That distinction should matter for planning: the runtime phase of a package
should depend on the *completion* of its own build phase, but should not treat
that build phase as a normal library dependency in its depset.

## 4. Graph semantics

The package graph is no longer a single node per package.

Instead:

- build planning selects `pkg.build`
- runtime planning selects `pkg.runtime`
- dev planning selects `pkg.dev`

Dependency edges are phase-aware:

- `pkg.build -> dep.runtime` for build-only relationships when the selected
  target graph is `Build`
- `pkg.runtime -> dep.runtime` for normal package dependencies
- `pkg.dev -> dep.runtime` for dev-only dependencies
- implicit phase edges:
  - `pkg.runtime -> pkg.build`
  - `pkg.dev -> pkg.runtime`

The important implementation detail should be that the implicit self-edge
`pkg.runtime -> pkg.build` is an ordering edge, not a normal library
dependency. It guarantees build tools run first, but the runtime depset should
not try to resolve `.cmxa`/library artifacts out of its own build phase.

Likewise, when the selected target graph is `Build`, build dependencies should
resolve to dependency runtime artifacts, not dependency build phases. That
matches the real use cases we are solving:

- `std.build` needs the compiled runtime artifact of `fixme`
- fused tooling and future build hooks need access to package code built for
  use as tools
- they should not yet consume a separate `dep.build` artifact surface

In the `Runtime` and `Dev` graphs, `pkg.build` should still exist as an
ordering prerequisite for the local package, but those graphs should not
recursively traverse `build_dependencies`. That keeps:

- `pkg.build -> pkg.runtime` ordering intact
- build-time tooling out of normal runtime and dev package products
- false cycles like `std.runtime -> std.build -> fixme.runtime -> ...`
  out of the runtime graph

Cycle detection must happen inside the selected graph, not across all declared
dependency classes at once.

That is what makes the `std` / `propane` case work correctly:

- `propane.runtime -> std.runtime`
- `std.dev -> propane.runtime`
- `std.runtime -> std.build`
- `std.dev -> std.runtime`
- no false cycle between `std.runtime` and `propane.runtime`

In other words: a dev-target build may legitimately depend on a package whose
runtime artifact depends back on the current package. What matters is whether
the specific target graph being built is valid, not whether the union of all
dependency classes is acyclic.

## 5. Command mapping

The command-to-phase mapping should be:

- `tusk build` -> `Runtime`
- `tusk install` -> `Runtime`
- `tusk run <binary>` -> `Runtime`
- `tusk test` -> `Dev`
- `tusk bench` -> `Dev`
- generated tool flows such as fused `tusk-fix` -> `Build`

This is enough to unlock the main real-world cases without requiring a richer
target system up front.

## 6. Execution model

The executor should schedule scoped package nodes, not plain package names.

That means the queue works over nodes like:

- `kernel.build`
- `kernel.runtime`
- `std.build`
- `std.runtime`
- `std.dev`

rather than over plain package names.

The scheduling model should be:

1. construct the scoped package graph for the requested command scope
2. enqueue all reachable scoped nodes
3. repeatedly pick any node whose dependencies are already completed
4. if a node is not ready yet, postpone it and retry later
5. mark completion by scoped package key, not by plain package name

This preserves the existing “try work, postpone blocked nodes, retry after
more completions” behavior, but makes it phase-aware.

### 6.1 Runtime build example

For:

```text
syn -(runtime)-> ceibo -(runtime)-> std -(runtime)-> { kernel, miniriot }
```

a `tusk build syn` graph should look conceptually like:

```text
kernel.build   -> kernel.runtime
miniriot.build -> miniriot.runtime -> kernel.runtime
std.build      -> std.runtime      -> { kernel.runtime, miniriot.runtime }
ceibo.build    -> ceibo.runtime    -> std.runtime
syn.build      -> syn.runtime      -> ceibo.runtime
```

The important thing is that each runtime node depends on:

- its own build phase
- the runtime phases of its declared dependencies

So the executor is free to start any `*.build` node that has no unsatisfied
ordering constraints in the selected graph, and runtime nodes unlock naturally
as those finish.

### 6.2 Dev build example

For:

```text
propane -(runtime)-> std
std -(dev)-> propane
```

a `tusk test std:...` graph should look conceptually like:

```text
std.build      -> std.runtime -> std.dev
propane.build  -> propane.runtime

propane.runtime -> std.runtime
std.dev         -> propane.runtime
```

This is the key case the old single-node package graph could not represent.

There is no runtime cycle:

- `propane.runtime -> std.runtime`

and there is no invalid build cycle:

- `std.dev -> propane.runtime`

because `std.dev` is a distinct node from `std.runtime`.

### 6.3 Ordering vs artifact dependencies

One subtle but important rule is that not every edge means “treat this as a
normal library dependency.”

Specifically:

- `pkg.runtime -> pkg.build` is an ordering edge
- `pkg.dev -> pkg.runtime` is both an ordering edge and a target relationship

The executor should wait for those dependencies to complete, but the planner
should not automatically treat `pkg.build` as a library artifact that
`pkg.runtime` links against.

In practice this means:

- self build-phase edges gate scheduling
- runtime depsets still contain only real package dependency artifacts
- build phases can stay mostly invisible to end users even though they are
  first-class scheduling nodes

This distinction is what keeps the scoped graph honest without accidentally
inventing fake “build artifact libraries” for every package.

## 7. Workspace loading vs graph construction

Workspace loading should still be able to discover packages referenced through
any dependency class so tooling can see the full package set.

That means:

- package discovery may traverse all declared dependency classes
- graph construction must remain scope-specific

This cleanly separates:

- “what packages exist in the workspace universe?”
- from
- “what packages are needed for this specific build target?”

## 8. Immediate use cases

This model is required for both of these:

### 8.1 `std` build-only rule authoring

```toml
[build-dependencies]
fixme = { path = "../fixme" }
```

This puts `fixme` on `std.build`, not on `std.runtime`, assuming the
provider implementation itself also lives outside `src/` in a build-only
location like `fix/`.

### 8.2 `std` test-time dependency on `propane`

```toml
[dev-dependencies]
propane = { path = "../propane" }
```

This lets `std` use `propane` in tests without pretending `propane` is part of
the `std` runtime artifact graph.

## 9. Design constraints

The implementation should preserve these constraints:

- runtime commands should not pull build-only dependency closures into the
  runtime graph
- dev commands should not rebuild the package runtime library under a different
  dependency universe
- build-only tooling flows should resolve against `pkg.build`
- cycle detection should run against the selected scoped graph, not the union
  of all dependency classes

## Drawbacks
[drawbacks]: #drawbacks

- planner and executor code must choose scopes explicitly instead of assuming
  one graph
- some commands will eventually need sharper target semantics than “runtime” vs
  “dev”
- debugging dependency issues becomes slightly more subtle because users need to
  know which graph a command is using

These are acceptable costs because the alternative is a package model that
cannot represent real Riot workflows cleanly.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not keep one package graph and just mark dependency classes on edges

Because that still encourages cycle detection and planning against the union of
all edges, and it still leaves one package node trying to represent three
different build products.

The requirement is not just richer metadata. The requirement is distinct phase
selection by build target.

The bootstrap work that motivated this design also exposes a second issue with
the single-node model: ordering-only relationships and artifact dependencies
get conflated too easily. Representing `pkg.build`, `pkg.runtime`, and
`pkg.dev` as distinct nodes makes it much clearer which edges are:

- pure scheduling constraints
- library/runtime artifact dependencies
- dev-only target relationships

### Why not make build-dependencies a tusk-fix-specific escape hatch

Because the pressure is broader than `tusk-fix`. Macros, codegen, generators,
future package commands, and other build-time systems want the same separation.

### Why not exclude dev-dependencies from all graphs

Because then they are not useful for test/example/bench targets, which defeats
the purpose of declaring them in the first place.

## Prior art
[prior-art]: #prior-art

The split mirrors the familiar model used by other build tools:

- runtime dependencies
- development/test dependencies
- build-time/tooling dependencies

What is slightly stricter here is the explicit statement that these must map to
different dependency graphs, not just different TOML sections.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should package commands eventually declare which package phase they consume?
- Should `tusk run` ever gain a way to opt into dev scope for explicitly
  dev-only binaries?
- How far should Riot go before it needs a richer target-specific planning
  model beyond `Build`, `Runtime`, and `Dev`?
