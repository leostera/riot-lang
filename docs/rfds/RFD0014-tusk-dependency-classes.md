# RFD0014 - Tusk Dependency Classes

- Feature Name: `tusk_dependency_classes`
- Start Date: `2026-03-20`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD adds first-class dependency classes to `tusk` manifests:

- `[dependencies]`
- `[dev-dependencies]`
- `[build-dependencies]`

The goal is to let packages express intent clearly and to keep build graphs
honest. In particular, build-only authoring dependencies such as
`std -> tusk-fix-api` should not contaminate the normal package build graph and
should not participate in cycle detection for checked-in artifacts.

## Motivation
[motivation]: #motivation

The immediate motivation is `tusk-fix` package-provided rules.

`std` wants to own `std:no-stdlib`, and the provider source wants shared
rule-authoring types from `tusk-fix-api`. But `tusk-fix-api` depends on `syn`,
`syn` depends on `ceibo`, and `ceibo` depends on `std`. If `std` models
`tusk-fix-api` as a normal dependency, the normal package graph gets a cycle:

- `std -> tusk-fix-api -> syn -> ceibo -> std`

That dependency is real for build-time tooling, but not for the normal `std`
library artifact.

More broadly, Riot needs to distinguish:

- dependencies needed by checked-in package artifacts
- dependencies needed by tests/examples/bench binaries
- dependencies needed only by build-time generators, providers, or tooling

Without that split, the package graph is too coarse.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Packages can now declare three dependency sections:

```toml
[dependencies]
std = { path = "../std" }

[dev-dependencies]
propane = { path = "../propane" }

[build-dependencies]
tusk-fix-api = { path = "../tusk-fix-api" }
```

The intended meaning is:

- `dependencies`: required for normal package artifacts
- `dev-dependencies`: required for tests/examples/bench and other dev-only
  package outputs
- `build-dependencies`: required only by build-time tooling and generated
  workflows, not by the package's checked-in runtime artifacts

Workspace manifests may also define the same sections so package manifests can
continue using `{ workspace = true }` in the matching dependency class.

For normal package builds:

- `dependencies` and `dev-dependencies` participate in the package build graph
- `build-dependencies` do not

That gives the current repo the behavior it wants:

- tests and examples can pull extra packages without pretending they are runtime
  dependencies
- build-only authoring helpers do not create false cycles in normal package
  builds

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

It should also expose helpers for:

- `build_graph_dependencies = dependencies @ dev_dependencies`
- `all_dependencies = dependencies @ dev_dependencies @ build_dependencies`

That keeps the core type honest while still giving planner code a clear
“dependencies that matter for normal package builds” surface.

## 3. Planner behavior

For the current implementation:

- package graph edges use `build_graph_dependencies`
- dependency satisfaction checks use `build_graph_dependencies`
- package hash and workspace-specific dependency hashing use
  `build_graph_dependencies`
- stdlib/unix/dynlink checks use `build_graph_dependencies`

This matches current `tusk` behavior, where tests/examples/bench binaries are
autodiscovered and built as part of the package build plan.

`build_dependencies` are intentionally excluded from that graph.

## 4. Workspace loading

Workspace loading should still be able to discover packages referenced through
any dependency class so workspace-level tooling can see the full package set.

That means:

- package loading may traverse all declared dependency classes
- package graph construction remains selective and uses only
  `build_graph_dependencies`

This separates “discover package metadata” from “create the normal build graph”.

## 5. Why dev dependencies participate in normal package builds

Today, `tusk` autodiscovers:

- test binaries
- example binaries
- benchmark binaries

and plans them as part of the package.

So, with the current planner architecture, `dev-dependencies` must participate
in the package build graph or those binaries would fail to link.

That is a reasonable v1 interpretation:

- `dependencies`: library/runtime
- `dev-dependencies`: package-local dev outputs
- `build-dependencies`: tool-only

If Riot later introduces more target-specific planning, the semantics can be
refined further. This RFD does not require that refactor.

## 6. Immediate use case

The immediate manifest change enabled by this RFD is:

```toml
[build-dependencies]
tusk-fix-api = { path = "../tusk-fix-api" }
```

for packages like `std` that own build-time rule providers but must not acquire
normal runtime edges to `tusk-fix-api`.

## Drawbacks
[drawbacks]: #drawbacks

- the package model grows more explicit and slightly more verbose
- planner code must choose the correct dependency projection instead of blindly
  using one list
- some future tooling may want even finer distinctions than the three classes
  here

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why not keep one dependency list

Because it forces build-time and runtime concerns into the same graph and causes
false cycles like the `std -> tusk-fix-api` case.

### Why not make build-dependencies a tusk-fix-specific feature

Because the pressure is broader than `tusk-fix`. Build-time generators, macro
tooling, codegen, and future package commands all want the same separation.

### Why not exclude dev-dependencies from the build graph too

Because the current package planner builds dev outputs as binaries. Excluding
them now would make declared dev dependencies largely useless without a larger
planner refactor.

## Prior art
[prior-art]: #prior-art

The split mirrors the familiar shape used by other build tools:

- runtime dependencies
- development/test dependencies
- build-time/tooling dependencies

The names chosen here intentionally match that mental model.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should future `tusk` target planning distinguish library builds from
  test/example/bench builds more sharply, so `dev-dependencies` can be excluded
  from some build commands?
- Should package commands eventually declare whether they consume normal,
  dev-only, or build-only dependency closures?
