# Tusk Specs

This directory contains readable TLA+ models for the current `tusk` build
system design.

The intent is the same as `specs/miniriot`, but for build semantics instead of
runtime semantics: keep the model small, extracted from the current code, and
use it to expose design mismatches before they become harder-to-debug build
bugs.

## Layout

- `ActionCache.tla`: a PlusCal slice of the action-level cache contract for
  `BuildForeignDependency` actions.
- `ActionCache.cfg`: a passing smoke config for the action-cache slice.
- `ActionCacheCommandOrderBug.cfg`: a failing config showing that the current
  action hash can treat different command orders as the same cache key.
- `ModuleGraphStructure.tla`: a PlusCal slice of the pre-`ocamldep` structural
  module-graph builder rules for library scanning and synthetic interface
  creation.
- `ModuleGraphStructure.cfg`: a passing smoke config for the structural
  module-graph slice.
- `ModuleGraphWiring.tla`: a PlusCal slice of the post-`ocamldep` dependency
  wiring rules that apply registry lookups, self-edge filtering, and
  `MLI -> ML` filtering.
- `ModuleGraphWiring.cfg`: a passing smoke config for the wiring slice.
- `ModuleGraphWiringPreferenceBug.cfg`: a failing config showing that the
  current wiring rules do not prefer implementation nodes when both interface
  and implementation exist.
- `ModuleGraphNamespaceResolution.tla`: a PlusCal slice of the nested-namespace
  dependency reconstruction path before alias handling.
- `ModuleGraphNamespaceResolution.cfg`: a passing smoke config for the
  namespace-resolution slice.
- `ModuleGraphNamespaceResolutionBug.cfg`: a failing config showing that the
  current lookup ignores the reconstructed namespace and wires every
  same-simple-name candidate.
- `ModuleGraphAliasResolution.tla`: a PlusCal slice of the alias-context
  dependency lookup gap between scan-time `open_modules` and wire-time edge
  resolution.
- `ModuleGraphAliasResolution.cfg`: a passing smoke config for the
  alias-resolution slice.
- `ModuleGraphAliasResolutionBug.cfg`: a failing config showing that the
  current lookup ignores alias-exposed targets and wires every same-simple-name
  candidate.
- `ModuleScannerPipeline.tla`: a PlusCal slice of the scanner + filter path for
  entry tagging, relative paths, pruning, and canonical ordering.
- `ModuleScannerPipeline.cfg`: a passing smoke config for the scanner baseline
  slice.
- `ModuleScannerNativeTagBug.cfg`: a failing config showing that allowed `.c`
  and `.h` files are currently tagged as `Other` and dropped.
- `PlanBundleModuleGraphRoundTrip.tla`: a PlusCal slice of the planner bundle
  module-graph serializer/deserializer fidelity for per-node `open_modules`.
- `PlanBundleModuleGraphRoundTrip.cfg`: a passing smoke config for the
  plan-bundle round-trip baseline.
- `PlanBundleModuleGraphOpenModulesBug.cfg`: a failing config showing that the
  current plan-bundle round-trip erases non-empty `open_modules`.
- `PlanBundleVersionGate.tla`: a PlusCal slice of the warm-plan cache
  acceptance gate for persisted plan bundles.
- `PlanBundleVersionGate.cfg`: a passing smoke config for the accepted
  fresh-bundle path.
- `PlanBundleVersionGateStaleVersion.cfg`: a passing config for the stale
  bundle-version rebuild path.
- `PlanBundleToolchainInvalidation.tla`: a PlusCal slice of the planner-cache
  key mismatch between toolchain-insensitive plan bundles and
  toolchain-sensitive action hashes.
- `PlanBundleToolchainInvalidation.cfg`: a passing smoke config for the
  no-toolchain-change baseline.
- `PlanBundleToolchainInvalidationBug.cfg`: a failing config showing that the
  current planner bundle key can survive a toolchain change and restore stale
  action hashes.
- `BugInventory.md`: the running list of bug-shaped properties found by the
  extracted specs. This is the place to accumulate likely bugs before we add
  OCaml regression tests.
- `PropertyInventory.md`: a backlog of current `tusk` semantic properties,
  grouped into candidate TLA+ spec slices.

## Why Start Here

The current cache path is split across a few packages:

- `tusk-planner` computes action hashes.
- `tusk-executor` decides cache hit vs miss and materializes cached outputs.
- `tusk-store` keeps the immutable action artifact store.

That makes action-level caching a good first slice: it is small enough to model
readably, but central enough that a hash-design mistake becomes a real build
correctness bug.

## What `ActionCache.tla` Extracts

This first slice intentionally narrows to `BuildForeignDependency` because
`packages/tusk-planner/src/action.ml` currently normalizes `build_cmd` by
sorting it before hashing. The model keeps:

- `StableKeyFields[a]`: an abstraction of the other hashed fields from the
  action record such as name, path, outputs, and env.
- `BuildCmd[a]`: the ordered command sequence, modeled explicitly because its
  order matters to execution semantics.
- `ActionHash(a)`: the current normalized key shape, modeled as
  `<<StableKeyFields[a], CommandBag(BuildCmd[a])>>`.
- a tiny cache machine with `CacheMiss` and `CacheHit` steps mirroring
  `action_executor.ml`.

The model does not yet cover:

- action dependencies from `action_queue.ml`
- package-level export manifests from `coordinator.ml`
- planner bundle caching from `package_planner.ml`
- concurrent workers

Those are good next slice candidates once this cache-key law is settled.

## What `ModuleGraphStructure.tla` Extracts

This slice intentionally stops before `wire_dependencies`. It extracts the
structural rules currently implemented across
`module_graph.ml` and `library_definition.ml`:

- the library interface file excludes itself from its own child-file set
- binary OCaml sources are filtered out of child files
- same-named files shadow same-named subdirectories
- directories with no OCaml content do not create synthetic alias/interface/
  implementation nodes
- concrete `foo/foo.ml` + `foo/foo.mli` libraries depend only on child files
- generated or partial libraries depend on all child modules
- library implementations always depend on their matching interfaces

The abstraction is intentionally name-based instead of path-based. That keeps
the spec readable, but it also means a shadowed directory and a surviving file
with the same module name collapse to one name in dependency sets. The spec
therefore checks that shadowed directories are filtered out of `ChildDirs`,
not that the shared module name disappears from every dependency set.

## What `ModuleGraphWiring.tla` Extracts

This slice intentionally starts after name resolution. It narrows to the
current `wire_dependencies` loop over resolved dependency names and registry
entries:

- every referenced module name expands to every registered node with that name
- self-edges are skipped
- `MLI -> ML` edges are skipped
- everything else is added as a dependency edge

The bug config then checks the stronger planner law already encoded in
`packages/tusk-planner/tests/dependency_resolution_tests.ml`: when both
`Foo.mli` and `Foo.ml` exist, a downstream reference to `Foo` should prefer the
implementation node. Under the current extracted semantics, an interface source
like `Bar.mli` instead drops the implementation edge and keeps only the
interface edge.

## What `ModuleGraphNamespaceResolution.tla` Extracts

This slice isolates the smaller step just before registry lookup:

- the source file reconstructs a qualified dependency name from its own nested
  namespace
- the current lookup still uses the simple module name
- every registry candidate for that simple name is then wired

The bug config checks the stronger law we actually want: if the source
reconstructs `Pkg__Sub__Foo` and both `Pkg__Foo` and `Pkg__Sub__Foo` exist, the
planner should prefer the `Pkg__Sub__Foo` target instead of wiring both.

## What `ModuleGraphAliasResolution.tla` Extracts

This slice isolates a different ambiguity source than nested directories:

- scan-time graph construction stores alias nodes in `open_modules`
- action generation later turns those alias nodes into `-open` compiler flags
- the current `wire_dependencies` path still ignores alias context entirely

The bug config checks the stronger law inherited from the old `minitusk`
behavior: if alias context exposes `Pkg__Util__Foo` for a dependency named
`Foo`, dependency wiring should prefer that alias-matched target instead of
wiring both `Pkg__Foo` and `Pkg__Util__Foo`.

## What `ModuleScannerPipeline.tla` Extracts

This slice moves one stage earlier than the graph builder:

- raw directory entries are tagged into scanner entry kinds
- stored paths stay relative to the source-directory base
- planner filtering keeps allowed typed entries and prunes empty dirs
- the final entry sequence is emitted in canonical kind-then-name order

The bug config checks one stronger law already suggested by the current type
surface: if `.c` and `.h` are first-class scanner entry kinds, allowed native
source files should survive the pipeline as dedicated `C` / `H` entries instead
of being dropped as `Other`.

## What `PlanBundleModuleGraphRoundTrip.tla` Extracts

This slice moves later in the planner pipeline, into
`packages/tusk-planner/src/package_planner.ml`:

- `module_graph_to_json` serializes the module graph into the persisted plan
  bundle
- `module_graph_of_json` restores it on a warm-plan cache hit
- per-node `open_modules` should survive that round-trip if the bundle is
  meant to restore the same graph rather than only a graph-shaped shell

The bug config checks that stronger fidelity law directly. Under the current
implementation-shaped semantics, every node is serialized with `"opens": []`
and restored with `open_modules = []`, so any non-empty open-module context is
lost across the round-trip.

## What `PlanBundleVersionGate.tla` Extracts

This slice stays in `packages/tusk-planner/src/package_planner.ml`, but narrows
to the acceptance gate on a loaded plan bundle:

- missing bundles rebuild
- decode exceptions rebuild
- wrong bundle versions rebuild
- wrong package identity rebuilds
- module-graph or action-graph parse failures rebuild
- only a fully accepted bundle yields a warm cache hit

So far this slice does not expose a bug. The extracted version gate matches the
intended stale-bundle invalidation behavior.

## What `PlanBundleToolchainInvalidation.tla` Extracts

This slice stays in the same planner-cache area, but isolates a different law:

- `compute_input_hash` decides whether the planner reuses a cached plan bundle
- `Action_node.make` computes per-action hashes that do include toolchain
  identity
- if the planner cache key omits toolchain identity, a warm-plan cache hit can
  restore an action graph whose stored hashes were computed under a different
  toolchain

The bug config checks that stronger law directly. Under the current extracted
semantics, a first plan stores an action hash derived from `toolchain-v1`, a
second plan with `toolchain-v2` computes the same bundle key, and the planner
restores the old `toolchain-v1` action hash.

## How To Work On The Spec

`ActionCache.tla` is written primarily in PlusCal. Treat the PlusCal algorithm
as the source of truth and regenerate the translated TLA whenever the algorithm
changes:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  pcal.trans specs/tusk/ActionCache.tla
```

Then run TLC from the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ActionCache.tla \
  -config specs/tusk/ActionCache.cfg
```

And for the structural module-graph slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphStructure.tla \
  -config specs/tusk/ModuleGraphStructure.cfg
```

And for the dependency-wiring slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphWiring.tla \
  -config specs/tusk/ModuleGraphWiring.cfg
```

And for the namespace-resolution slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphNamespaceResolution.tla \
  -config specs/tusk/ModuleGraphNamespaceResolution.cfg
```

And for the alias-resolution slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphAliasResolution.tla \
  -config specs/tusk/ModuleGraphAliasResolution.cfg
```

And for the scanner pipeline slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleScannerPipeline.tla \
  -config specs/tusk/ModuleScannerPipeline.cfg
```

And for the plan-bundle module-graph round-trip slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleModuleGraphRoundTrip.tla \
  -config specs/tusk/PlanBundleModuleGraphRoundTrip.cfg
```

And for the plan-bundle version-gate slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleVersionGate.tla \
  -config specs/tusk/PlanBundleVersionGate.cfg
```

And for the plan-bundle toolchain-invalidation slice:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleToolchainInvalidation.tla \
  -config specs/tusk/PlanBundleToolchainInvalidation.cfg
```

The bug config is expected to fail under the current implementation-shaped
semantics:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ActionCache.tla \
  -config specs/tusk/ActionCacheCommandOrderBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphWiring.tla \
  -config specs/tusk/ModuleGraphWiringPreferenceBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphNamespaceResolution.tla \
  -config specs/tusk/ModuleGraphNamespaceResolutionBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleGraphAliasResolution.tla \
  -config specs/tusk/ModuleGraphAliasResolutionBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/ModuleScannerPipeline.tla \
  -config specs/tusk/ModuleScannerNativeTagBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleModuleGraphRoundTrip.tla \
  -config specs/tusk/PlanBundleModuleGraphOpenModulesBug.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleVersionGate.tla \
  -config specs/tusk/PlanBundleVersionGateStaleVersion.cfg
```

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/tusk/PlanBundleToolchainInvalidation.tla \
  -config specs/tusk/PlanBundleToolchainInvalidationBug.cfg
```

## Current Findings

`ActionCacheCommandOrderBug.cfg` is designed to expose one concrete design bug:
two `BuildForeignDependency` actions with the same non-command fields and the
same command multiset, but with different command order, can reuse the same
cache entry even though they should produce different fresh results.

`ModuleGraphWiringPreferenceBug.cfg` exposes a second design bug: the current
module-graph wiring rules and the intended "prefer implementation" behavior are
in conflict. A downstream interface reference to `Foo` sees both `Foo.ml` and
`Foo.mli` in the registry, but the current `MLI -> ML` filter removes
`Foo.ml`, leaving only `Foo.mli`. The direct planner test binary reproduces the
same failure:

```sh
_build/debug/aarch64-apple-darwin/out/tusk-planner/dependency_resolution_tests \
  run-tests "module graph prefers implementation when interface exists"
```

`ModuleGraphNamespaceResolutionBug.cfg` exposes a third design bug: the planner
reconstructs a qualified dependency name from the source file's nested
namespace, but the next lookup step still drops back to the simple name. When
both `Pkg__Foo` and `Pkg__Sub__Foo` exist, the current extracted semantics wire
both candidates instead of preferring the source-local namespace target.

`ModuleGraphAliasResolutionBug.cfg` exposes a fourth design bug: alias context
is already present on module nodes and later used for compiler `-open` flags,
but the current dependency-wiring pass ignores it. When alias context says
`Foo` should mean `Pkg__Util__Foo`, the current extracted semantics can still
wire both `Pkg__Foo` and `Pkg__Util__Foo`.

`ModuleScannerNativeTagBug.cfg` exposes a fifth design bug: the scanner type and
filtering path both make room for dedicated `C` and `H` entries, but the
current extracted tagging logic in `scan_directory` classifies every non-OCaml
extension as `Other`. Allowed `.c` and `.h` files are therefore dropped before
the planner can keep them.

`PlanBundleModuleGraphOpenModulesBug.cfg` exposes a sixth design bug: persisted
plan bundles do not round-trip `open_modules`. The current serializer writes an
empty `"opens"` list for every module-graph node, and the current deserializer
restores every node with `open_modules = []`, so a warm-plan cache hit loses
non-empty alias-open context.

`PlanBundleToolchainInvalidationBug.cfg` exposes a seventh design bug: the
planner bundle cache key ignores toolchain identity even though action-node
hashes include it. A warm-plan cache hit can therefore restore stale action
hashes computed under an older toolchain.

## Validation Notes

These configs are safety-only and intentionally tiny.

- `ActionCache.cfg` currently completes with 20 distinct states and no errors.
- `ActionCacheCommandOrderBug.cfg` currently fails with a counterexample where
  `SecondBuild` takes a cache hit and materializes `<<"Prep", "Compile">>`
  instead of its own ordered command result `<<"Compile", "Prep">>`.
- `ModuleGraphStructure.cfg` currently completes with 4,634 distinct states and
  no errors.
- `ModuleGraphWiring.cfg` currently completes with 10 distinct states and no
  errors.
- `ModuleGraphWiringPreferenceBug.cfg` currently fails with a counterexample
  where `BarMLI` processes `FooML` first, drops it because of the current
  `MLI -> ML` filter, then keeps only `FooMLI`.
- `ModuleGraphNamespaceResolution.cfg` currently completes with 6 distinct
  states and no errors.
- `ModuleGraphNamespaceResolutionBug.cfg` currently fails with a
  counterexample where the source reconstructs `Pkg__Sub__Foo` but still wires
  both `Pkg__Foo` and `Pkg__Sub__Foo`.
- `ModuleGraphAliasResolution.cfg` currently completes with 6 distinct states
  and no errors.
- `ModuleGraphAliasResolutionBug.cfg` currently fails with a counterexample
  where alias context points at `Pkg__Util__Foo` but current lookup still wires
  both `Pkg__Foo` and `Pkg__Util__Foo`.
- `ModuleScannerPipeline.cfg` currently completes with 19 distinct states and
  no errors.
- `ModuleScannerNativeTagBug.cfg` currently fails with a counterexample where
  `src/stubs.c` and `src/api.h` are both tagged as `Other` and dropped.
- `PlanBundleModuleGraphRoundTrip.cfg` currently completes with 10 distinct
  states and no errors.
- `PlanBundleModuleGraphOpenModulesBug.cfg` currently fails with a
  counterexample where `Main` starts with non-empty `open_modules` but the
  restored graph gives it `{}`.
- `PlanBundleVersionGate.cfg` currently completes with 9 distinct states and no
  errors.
- `PlanBundleVersionGateStaleVersion.cfg` currently completes with 6 distinct
  states and no errors.
- `PlanBundleToolchainInvalidation.cfg` currently completes with 4 distinct
  states and no errors.
- `PlanBundleToolchainInvalidationBug.cfg` currently fails with a
  counterexample where the planner reuses a bundle stored under
  `toolchain-v1` after a switch to `toolchain-v2`, and restores the old action
  hash instead of replanning or rehashing.
