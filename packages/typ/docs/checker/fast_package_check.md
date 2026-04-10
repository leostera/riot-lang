# Fast Package Check

This document specifies the package-check architecture `typ` should grow
toward for cold build checks.

The goal is simple:

- `riot check` should use one authoritative typing flow
- that flow should be incremental by module group
- it should persist reusable module typings as soon as they are authoritative
- it should not pay query or editor costs on the plain check path

This is the architecture that has a real chance of taking
`riot run riot -- check -p kernel-new` from the current multi-second prototype
range down toward the low hundreds of milliseconds and below.

## 1. Problem

The current implementation has two package-check orchestration paths:

- rooted snapshot forcing
- package-scan incremental analysis

Those two paths share the same core checker, but they do not share the same
engine semantics.

That causes two different classes of problems:

- performance waste
- semantic drift

Performance waste comes from:

- preparing rooted snapshots and then asking them to re-enumerate module
  typings as a cold package-check boundary
- keeping query-facing payload attached to plain check analyses
- eager end-of-run persistence and reload work
- fallback public-module recovery passes

Semantic drift comes from:

- separate ambient/module-surface logic in the incremental path
- separate public-module shaping logic in the rooted path
- different places deciding what a local alias or public module view means

The result is bad in both directions:

- the rooted path is more trusted but too expensive
- the incremental path is closer to the right performance shape but easier to
  get wrong

That is not a stable architecture.

## 2. Target Shape

`typ` should expose one authoritative package-check engine for build-style
checking.

That engine should:

1. accept already planned and ordered module groups
2. process those groups in dependency order
3. produce authoritative `ModuleTypings` once per finished module
4. persist those typings immediately
5. keep those same typings loaded in memory for downstream modules
6. emit checked-file payload for each source as it finishes

This means the hot check path is:

```text
plan package
-> order module groups
-> for each module group:
     build ambient from already-finished authoritative module typings
     analyze sources in the group
     pair interface/implementation once
     persist authoritative module typings immediately
     add them to the in-memory loaded-module index
     emit checked-file results for the group
-> done
```

There should not be a second host-side pass that asks for all module typings at
the end and then persists or reloads them again.

## 3. Comparison With OCaml

This target is deliberately close to the compiler architecture in
`vendor/ocaml/typing`.

Relevant OCaml entrypoints:

- [typemod.ml](../../../../vendor/ocaml/typing/typemod.ml#L3094)
  `type_structure`
- [typemod.ml](../../../../vendor/ocaml/typing/typemod.ml#L3278)
  `type_implementation`
- [typemod.ml](../../../../vendor/ocaml/typing/typemod.ml#L3382)
  `save_signature`
- [env.ml](../../../../vendor/ocaml/typing/env.ml#L2656)
  `save_signature`
- [path.ml](../../../../vendor/ocaml/typing/path.ml#L16)
  `Path.t`
- [ident.ml](../../../../vendor/ocaml/typing/ident.ml#L99)
  `Ident.t`

The important OCaml shape is:

- type a structure once
- produce one signature once
- save that signature once
- extend the environment with that saved signature

What OCaml does not do on the hot path is:

- build two different package-check orchestrators
- type modules through one path and then reconstruct authoritative summaries
  through another
- carry editor/query payload through every cold build check

`typ` does not need to be source-identical to OCaml, but it should match the
same architectural spirit:

- one authoritative typing path
- one authoritative reusable module artifact
- one incremental environment growth story

## 4. Authoritative Artifact

`ModuleTypings` is the authoritative reusable artifact for finished modules.

That means:

- finished module groups produce `ModuleTypings`
- downstream modules type against `ModuleTypings`
- persistence happens over `ModuleTypings`
- package bundles are assembled out of already-authoritative `ModuleTypings`

This is the `.cmi`-like boundary for `typ`.

No later phase should need to rebuild the same authority from richer internal
state.

## 5. Check Payload vs Query Payload

The build check path and the query path should not carry the same payload.

For plain package checks, the engine should retain only a minimal check payload:

- parse diagnostics
- lowering diagnostics
- typing diagnostics
- file summary
- authoritative value-definition exports needed for persisted summaries
- minimal checked-file data required by `riot-check`

The query/editor payload should be optional:

- semantic tree
- type index
- item traces
- expression traces
- richer definition provenance helpers

This split is crucial for performance.

The plain `riot check` path should not pay to retain query payload it will not
use.

## 6. Single Surface Logic

One module result can be exposed under multiple visible names:

- internal module name
- local alias
- public package name

Those visible views must all come from one surface-shaping layer.

That means:

- one place decides how exports are qualified
- one place decides how type declarations are qualified
- one place decides how local aliases and public names map to authoritative
  module results

Hosts should not reimplement that shaping logic themselves.

If `riot-check` and `Snapshot` each invent their own public-module surface
rules, they will drift again.

## 7. One Loaded-Module Index

The hot path should use one keyed loaded-module index.

That index should:

- be keyed by authoritative module name
- support cheap insertion as modules finish
- support cheap lookup for ambient construction
- support cheap alias/public views without rebuilding whole lists

The package-check engine should grow this index monotonically as it walks the
module graph.

## 8. Role Of Snapshots

Rooted snapshots still matter, but they should stop being the main cold package
check driver.

Snapshots remain the right tool for:

- editor sessions
- queries over one rooted revision-bound world
- interactive tooling that wants rich semantic payload

But plain package checks should not have to route through snapshot forcing and
snapshot-wide module-typing collection just to get authoritative reusable
module summaries.

So the architecture should be:

- incremental package-check engine for build-style checking
- rooted snapshots for query/editor workflows

These are different consumers of the same checker semantics, not different
semantic engines.

## 9. Proposed Engine API

The exact names can change, but the engine contract should look roughly like:

```text
check_package_incrementally :
  config ->
  ordered_module_groups ->
  on_module_finished ->
  on_source_checked ->
  result
```

Where each finished module callback receives:

- authoritative `ModuleTypings`
- checked-file payload for the group
- enough dependency metadata for host events and store writes

The engine result should also expose the final authoritative public-module
bundle directly, so the host does not need to reconstruct package typings from
per-group callback payloads or from a snapshot.

## 10. Migration Plan

The implementation should move in these steps:

1. introduce the new incremental authoritative engine in `typ`
2. move `riot-check` package checks onto that engine
3. make package-level persistence happen as modules finish
4. split check payload from query payload
5. remove the old rooted package-check orchestration path
6. keep rooted snapshots for query/editor use

During this migration:

- correctness stays pinned by the oracle fixture corpus
- `kernel-new` stays the main cold-check benchmark
- performance work should focus on the new unified path only

## 11. Non-Goals

This document does not require:

- removing rooted snapshots entirely
- making LSP use the same payload shape as plain build checks
- reusing every existing helper unchanged

It is fine to replace large pieces of package-check orchestration if that is
what the architecture requires.

## 12. Design Rule

If a future optimization keeps two different package-check semantics alive, it
is probably the wrong optimization.

The target is:

- one authoritative typing flow
- incremental by construction
- query payload only when requested
- immediate persistence of finished authoritative module typings
