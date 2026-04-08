# Typ Performance Loop

1. pick an item from the list below, make sure to validate your solutoin against how ocaml implements it
2. review its performance
3. commit with conventional commit only your work
4. remove the item from the list below
5. analyze potential next steps and add them to the items list
5. repeat

This is the working loop for getting `typ` to OCaml-level checker speed without
turning the implementation into an untestable mess.

The goal is not to guess at micro-optimizations. The goal is to keep closing the
algorithmic gap between `typ` and OCaml, one small checkpoint at a time.

Our goal success is measured by
- run `riot check` on Orcaset in under `50ms`
- run `riot check` on Riot itself in under `500ms`

These are intentionally aggressive. They are useful because they force us to
keep looking for algorithmic waste instead of settling for "fast enough for
now".

## Current Focus

We now have two useful benchmark floors under
`packages/riot-check/tests/workspace_fixtures/`:

- `no_deps_single`: one package, one file, no deps
- `no_deps_pair`: two packages, no `std`, one local package edge

Those fixtures are the "did we make the checker itself slower?" guardrail.

The first real package we should make semantically trusted is now `kernel`.
`colors` is not an honest compiler target until `kernel -> actors -> std`
becomes trusted, because `riot check` currently allows downstream packages to
consume `ErroredExport` dependency summaries for recovery/LSP behavior.

For builtin deps such as `stdlib`, `unix`, and `dynlink`, do not try to find
and type the toolchain `.mli` files. Instead, keep a Riot-owned stub surface
such as `OCamlStdlib.ml` / builtin module summaries that define exactly the
types and values `typ` needs for checking. The point is to make builtin
interfaces explicit and controllable, not to depend on where the host OCaml
installation stores interface files.

## Items

This is the current best guess for the next OCaml-parity batches.

- [ ] make `kernel` the first trusted real package and use it as the compiler
  correctness ladder below `actors`, `std`, and `colors`
- [ ] introduce an `OCamlStdlib` builtin stub surface for `stdlib`, `unix`,
  and `dynlink` so builtin deps come from Riot-owned summaries instead of
  external toolchain `.mli` discovery
- [ ] expand bootstrap/builtin module summaries to cover the first `kernel`
  slice (`Bytes`, `Hashtbl`, `Buffer`, `Array`, `Obj`, `Unix`, and other
  modules that currently show up as `TYP2001` unbound names)
- [ ] make builtin dependency summaries authoritative and explicitly separate
  them from workspace package summaries in `riot-check` / `typ`
- [ ] use the no-deps fixtures as a hard perf guardrail before and after every
  meaningful checker batch
- [ ] make `Summary2` the only persisted/replay env summary format and delete
  any remaining legacy-summary conversion paths outside `Infer`
- [ ] make snapshot and module-typing hydration reuse `Summary2` / `Env`
  replay directly instead of re-qualifying exported strings back into ambient
  envs
- [ ] push `IdentPath` to lowering, persistence, and printing boundaries only
- [ ] canonicalize named type constructors across lowering and loaded summaries
  so unify no longer needs mixed resolved/unresolved name-path fallback
- [ ] make nominal variance lookup consume canonical constructor ids directly in
  expansive lowering instead of re-resolving by name through visible type
  indexes
- [ ] make constructor resolution fully descriptor-first with no late candidate
  reconstruction
- [ ] make label and record resolution fully descriptor-first with owner-indexed
  fast paths
- [ ] make `TypeDecl` descriptors carry all hot-path variance metadata
- [ ] stop rebuilding visible-type indexes more than the specific `type` /
  `include` / `module alias` update requires
- [ ] make `with_local_level_gen` the only region-entry path for generalization
- [ ] make current region pools fully authoritative for generalization
- [ ] make expansive-binding lowering fully pool-driven rather than root-walk
  driven
- [ ] make `TypeScheme.instantiate` a stricter OCaml-style copy scope that
  reuses closed nongeneric structure aggressively
- [ ] make unify use visited-pair caches and closed-subtree cutoffs everywhere
- [ ] make level lowering and pool rehoming one invariant instead of caller
  discipline
- [ ] remove any remaining resolution or printing work from solver hot paths
- [ ] only after the above, run a fresh profiler and treat the remaining cost as
  constant-factor work instead of missing-algorithm work

- [ ] make `Env` replay caches query-local so no summary replay cache state can
  leak across snapshot/query boundaries
- [ ] benchmark and then remove any remaining duplicate analysis work between
  `Batch.check_source`, `Session.prepare_snapshot`, and fallback direct analysis

## Last Checkpoint: `replace legacy Env with Summary2 and Env2 core`

- status: complete
- Orcaset: `time riot check --json | grep check_summary`  
  `3.70s` user, `0.56s` system, `125%` cpu, `3.389s` wall, summary
  `{"files":31,"read_failures":6,"diagnostics":829,"warnings":8}`
- Riot: `time riot run riot -- check --json | grep check_summary`  
  `152.61s` user, `33.23s` system, `236%` cpu, `1:18.43` wall, summary
  `{"files":1723,"read_failures":1301,"diagnostics":6691,"warnings":2}`
- commit: `TBD`

## Current Investigation: `make kernel semantically trusted`

- status: in progress
- no-deps single floor:
  `time riot check -p solo --json | grep check_summary`
  `0.040s` wall, summary `{"files":1,"read_failures":0,"diagnostics":0,"warnings":0}`
- no-deps pair floor:
  `time riot check -p leaf --json | grep check_summary`
  `0.036s` wall, summary `{"files":1,"read_failures":0,"diagnostics":0,"warnings":0}`
- kernel build floor:
  `time riot build kernel`
  `0.105s` wall
- kernel check baseline:
  `time riot check -p kernel --json | grep check_summary`
  `11.724s` wall, summary `{"files":176,"read_failures":2,"diagnostics":3018,"warnings":2}`
- notes:
  `kernel` is currently dominated by missing builtin/toolchain surface
  (`Unix.*`, `Bytes.*`, `Hashtbl.*`, `Buffer.*`, `Array.of_list`, `Obj.magic`,
  etc.) plus unsupported lowering forms. This is a correctness project first,
  not a constant-factor optimization project yet.

## Last Checkpoint: `cache a root module component index for Module_env.lookup`

- status: complete
- Orcaset: `time riot check --json | grep check_summary`  
  `3.53s` user, `0.45s` system, `136%` cpu, `2.909s` wall, summary
  `{"files":31,"read_failures":6,"diagnostics":834,"warnings":8}`
- Riot: `time riot run riot -- check --json | grep check_summary`  
  `194.96s` user, `50.32s` system, `240%` cpu, `1:41.99` wall, summary
  `{"files":1719,"read_failures":1299,"diagnostics":6689,"warnings":2}`
- commit: unable to create `.git/index.lock` in this environment

## Last Checkpoint: `make Module_env a closer analogue of OCaml component tables`

- status: complete
- Orcaset: `time riot check --json | grep check_summary`  
  `3.80s` user, `0.48s` system, `107%` cpu, `3.987s` wall, summary
  `{"files":31,"read_failures":6,"diagnostics":827,"warnings":8}`
- Riot: `time riot run riot -- check --json | grep check_summary`  
  `158.93s` user, `33.48s` system, `180%` cpu, `1:46.70` wall, summary
  `{"files":1719,"read_failures":1299,"diagnostics":6630,"warnings":2}`
- commit: `ae0be924d`

## Last Checkpoint: `make dotted lookup always go through module components instead of fallback path rewriting`

- status: complete
- Orcaset: `time riot check --json | grep check_summary`  
  `3.45s` user, `0.49s` system, `116%` cpu, `3.378s` wall, summary
  `{"files":31,"read_failures":6,"diagnostics":827,"warnings":8}`
- Riot: `time riot run riot -- check --json | grep check_summary`  
  `153.45s` user, `32.15s` system, `222%` cpu, `1:23.50` wall, summary
  `{"files":1719,"read_failures":1299,"diagnostics":6630,"warnings":2}`
- commit: `bc7e7e843`

## How to analyze potential next items

1. Analyze the code in ./packages/typ/
2. Analyze the code in ./vendor/ocaml/typing/
3. Compare implementations algorithms, and ask yourself "why is OCaml faster here?"
4. Find design issues or implementation shortcomings in our package
5. Add these to the list above

The best new items are usually the smalleset change that makes `typ` more like
OCaml's algorithm while still being small enough to validate and revert
independently

The usual questions are:

- what does OCaml do here that we still do not?
- what work are we still recomputing that OCaml stores once?
- what hot path is still resolving by name/path instead of descriptor/id?
- what structure are we still walking that OCaml skips by invariant?

## How to validate your work for correctness

Run these after each meaningful item or batch of items:

```sh
riot build typ
timeout 180 riot test -p typ
riot build
riot install riot
```

If the batch changes semantics:
- inspect diagnostics output
- inspect snapshot drift carefully
- decide whether the old behavior was wrong or the new behavior is wrong

Do not blindly revert every semantic diff, instead use this rule:
- if the change is slower and semantically neutral or incorrect, revert it
- if the change fixes behavior that was wrong before, fix the tests and keep
  judging the batch on the corrected semantics

## How to validate your work for performance

We run two benchmarks: on orcaset (a small test package) and on riot itself:

```sh
cd /Users/leostera/Developer/github.com/Orcaset/orcaset-oc && time riot check --json | grep check_summary
```

```sh
cd /Users/leostera/Developer/github.com/leostera/riot && time riot run riot -- check --json | grep check_summary
```

When comparing runs, compare both:

- runtime
- semantic summary

Specifically watch:

- `files`
- `read_failures`
- `diagnostics`
- `warnings`

A faster run with different diagnostics might be a correctness regression, not a
win.

Before and after any meaningful checker rewrite, also run the no-deps floors:

```sh
cd /Users/leostera/Developer/github.com/leostera/riot/packages/riot-check/tests/workspace_fixtures/no_deps_single
time riot check -p solo --json | grep check_summary
```

```sh
cd /Users/leostera/Developer/github.com/leostera/riot/packages/riot-check/tests/workspace_fixtures/no_deps_pair
time riot check -p leaf --json | grep check_summary
```

Those runs tell us whether we made the checker itself slower independently of
real workspace complexity or broken package surfaces.

## How to commit your work

If a batch is clean, commit it immediately, with messages like this:

- `refactor(typ): rewrite infer env core with symbolic namespace tables`
- `perf(typ): cache descriptor variances across visible type updates`
- `refactor(typ): centralize local-level generation and pool ownership`

Checkpoint commits matter because most performance work is exploratory.

We want to be able to:

- keep good architectural wins even when they are not immediate speed wins
- revert local regressions cleanly
- compare benchmarks between stable points in history

For every checkpoint, write down the measured speeds too.

At minimum, record:

- Orcaset runtime
- Riot runtime
- the semantic summary for both runs
- the commit hash for the checkpoint

That makes it possible to see exactly which batch produced the biggest speedup
instead of relying on memory or commit messages.

## Other useful heuristics

These have held up well so far:

- env work should move toward symbolic namespace tables, not ad hoc flattening
- descriptor work should move toward ids in hot paths, not strings or paths
- solver work should move toward levels, pools, and local copy scopes
- builtin deps should come from Riot-owned stub summaries, not from trying to
  chase host toolchain interface files
- correctness comes before speed, but correctness fixes should still be
  benchmarked
- architectural parity with OCaml is usually more valuable than guessing at a
  micro-optimization

## What Not To Do

Do not do these things:

- do not guess at the next optimization without first comparing `typ` against
  the relevant OCaml typing modules
- do not treat profiler output as a substitute for algorithmic parity when we
  still know we are missing an OCaml invariant or data structure
- do not land giant rewrites without intermediate checkpoints
- do not keep semantically neutral regressions just because the code looks
  cleaner
- do not revert a change only because snapshots changed; first decide whether
  the old behavior was wrong
- do not compare benchmark numbers without also comparing semantic summaries
- do not benchmark only Orcaset; always benchmark Riot itself too
- do not let strings and `IdentPath` leak deeper into hot paths when an id or
  descriptor should exist instead
- do not reintroduce flattened env reconstruction into hot lookup paths
- do not let query-local mutable state escape the query boundary
- do not mix unrelated work into performance checkpoints
- do not commit a performance batch without writing down the measured speeds for
  that checkpoint
