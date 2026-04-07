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

## Items

This is the current best guess for the next OCaml-parity batches.

- [ ] make env summary and reconstruction the authoritative env model instead of
  a side API
- [ ] make summary replay relative to the enclosing env, not `Env.empty`
- [ ] make module scopes carry per-namespace open components so `add_open` can
  consume them directly instead of recursively rebuilding visible env
  components
- [ ] make `Module_env` a closer analogue of OCaml component tables
- [ ] make dotted lookup always go through module components instead of fallback
  path rewriting
- [ ] push `IdentPath` to lowering, persistence, and printing boundaries only
- [ ] remove remaining hot-path name/path fallback in named type comparison
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
