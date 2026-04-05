# Typ Specs

This directory is the spec stack for `typ`.

The point of these docs is not to narrate the current prototype. The point is
to say what checker and engine we are actually trying to build.

If the implementation drifts from these docs, that should be visible and
discussable.

## How To Read This Set

Start here:

- [checker.md](./checker.md)
  the first core HM-ish checker slice
- [solver.md](./solver.md)
  the shared algebra underneath the checker slices
- [lowering.md](./lowering.md)
  the contract between source syntax and semantic typing input
- [engine.md](./engine.md)
  the library/runtime contract: sessions, snapshots, summaries, queries

Then read the feature slices:

- [nominal_data.md](./nominal_data.md)
  ordinary type declarations, records, ordinary variants
- [modules.md](./modules.md)
  the module calculus: structures, signatures, functors, inclusion,
  strengthening
- [first_class_modules.md](./first_class_modules.md)
  the bridge between the core calculus and the module calculus
- [gadts.md](./gadts.md)
  constructor result annotations, refinement, existentials, local equalities
- [polyvariants.md](./polyvariants.md)
  row-typed variants, row unification, pressure, and finalization
- [diagnostics.md](./diagnostics.md)
  structured diagnostics as a first-class machine-facing contract

## What Each Doc Owns

These docs are meant to have clear ownership boundaries.

- `checker.md`
  owns the first core calculus and its typing rules
- `solver.md`
  owns shared inference and solving operations used by the checker docs
- `lowering.md`
  owns semantic normalization, origins, and recovery lowering
- `engine.md`
  owns sessions, rooted snapshots, `ModuleSummary`, store hydration, and query
  semantics
- feature docs
  own the extra semantic rules for each language fragment
- `diagnostics.md`
  owns the shape and lifecycle of diagnostics across parse, lowering, typing,
  and query boundaries

If two docs start trying to own the same thing, one of them is probably too
wide.

## How This Maps To `typ`

At a high level, the implementation wants these seams too:

- source + parse layer
- lowering layer
- semantic storage layer
- solver + inference layer
- summary/persistence layer
- session/snapshot/query engine layer
- diagnostics layer

That is the architectural point of writing these separately instead of
collecting everything into one huge “design.md”.

## How To Use These Docs During Implementation

The intended loop is:

1. pick one implementation slice
2. find the spec docs that own that slice
3. write fixtures and snapshots against the promised behavior
4. implement the smallest missing piece
5. update the spec only if we learned the contract itself was wrong or missing

That keeps the docs as specs instead of post-hoc changelogs.

## Validation

These docs should be treated like code.

That means a good docs change should at least pass:

- clean internal links
- terminology consistency with the public `typ` interfaces where names already
  exist
- `git diff --check`

And when the implementation exists for the relevant slice:

- snapshots
- diagnostics fixtures
- behavioral tests against the stated contract

## Current Big Picture

The spec stack says `typ` is:

- a library-first checker
- incremental through sessions and rooted snapshots
- semantically centered on lowered forms plus origins, not on raw CST
- query-first at the public boundary
- driven by canonical reusable `ModuleSummary` artifacts
- structured-diagnostic-first from parse through typing

That is the checker and engine we are building.
