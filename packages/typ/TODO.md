# Typ TODO

This file is the working playbook for `packages/typ`.

The point is not to restate the whole manual. The point is to make the next
work slice obvious:

- what to read
- how to implement a feature
- how to verify it
- how to make sure it helps both compiler-style and LSP-style use-cases

## Core Rule

Do not design new `typ` behavior from the current prototype outward.

Design from the manual inward:

1. find the owning docs
2. write or extend fixtures for the promised behavior
3. implement the smallest missing semantic slice
4. verify it through `typ`, `riot check`, and `riot-lsp`

If the implementation and the manual disagree, either:

- the code is wrong, or
- the manual is incomplete and needs an explicit update

Do not leave the disagreement implicit.

## Read First

Start here:

- [docs/index.md](./docs/index.md)
- [docs/checker/index.md](./docs/checker/index.md)
- [docs/checker/checker.md](./docs/checker/checker.md)
- [docs/checker/solver.md](./docs/checker/solver.md)
- [docs/checker/lowering.md](./docs/checker/lowering.md)
- [docs/checker/engine.md](./docs/checker/engine.md)

Those define the center of gravity:

- semantic tree, not raw CST
- explicit origins
- explicit sessions and snapshots
- query-first API
- canonical `ModuleSummary`

## Reading Map

When working on a feature, read the owning manual sections first.

### Core typing

- [docs/checker/checker.md](./docs/checker/checker.md)
- [docs/checker/solver.md](./docs/checker/solver.md)
- [docs/checker/generalization.md](./docs/checker/generalization.md)

Use this for:

- literals
- variables
- functions
- apply
- let / let rec
- value restriction
- generalization bugs

### Lowering and origins

- [docs/checker/lowering.md](./docs/checker/lowering.md)
- [docs/checker/diagnostics.md](./docs/checker/diagnostics.md)

Use this for:

- what survives lowering
- what is normalized away
- how to preserve source spans and origins
- recovery nodes and unsupported syntax

### Ordinary data

- [docs/checker/nominal_data.md](./docs/checker/nominal_data.md)
- [docs/checker/extensible_variants.md](./docs/checker/extensible_variants.md)
- [docs/checker/pattern_analysis.md](./docs/checker/pattern_analysis.md)

Use this for:

- type declarations
- records
- ordinary variants
- extension constructors
- exceptions
- exhaustiveness / redundancy

### Advanced core typing

- [docs/checker/labeled_args.md](./docs/checker/labeled_args.md)
- [docs/checker/gadts.md](./docs/checker/gadts.md)
- [docs/checker/polyvariants.md](./docs/checker/polyvariants.md)
- [docs/checker/effects.md](./docs/checker/effects.md)

Use this for:

- labels and optional args
- GADT refinement
- polymorphic variants
- effect handlers

### Modules

- [docs/checker/modules.md](./docs/checker/modules.md)
- [docs/checker/signatures.md](./docs/checker/signatures.md)
- [docs/checker/first_class_modules.md](./docs/checker/first_class_modules.md)
- [docs/checker/recursive_modules.md](./docs/checker/recursive_modules.md)

Use this for:

- structures
- signatures
- functors
- first-class modules
- recursive modules
- interface checking

### Engine and host integration

- [docs/checker/engine.md](./docs/checker/engine.md)
- [docs/checker/diagnostics.md](./docs/checker/diagnostics.md)

Use this for:

- sessions
- rooted snapshots
- dependency discovery
- `MissingRequirements`
- `ModuleSummary`
- compiler/LSP query behavior

## Where The Code Usually Lives

This is the rough mapping from the manual to the current code.

### Parse / lowering / semantic tree

- `src/lower.ml`
- `src/SemanticTree*.ml`
- `src/Origin*.ml`
- `src/Diagnostic*.ml`

### Solver / inference

- `src/infer.ml`
- `src/TypeRepr*.ml`
- `src/TypeScheme*.ml`
- `src/TypePrinter*.ml`

### Summaries / persistence / queries

- `src/SourceAnalysis.ml`
- `src/ModuleSummary*.ml`
- `src/PersistedSummary*.ml`
- `src/Query*.ml`
- `src/TypeIndex*.ml`

### Engine

- `src/Session*.ml`
- `src/Snapshot*.ml`
- `src/MissingRequirements*.ml`

If the module boundaries drift too far from the manual, that is usually a sign
the implementation needs refactoring before adding more feature work.

## Feature Implementation Loop

Every new feature should follow this loop.

1. Pick one semantic slice.
   Good slices are things like:
   - record construction
   - one labeled-argument rule
   - one pattern-analysis behavior
   - one summary/query seam

2. Read the owning docs first.
   Do not start from the current prototype behavior.

3. Add or extend tests before changing code.
   The main places are:
   - `tests/fixtures/`
   - `tests/diagnostics/`
   - `tests/session_tests.ml`
   - `tests/persisted_summary_tests.ml`

4. Implement the smallest missing piece.
   Prefer:
   - explicit origins
   - structured diagnostics
   - query-local mutation only
   - recovery nodes for unsupported syntax instead of silent dropping

5. Re-run verification at three levels:
   - `typ` package behavior
   - compiler-style `riot check`
   - editor-style `riot-lsp`

6. If the feature changes the contract, update the manual in the same slice.

7. Commit the slice.
   Use a conventional commit.

## Verification Loop

Run these in roughly this order.

### 1. Local package checks

These are the minimum checks for almost every `typ` change:

```ocaml
timeout 180 riot build typ
timeout 180 riot test -p typ
```

If the change touches docs only, still run:

```ocaml
git diff --check -- packages/typ
```

### 2. Compiler-style verification

`typ` is not done until it works through `riot check`.

Use:

```ocaml
timeout 30 riot build riot-cli
timeout 30 riot test riot-cli:check_tests
riot run riot -- check -p colors
riot run riot -- check -p tty
riot run riot -- check -p colors --json
```

What to look for:

- diagnostics still stream
- diagnostics still use workspace-relative paths
- human output still matches the `syn`-style report shape
- `--json` still emits per-file events, including clean-file events
- package-level checking still sees sibling modules the same way `typ` does

The main CLI integration point is:

- [check_cmd.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/riot-cli/src/check_cmd.ml)

### 3. LSP-style verification

`typ` is also not done until the editor-facing behavior still makes sense.

Use:

```ocaml
timeout 30 riot build riot-lsp
timeout 180 riot test riot-lsp:framing_tests
timeout 180 riot test riot-lsp:session_fixture_tests
```

What to look for:

- type diagnostics still publish cleanly
- spans still map correctly into LSP UTF-16 ranges
- package-scoped typing still resolves sibling modules the same way as
  `riot check`
- no protocol noise leaks into stdout

The main LSP integration point is:

- [session.ml](/Users/leostera/Developer/github.com/leostera/riot/packages/riot-lsp/src/session.ml)

Right now `riot-lsp` still uses `Typ.Session.snapshot`. As `typ` moves harder
toward rooted snapshot preparation and `MissingRequirements`, this file is one
of the first places that must stay aligned.

### 4. Real-package baseline loop

After the tests pass, run a real package.

Start small:

```ocaml
riot run riot -- check -p colors
riot run riot -- check -p tty
```

Then scale up:

```ocaml
riot run riot -- check -p mime
riot run riot -- check -p kernel
riot run riot -- check -p std
```

The real-package loop is how we discover the next missing feature cluster.

Use the remaining diagnostics as the feature inventory for the next slice.

## Feature Acceptance Checklist

Before calling a feature done, make sure all of these are true.

- It is covered by fixtures or diagnostics snapshots in `packages/typ/tests`.
- Its diagnostics are structured, not flattened into one string.
- Its spans and origins point back to the right source region.
- It behaves correctly through `Typ.Query`, not just one one-shot path.
- It still works through `riot check`.
- It still produces useful editor diagnostics through `riot-lsp`.
- If it affects exports, it is reflected in summary/persistence tests.

## Current Architectural Priorities

These are the big structural items still worth keeping in front of us.

### 1. Canonical `ModuleSummary`

We still want one real reusable module-summary center of gravity instead of
prototype split seams.

Manual:

- [docs/checker/engine.md](./docs/checker/engine.md)
- [docs/checker/signatures.md](./docs/checker/signatures.md)

### 2. Real rooted snapshot preparation

We have the rooted API surface, but the full dependency-discovery and
hydration loop still needs to match the engine spec more closely.

Manual:

- [docs/checker/engine.md](./docs/checker/engine.md)

### 3. Real interface/signature checking

`.mli` / interface behavior needs to stop being a soft edge and become part of
the real export boundary.

Manual:

- [docs/checker/signatures.md](./docs/checker/signatures.md)
- [docs/checker/modules.md](./docs/checker/modules.md)

### 4. Remove ambient fake knowledge where possible

Prefer loaded summaries and explicit environments over baked-in fallback
knowledge.

Manual:

- [docs/checker/engine.md](./docs/checker/engine.md)
- [docs/checker/modules.md](./docs/checker/modules.md)

## Snapshot And Diagnostics Bias

When in doubt:

- add a fixture
- add a diagnostics snapshot
- dump semantic structure and environments
- keep the diagnostic structured

This package is still young enough that regressions will be caught faster by
snapshot-heavy tests than by trying to reason about everything from memory.
