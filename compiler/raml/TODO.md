# Raml TODO

This is the working task list for `compiler/raml`.

Keep this file blunt and operational.

## Work Loop

Every slice should follow this loop:

1. Pick one example.
2. Read [docs/index.md](./docs/index.md) and [docs/architecture.md](./docs/architecture.md).
3. Read the owning backend manual in `docs/native/`, `docs/js/`, or `docs/wasm/`.
4. Write or update the source fixture for that example first.
5. Add or update the snapshots for every layer the example should reach:
   `Raml Core IR`, `JIR`, JS output, and any implemented native/wasm-specific
   IR or codegen layer.
6. Implement the smallest shared compiler slice needed for the example.
7. Implement the smallest backend-local slice needed for the example.
8. Run the verification stack below.
9. Update the docs in the same change if the contract moved.
10. Commit the slice with a conventional commit.

Do not batch unrelated compiler work into one big change.
Do not move to the next example until the current one is supported end to end
for every backend layer that exists.

## Example-Driven Rule

`raml` should grow around whole examples, not around disconnected IR chores.

That means:

- start from a tiny source program
- make that program lower through shared `Raml Core IR`
- make that same program lower through `JIR` and any implemented native/wasm
  backend IRs
- make that same program emit runnable JS once the JS path exists
- make that same program lower through native/wasm-specific late layers once
  those layers exist
- only then move on to the next example

This is stricter than "add one IR feature".
If an example needs a feature across shared lowering, `JIR`, and one backend's
late IR family, land those changes together.

The current implementation is already ahead on some pure top-level value cases,
but it is still behind on the first operational example below.

## Read First

Always start here:

- [docs/index.md](./docs/index.md)
- [docs/architecture.md](./docs/architecture.md)
- [docs/native/index.md](./docs/native/index.md)
- [docs/js/index.md](./docs/js/index.md)
- [docs/wasm/index.md](./docs/wasm/index.md)

Then read the owning backend docs for the task.

## Reading Map

Use this map before touching code.

### Shared compiler architecture

- [docs/architecture.md](./docs/architecture.md)

Use for:

- `Typ -> Raml Core IR` boundary
- shared passes
- `JIR` versus native/wasm backend-specific IR families
- artifacts and separate compilation

### JavaScript backend

- [docs/js/index.md](./docs/js/index.md)
- [docs/js/architecture.md](./docs/js/architecture.md)
- [docs/js/pipeline.md](./docs/js/pipeline.md)
- [docs/js/ir.md](./docs/js/ir.md)
- [docs/js/runtime-and-ffi.md](./docs/js/runtime-and-ffi.md)
- [docs/js/multi-backend-compatibility.md](./docs/js/multi-backend-compatibility.md)

Use for:

- `JIR`
- JS runtime representation
- JS FFI
- JS artifacts and codegen

### Native backend

- [docs/native/index.md](./docs/native/index.md)
- [docs/native/pipeline.md](./docs/native/pipeline.md)
- [docs/native/lambda.md](./docs/native/lambda.md)
- [docs/native/cmm.md](./docs/native/cmm.md)
- [docs/native/mach.md](./docs/native/mach.md)
- [docs/native/targets.md](./docs/native/targets.md)
- [docs/native/zort-compatibility.md](./docs/native/zort-compatibility.md)

Use for:

- native lowering constraints
- target/backend structure
- `zort` compatibility pressure

### Wasm backend

- [docs/wasm/index.md](./docs/wasm/index.md)
- [docs/wasm/pipeline.md](./docs/wasm/pipeline.md)
- [docs/wasm/runtime.md](./docs/wasm/runtime.md)
- [docs/wasm/ir.md](./docs/wasm/ir.md)
- [docs/wasm/zort-compatibility.md](./docs/wasm/zort-compatibility.md)

Use for:

- wasm lowering constraints
- wasm runtime and host boundary
- wasm-on-`zort` compatibility pressure

## Current Baseline

This is the current package baseline as of the last update to this file.

- `riot build raml`
  passes
- `riot test -p raml`
  passes
- `riot fix ./compiler/raml`
  passes
- `riot fmt ./compiler/raml`
  passes

## Verification Loop

Run these in order for each real slice.

```sh
riot fix ./compiler/raml
riot fmt ./compiler/raml
riot build raml
riot test -p raml
git diff --check -- compiler/raml
```

Interpret the results carefully:

- `riot test -p raml` should execute at least one real test suite. If it starts
  reporting no test suites again, the harness regressed.
- `riot fmt ./compiler/raml` is part of the loop because it can rewrite files in
  place.
- `git diff --check -- compiler/raml` is mandatory for docs, snapshot, and
  generated text changes too.

If the slice changes IR shape, lowering behavior, or emitted code:

1. Add or update snapshot coverage first.
2. Inspect snapshot drift carefully.
3. Decide whether the old behavior was wrong or the new behavior is wrong.

Do not treat every snapshot change as a regression.
Use this rule:

- if the behavior drift is accidental or worse, fix it
- if the old behavior was wrong, keep the new behavior and update the snapshots

## Snapshot Test Rules

Prefer fixture-driven snapshot tests for compiler work.

The first harness should follow the repo's existing pattern:

1. Add a `tests/` directory under `compiler/raml/`.
2. Add one or more `[[bin]]` test entries to [riot.toml](./riot.toml).
3. Use `Test.FixtureRunner.cases` to walk fixtures.
4. Use `Test.Snapshot.assert_json` for IR snapshots and
   `Test.Snapshot.assert_text` for emitted code.
5. Normalize unstable data such as absolute paths, timestamps, random ids, or
   generated symbol names before snapshotting.

Prefer separate snapshots for:

- `Raml Core IR`
- `JIR` or another backend-specific late IR
- final emitted JS / native-ish / wasm-ish output

Do not hide all compiler behavior behind one giant end-to-end snapshot if a
smaller IR snapshot would make drift easier to read.

For example-driven work, prefer one fixture family per source example with
parallel snapshots for each layer.

Shared `*.ml` programs should live under `tests/fixtures/corpus/`, while each
backend-oriented suite keeps its approved snapshots under
`tests/fixtures/js/`, `tests/fixtures/native/`, or `tests/fixtures/wasm/`.
Shared IR fixture inputs may stay in directories such as `core_ir/`,
`jir/`, and `jir_lowering/`, but backend-owned `.expected` files should not.
Ordered corpus filenames are just fixture ordering; compiler-facing relpaths
should drop the numeric prefix before deriving source-unit/module names.

## How To Add More Tasks

When you finish a task or discover a missing slice:

1. Re-read the owning docs and compare them to the current implementation.
2. Find the smallest independent compiler gap.
3. Phrase the task as one observable outcome, not a vague refactor.
4. Put the task in the right section below.
5. Add it in execution order, not just at the bottom.
6. If the task needs new verification or snapshot coverage, say so directly in
   the task text.

Good tasks usually look like:

- define one IR module or contract
- lower one semantic feature into `Raml Core IR`
- lower one `Raml Core IR` slice into `JIR` or one backend-specific late IR
- add one artifact or summary boundary
- add one fixture/snapshot family

Bad tasks usually look like:

- "build the JS backend"
- "do wasm"
- "refactor the compiler"

Keep tasks slice-sized.

## Current Tasks

### Example Ladder

Work through these in order.
Do not skip ahead unless the earlier example is already supported across all
currently implemented layers.

1. [ ] Example 01: hello world side effect.
   Source:
   ```ml
   open Std

   let () = println "hello world"
   ```
   This is the first real operational target. It forces `raml` to handle an
   `open`, a top-level side-effecting unit binding, a string literal, an
   unresolved external/runtime function call, and module entry sequencing.
2. [ ] Example 02: exported constants.
   Start from top-level exported values of type `int`, `bool`, `float`,
   `string`, and `unit`.
3. [ ] Example 03: top-level function plus direct call.
   Add one positional function and one top-level call site using a previously
   bound value.
4. [ ] Example 04: grouped initialization order.
   Add multiple top-level groups whose execution order matters.
5. [ ] Example 05: conditional expression.
   Add `if/then/else` and make the representation/backend split explicit.
6. [ ] Example 06: local bindings.
   Add non-top-level `let` and make closure/env pressure visible in shared IR.
7. [ ] Example 07: records, variants, and pattern matching.
   This is the first data-representation-heavy example and should only land
   after the simpler control-flow slices.

### Example 01: Hello World

These are the immediate tasks needed to make the first example work.

1. [x] Add one dedicated fixture family for the hello-world example.
   Snapshot the source example through `Raml Core IR`, `JIR`, JS output, and
   any future native/wasm projections so every layer is forced to agree on the
   same source program.
2. [ ] Decide how `open Std` appears in shared lowering.
   Either lower it explicitly or resolve it away before `Raml Core IR`, but the
   rule must be documented and tested.
3. [x] Lower top-level unit bindings used only for side effects.
   `Typ -> Raml Core IR` now lowers `let () = expr` into explicit init-time
   `Eval` items instead of forcing fake named bindings.
4. [x] Make `Raml Core IR` represent module-entry side effects directly.
   `Binding_group.items` now carries both named `Binding` items and effectful
   `Eval` items.
5. [x] Extend `JIR` lowering for side-effecting entry statements.
   The JS path now lowers eval items into ordered top-level expression
   statements.
6. [ ] Decide the first runtime/FFI story for `println`.
   For the JS path this likely becomes a runtime import or a known external.
   For the shared/compiler contract, freeze the boundary instead of hardcoding a
   JS-only assumption into `Core IR`.

### Foundation

8. [x] Fix the package skeleton lint baseline.
   Rename `src/raml.ml` and `src/raml.mli` to snake_case, update
   [riot.toml](./riot.toml), and get `riot fix ./compiler/raml` clean.
9. [x] Add a real test harness for `raml`.
   Create `compiler/raml/tests/`, and make `riot test -p raml` run actual tests.
10. [x] Add the first fixture/snapshot runner.
   Follow the existing `Test.FixtureRunner` + `Test.Snapshot` pattern used in
   other packages and snapshot a tiny compiler slice.

### Shared IR

11. [x] Define the first `Raml Core IR` modules.
   The shared center is now Lambda-shaped enough to grow real passes:
   `Compilation_unit`, `Binding_group`, `Init_item`, expression-level
   `lambda`/`apply`, and constants.
12. [x] Add stable printers or encoders for `Raml Core IR`.
   We need deterministic JSON or text snapshots before the IR grows.
13. [x] Define the first lowering contract from `typ` semantic data into
   `Raml Core IR`.
   Land the first explicit implementation-only slice: top-level non-nested
   `let` groups, variable and unit top-level binders, constants, symbolic
   variables, positional direct/indirect applies, and top-level lambdas.
14. [x] Make module initialization order and exports explicit in `Raml Core IR`.
   This is shared compiler work, not a backend-local hack.

### JavaScript Path

15. [x] Define the first `JIR` modules.
   Keep them JS-specific and late; do not let them become the shared compiler
   IR.
16. [x] Lower one tiny `Raml Core IR` slice into `JIR`.
   Start with constants, lets, direct calls, and exports.
17. [x] Add the first JS emission snapshots.
    Snapshot `Raml Core IR`, `JIR`, and final JS separately for the same
    fixtures.

### Native Path

18. [x] Define the first `NIR` modules.
    Make `NIR` the first native-only late IR after `Core_ir`.
19. [x] Lower one tiny `Raml Core IR` slice into `NIR`.
    Start with constants, direct calls, top-level lambdas, and module entry.
20. [x] Define the first `NIR -> MIR -> LIR` contracts.
    Freeze the ownership boundary between native/runtime-oriented lowering,
    machine-oriented lowering, and final linear emission.
21. [x] Decide and document the first native codegen route.
   Keep the first native backend on one locked target and direct assembly from
   a restartable `Linear IR`; do not route v1 through LLVM or Zig.
22. [x] Add the first native scaffold snapshots.
   Reuse `core_ir` fixtures and snapshot `NIR`, `MIR`, `LIR`, and host/target-
   aware native emitter output for the same inputs.
23. [x] Lift host/target triple inputs into the public compile/codegen API.
   `Raml.Config` now carries explicit `host` and `target` triples, and backend
   routing happens immediately after `Core_ir` based on the target triple.

### Wasm Path

24. [ ] Define the first wasm post-`Core_ir` runtime/host contract.
   Freeze the wasm-only layer that owns Wasm GC value encoding, helper/host
   imports, startup/loader metadata, and one first host/effects mode before
   final Wasm emission exists.

### Artifacts

25. [ ] Define the first summary artifact boundary.
   Decide what per-module data `raml` needs before `JIR`, `NIR`, or wasm
   runtime/host lowering turn into full codegen.
26. [ ] Keep the docs and this TODO file in sync with the implementation.
   If the architecture changes, update the manual in the same batch.
