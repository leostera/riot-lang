# Raml TODO

This is the working task list for the Raml compiler family.

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

This is the current compiler-family baseline as of the last update to this
file.

- `riot build raml`
  passes
- `riot test -p raml`
  passes
- `riot fix ./compiler/raml`
  passes
- `riot fmt ./compiler/raml`
  passes
- `Core_ir` now reuses `Typ.Model.SurfacePath`, `Typ.Model.BindingId`, and
  `Typ.Model.EntityId` directly through the local
  `Core_ir.Surface_path` / `Binding_id` / `Entity_id` modules, so shared IR
  refs no longer depend on raw strings
- the JS source-driven lane now also includes
  `0120_string_of_int.ml`, which keeps `Core_ir` direct callee
  `string_of_int` shared while the JS backend lowers it through
  `callPrimitive("%string_of_int", ...)` in `./riot-runtime.js`
- the JS source-driven lane now also includes
  `0126_string_of_float.ml`, which keeps `Core_ir` direct callee
  `string_of_float` shared while the JS backend lowers finite-input calls
  through `callPrimitive("%string_of_float", ...)` in `./riot-runtime.js`
  instead of emitting a bare `string_of_float` identifier
- the JS source-driven lane now also includes
  `0127_float_of_string.ml`, which keeps `Core_ir` direct callee
  `float_of_string` shared while the JS backend lowers finite-input calls
  through `callPrimitive("%float_of_string", ...)` in `./riot-runtime.js`
  instead of emitting a bare `float_of_string` identifier
- the JS source-driven lane now also includes
  `0121_print_newline.ml`, which keeps `Core_ir` direct callee
  `print_newline` shared while the JS backend lowers it through an explicit
  `print_newline` import from `./riot-runtime.js` instead of an ambient
  global
- the JS source-driven lane now also includes
  `0122_int_of_string.ml`, which keeps `Core_ir` direct callee
  `int_of_string` shared while the JS backend lowers valid-input calls
  through `callPrimitive("%int_of_string", ...)` in `./riot-runtime.js`
  instead of emitting a bare `int_of_string` identifier
- the JS source-driven lane now also includes
  `0124_print_int.ml`, which keeps `Core_ir` direct callee
  `print_int` shared while the JS backend lowers it through an explicit
  `print_int` import from `./riot-runtime.js` instead of emitting a bare
  `print_int` identifier or relying on an ambient global
- the JS source-driven lane now also includes
  `0125_print_string.ml`, which keeps `Core_ir` direct callee
  `print_string` shared while the JS backend lowers it through an explicit
  `print_string` import from `./riot-runtime.js` instead of emitting a bare
  `print_string` identifier or relying on an ambient global
- the JS source-driven lane now also includes
  `0128_print_char.ml`, which keeps `Core_ir.Constant.Char` backend-neutral
  while the JS backend lowers direct `print_char` calls through an explicit
  `print_char` import from `./riot-runtime.js` and lowers the shared char
  literal payload to a one-character JS string instead of rejecting the
  source literal or relying on an ambient global
- the JS source-driven lane now also includes
  `0026_sequence_and_ignore.ml`, which keeps direct `ignore` shared by
  lowering source-level `ignore expr` calls through
  `Core_ir.Expr.Sequence` plus `Core_ir.Constant.Unit` instead of emitting a
  bare `ignore` identifier or introducing a JS runtime helper
- the JS source-driven lane now also includes
  `0123_module_identity.ml`, which proves a compile-time-only module lowers
  to empty shared `Core_ir`, empty `JIR`, and empty emitted JS while keeping
  the logical source unit identity in the corpus-backed snapshots
- the JS source-driven lane now also includes
  `0057_phantom_length_vector.ml`, which proves that phantom-index-only
  GADT-style constructors and exhaustive matches whose type indices erase at
  runtime still lower through the existing shared tagged-tuple ordinary-
  variant contract in `Core_ir`, `JIR`, and emitted JS instead of earning a
  JS-only vector encoding
- the active native corpus suite now also includes
  `0120_string_of_int.ml`, which snapshots one top-level source-level
  `string_of_int` direct call nested inside `print_endline` through `NIR`,
  `MIR`, `LIR`, native emission, and linker planning on
  `aarch64-apple-darwin` without introducing a new native pass
- the active native corpus suite now also includes
  `0121_print_newline.ml`, which snapshots one top-level source-level
  `print_newline` direct call through `NIR`, `MIR`, `LIR`, native emission,
  and linker planning on `aarch64-apple-darwin` without introducing a new
  native pass
- the active native corpus suite now also includes
  `0122_int_of_string.ml`, which snapshots one top-level source-level
  valid-input `int_of_string` direct call through `NIR`, `MIR`, `LIR`,
  native emission, and linker planning on `aarch64-apple-darwin` without
  introducing a new native pass
- the active native corpus suite now also includes
  `0123_module_identity.ml`, which snapshots a compile-time-only module with
  empty `NIR`, `MIR`, and `LIR` plus empty-symbol native emission and a clean
  Darwin linker plan on `aarch64-apple-darwin` without introducing a new
  native pass
- the active native corpus suite now also includes
  `0124_print_int.ml`, which snapshots one top-level source-level
  `print_int` direct call through `NIR`, `MIR`, `LIR`, native emission, and
  linker planning on `aarch64-apple-darwin` without introducing a new native
  pass
- the active native corpus suite now also includes
  `0125_print_string.ml`, which snapshots one top-level source-level
  `print_string` direct call through `NIR`, `MIR`, `LIR`, native emission,
  and linker planning on `aarch64-apple-darwin` without introducing a new
  native pass
- the active native corpus suite now also includes
  `0126_string_of_float.ml`, which snapshots one top-level source-level
  finite-input `string_of_float` direct call nested inside `print_endline`
  through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
  `aarch64-apple-darwin` without introducing a new native pass
- the active native corpus suite now also includes
  `0127_float_of_string.ml`, which snapshots one top-level source-level
  finite-input `float_of_string` direct call nested inside
  `string_of_float` and `print_endline` through `NIR`, `MIR`, `LIR`,
  native emission, and linker planning on `aarch64-apple-darwin` without
  introducing a new native pass
- the active native corpus suite now also includes
  `0128_print_char.ml`, which snapshots one top-level source-level
  `print_char` direct call with a shared `Core_ir.Constant.Char` lowered as a
  one-character string literal through `NIR`, `MIR`, `LIR`, native emission,
  and linker planning on `aarch64-apple-darwin` without introducing a new
  native pass
- the active native corpus suite now also includes
  `0026_sequence_and_ignore.ml`, which snapshots one top-level
  `ignore (step "c" n)` eval through the shared `Core_ir.Sequence` plus unit
  lowering path, then through `NIR`, `MIR`, `LIR`, native emission, and
  linker planning on `aarch64-apple-darwin` without introducing a new native
  pass
- the active native corpus suite now includes
  `0002_exported_constants.ml`,
  `0003_top_level_function_direct_call.ml`,
  `0012_list_recursion_sum.ml`,
  `0013_tail_recursive_factorial.ml` and
  `0014_mutual_recursion_even_odd.ml`,
  `0022_local_functions_and_closures.ml`,
  `0023_partial_application.ml`,
  `0049_function_composition_pipeline.ml`,
  `0057_phantom_length_vector.ml`, and `Native.Nir.Lowering` lowers
  function-only recursive local `let` groups through the existing lifted
  local-function path while expression-position anonymous `fun` values now
  materialize as closure tuples plus lifted wrapper entrypoints; the native
  fixture suite snapshots mutually recursive top-level functions plus
  source-level `<>` through `NIR`, `MIR`, `LIR`, native emission, and linker
  planning on `aarch64-apple-darwin`; that same native lane now also snapshots
  stdlib `list` constructors plus one exhaustive recursive `match` through the
  shared tagged-tuple contract across `NIR`, `MIR`, `LIR`, native emission,
  and linker planning; the active
  native corpus suite now also includes
  `0115_external_print_endline.ml`, which snapshots a source-level external
  declaration call through the same native stage surface as a direct external
  symbol call, and `0116_prelude_result_match.ml`, which snapshots stdlib
  `result` constructors plus one exhaustive `match` through the shared tagged-
  tuple contract across `NIR`, `MIR`, `LIR`, native emission, and linker
  planning; the active native corpus suite now also includes
  `0022_local_functions_and_closures.ml`, which snapshots a top-level binding
  that stores one escaped local function value returned from its defining
  function and later calls that closure through the same native stage surface
  without introducing a new native pass; the active native corpus suite now
  also includes `0023_partial_application.ml`, which snapshots under-applied
  direct calls through `NIR`-generated wrapper closures that preserve the
  native closure ABI across later indirect calls, and the active native corpus
  suite now also includes
  `0117_dead_local_bindings.ml`, which snapshots a dead local value binding
  plus one unused non-escaping lifted local helper through the same native
  stage surface without introducing a new native pass; the active native corpus
  suite now also includes
  `0118_printf_and_print_endline.ml`, which snapshots one top-level
  `Printf.printf` eval item followed by one top-level `print_endline` eval
  item through the same native stage surface without introducing a new native
  pass; the active native corpus suite now also includes
  `0025_custom_infix_operators.ml`, which snapshots one top-level custom
  infix-operator direct call through the same native stage surface without
  introducing a new native pass after `Raml.Example_pipeline` seeds the
  minimal polymorphic ambient surface for `@` and `List.iter`; the active
  native corpus suite now also includes
  `0026_sequence_and_ignore.ml`, which snapshots a top-level ignored call
  through the existing shared sequence-plus-unit lowering path and the same
  native stage surface without introducing a new native pass; the active
  native corpus suite now also includes
  `0119_string_concat.ml`, which snapshots one top-level string
  concatenation direct call through the same native stage surface without
  introducing a new native pass; the active native corpus suite now also
  includes `0057_phantom_length_vector.ml`, which snapshots one phantom-
  indexed vector recursive sum through the existing shared tagged-tuple
  encoding plus recursive top-level function lowering, then through `NIR`,
  `MIR`, `LIR`, native emission, and linker planning on
  `aarch64-apple-darwin` without introducing a new native pass; the earlier synthetic
  `constants`, `functions_and_calls`, and `module_identity`
  native contracts are now covered by the shared corpus fixtures
  `0002_exported_constants.ml` and
  `0003_top_level_function_direct_call.ml`, and
  `0123_module_identity.ml`

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

1. [x] Example 01: hello world side effect.
   Source:
   ```ml
   let () = print_endline "hello, world"
   ```
   This first operational slice now compiles through the current shared
   lowering, JS example pipeline, native `NIR -> MIR -> LIR` stack, native
   emission, and linker-plan snapshot surface. The native fixture family also
   snapshots every currently named native pass surface (`normalize`,
   `simplify`, `canonicalize`, `insert_polls`, `layout_frames`, `schedule`).
   On the JS path, `print_endline` now lowers through an explicit sibling
   `./riot-runtime.js` import instead of an ambient global call. The native
   path still models the side effect as an external symbol call.
2. [x] Example 02: exported constants.
   Start from top-level exported values of type `int`, `bool`, `float`,
   `string`, and `unit`.
   The JS source-driven fixture family now snapshots exported constants through
   `Core_ir`, `JIR`, the example pipeline, and final JS emission, and the
   emitted ESM runs under `bun` without sibling runtime files. The native
   corpus suite now also includes `0002_exported_constants.ml`, which
   snapshots the same source program through pass-local `NIR`, `MIR`, and
   `LIR` surfaces plus final native emission and linker planning on
   `aarch64-apple-darwin`, replacing the older synthetic `constants` contract
   with a corpus-backed case.
3. [x] Example 03: top-level function plus direct call.
   Add one positional function and one top-level call site using a previously
   bound value.
   The JS source-driven fixture family now includes
   `0003_top_level_function_direct_call`, which snapshots a top-level lambda,
   a later direct call, and the final emitted JS across
   `Core_ir`, `JIR`, `Raml.Example_pipeline`, and `Raml.Compilation`.
   The same JS lane now also includes `0003_float_arithmetic`, which proves
   source-level `+.`, `*.`, and `sqrt` direct calls lower through
   `callPrimitive("%addfloat" | "%mulfloat" | "%sqrtfloat", ...)` in
   `./riot-runtime.js` instead of emitting bare float operators or an ambient
   `sqrt`, and the emitted JS remains runnable under `bun`. The same JS lane
   now also includes `0119_string_concat`, which proves source-level `^`
   direct calls lower through `callPrimitive("%concatstring", ...)` in
   `./riot-runtime.js` instead of emitting a bare `^` identifier, and the
   emitted JS remains runnable under `bun`.
   The same JS lane now also includes `0004_boolean_logic`, which proves
   source-level direct `not`, `&&`, and `||` lower through nested `JIR`
   conditional expressions instead of bare identifiers or fake runtime helper
   calls, so the emitted JS keeps short-circuit behavior under `bun`.
   The same JS lane now also includes `0023_partial_application`, which proves
   that multi-parameter compiled lambdas lower through `makeCurried` in
   `./riot-runtime.js` so under-applied calls remain runnable under `bun`.
   The same JS lane now also includes `0108_less_than_comparison` and
   `0109_greater_than_comparison`, which prove source-level `<` and `>`
   direct calls lower through `callPrimitive("%lt" | "%gt", ...)` in
   `./riot-runtime.js` instead of emitting bare operator identifiers, and the
   emitted JS remains runnable under `bun`. The same JS lane now also includes
   `0110_less_or_equal_comparison`, which proves source-level `<=` direct
   calls lower through `callPrimitive("%le", ...)` in `./riot-runtime.js`
   instead of emitting a bare `<=` identifier, and the emitted JS remains
   runnable under `bun`. The same JS lane now also includes
   `0111_greater_or_equal_comparison`, which proves source-level `>=` direct
   calls lower through `callPrimitive("%ge", ...)` in `./riot-runtime.js`
   instead of emitting a bare `>=` identifier, and the emitted JS remains
   runnable under `bun`. The same JS lane now also includes
   `0114_top_level_mutual_recursion`, which proves a top-level `let rec ...
   and ...` group stays one shared recursive init group and lowers through the
   current JS `let`-plus-assignment recursive-group path without needing a new
   grouping pass yet. The native corpus suite now also includes
   `0108_less_than_comparison`, `0109_greater_than_comparison`,
   `0110_less_or_equal_comparison`, and `0111_greater_or_equal_comparison`,
   which prove source-level `<`, `>`, `<=`, and `>=` direct calls reach
   `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`, with the AArch64 emitter mangling
   punctuation-bearing callee symbols before assembly. The same native lane
   now also includes `0003_top_level_function_direct_call`, which proves a
   top-level lambda plus later direct call reaches pass-local `NIR`, `MIR`,
   and `LIR` surfaces plus final native emission and linker planning on
   `aarch64-apple-darwin`, replacing the earlier synthetic
   `functions_and_calls` contract with a corpus-backed case. The same native
   lane now also includes `0023_partial_application`, which proves that an
   under-applied top-level direct call lowers in `NIR` to a closure tuple plus
   generated wrapper functions so later indirect calls still reach `MIR`,
   `LIR`, native emission, and linker planning honestly on
   `aarch64-apple-darwin`. The same JS lane now also includes
   `0049_function_composition_pipeline`, which proves expression-position
   anonymous `fun` values lower through shared `Core_ir.Lambda`, then through
   JS function expressions and the existing `makeCurried` runtime path inside
   one higher-order composition binding. The same native lane now also
   includes `0049_function_composition_pipeline`, which proves the same
   anonymous lambdas lower in `NIR` to closure tuples plus lifted wrapper
   entrypoints so the higher-order composition reaches `MIR`, `LIR`, native
   emission, and linker planning honestly on `aarch64-apple-darwin`. The same
   native
   lane now also includes `0114_top_level_mutual_recursion`, which proves a
   top-level function-only `let rec ... and ...` group lowers through the
   existing shared recursive init group plus native top-level lambda path,
   then reaches `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin` without introducing a new native pass. The same
   native lane now also includes `0119_string_concat`, which proves the
   source-level direct `^` call reaches `NIR`, `MIR`, `LIR`, native emission,
   and linker planning on `aarch64-apple-darwin`, with the AArch64 emitter
   reusing the existing punctuation-safe symbol mangling before assembly.
4. [x] Example 04: grouped initialization order.
   Add multiple top-level groups whose execution order matters.
   The JS source-driven fixture family now includes
   `0102_grouped_initialization_order`, which snapshots a named binding, an
   interleaved eval item, and a later binding initializer through `Core_ir`,
   `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission.
   The emitted JS runs under `bun` and preserves the observable `before` then
   `after` execution order. The native corpus suite now snapshots the same
   grouped-init example through `NIR`, `MIR`, `LIR`, native emission, and
   linker planning on `aarch64-apple-darwin`, preserving the same ordered
   top-level binding/eval/binding sequencing.
5. [x] Example 05: conditional expression.
   Add `if/then/else` and make the representation/backend split explicit.
   The shared source-driven slice now lowers source conditionals from `typ`
   into backend-neutral `Core_ir.If_then_else`, and the JS source-driven suite
   includes `0101_tail_conditional_direct_call`, which snapshots a tail
   conditional inside a top-level function body through `Core_ir`, `JIR`,
   `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission.
   The native corpus suite now snapshots the same tail-conditional example
   through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. That native lane now also includes
   `0104_sequence_before_conditional`, which proves `Core_ir.Sequence`
   survives native lowering as explicit ordered `NIR` let-binding evaluation
   before a later conditional result, then reaches `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`.
6. [x] Example 06: local bindings.
   Add non-top-level `let` and make closure/env pressure visible in shared IR.
   The shared source-driven slice now lowers supported local `let` groups from
   `typ` into backend-neutral `Core_ir.Expr.Let`. The source-driven JS suite
   now includes `0006_let_shadowing`, which snapshots nested local rebinding
   through `Core_ir`, `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and
   final JS emission. That JS slice now alpha-stabilizes shadowing locals in
   `JIR` and routes the example's integer operators through
   `./riot-runtime.js`, and the owned sibling `src/js/Printf.js` /
   `src/js/riot-runtime.js` module surfaces now let the emitted example run
   under `bun` without ambient globals.
   The same JS lane now also includes `0112_effect_position_local_let`, which
   proves the first dedicated JS flatten pass can inline effect-position
   zero-arg IIFEs before alpha stabilization so local-`let` eval slices emit
   as plain statements instead of top-level IIFEs. The same lane now also
   includes `0113_initializer_shadowing`, which proves declaration-initializer
   zero-arg IIFEs can flatten separately through a temp binding plus lexical
   block so a local shadow such as `let result = ... in result` does not force
   the final exported `result` binding to rename or leak initializer locals
   into module scope.
   The native corpus suite snapshots the same example through `NIR`, `MIR`,
   `LIR`, native emission, and linker planning on `aarch64-apple-darwin`.
   The JS source-driven suite also includes `0103_local_function_capture`,
   which snapshots a local function binding that captures an outer parameter
   through `Core_ir`, `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and
   final JS emission. The native corpus suite now includes the same example,
   lowering its non-escaping local function capture through a lifted helper
   call in `NIR`, then through `MIR`, `LIR`, native emission, and linker
   planning on `aarch64-apple-darwin`.
   The JS source-driven suite now also includes
   `0105_indirect_call_via_returned_closure`, which snapshots a returned local
   function value and later indirect call through `Core_ir`, `JIR`,
   `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission. The
   native corpus suite now includes the same example, lowering that escaped
   local function value through `NIR` as a closure tuple carrying a
   symbol-addressed closure entrypoint plus captured values, then through
   `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. The native corpus suite now also includes
   `0022_local_functions_and_closures`, which proves the same escaped local
   function path stays correct when the returned closure is first stored in a
   top-level binding and only called later. The same JS lane now also includes
   `0117_dead_local_bindings`, which proves the first dead-binding slice can
   remove an unused captured local helper plus its captured immutable literal
   binding from `JIR`, while a final normalize step drops the now-unused
   runtime helper import before JS emission.
7. [x] Example 07: tuples and tuple-pattern destructuring.
   Add tuple construction plus tuple binders in lambda parameters and local
   `let` bodies.
   The shared source-driven slice now lowers source tuples into
   backend-neutral `Core_ir.Expr.Tuple` / `Core_ir.Expr.Tuple_get`, while the
   JS source-driven suites include `0007_tuples_and_patterns`, which snapshots
   tuple construction and tuple-pattern destructuring through `Core_ir`,
   `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission.
   The emitted JS now runs under `bun` against the owned sibling
   `src/js/Printf.js` / `src/js/riot-runtime.js` module surface instead of a
   half-defined package import hole.
   The native corpus suite snapshots the same example through `NIR`, `MIR`,
   `LIR`, native emission, and linker planning on `aarch64-apple-darwin`,
   with tuple runtime helpers materialized explicitly in `NIR`.
8. [x] Example 08: immutable records and functional update.
   Add one pure record slice without widening into variants or record patterns
   yet.
   The shared source-driven slice now treats top-level type declarations as
   compile-time-only items for `Core_ir`, and lowers immutable record
   construction, field access, and functional update into the existing
   backend-neutral tuple construction/projection forms when the record labels
   resolve to one visible declaration. The JS source-driven suites now include
   `0008_records_and_updates`, which snapshots that record slice through
   `Core_ir`, `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and final JS
   emission. The native corpus suite snapshots the same example through `NIR`,
   `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`.
9. [x] Example 09: variants and pattern matching.
   Add the first closed ordinary-variant constructor plus exhaustive-`match`
   slice without widening into guards, open matches, polymorphic variants, or
   inline-record constructors yet.
   The shared source-driven slice now treats top-level ordinary variant type
   declarations as compile-time-only items for `Core_ir`, lowers closed
   constructor values through tagged tuples, and lowers exhaustive
   constructor-only matches through nested shared `%eq` tag checks plus tuple
   payload projection. The JS source-driven suites now include
   `0009_variants_and_match`, which snapshots that first sum-type slice
   through `Core_ir`, `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and
   final JS emission. The same JS lane now also includes
   `0010_option_pipeline`, which proves nested stdlib `option` flow keeps
   reusing the shared tagged-tuple contract while source-level `=` lowers
   through the JS runtime `%eq` boundary instead of emitting a bare `=`
   identifier, and
   `0106_prelude_option_match`, which proves stdlib `option` constructors and
   exhaustive `match` reuse that tagged-tuple contract, plus
   `0116_prelude_result_match`, which proves stdlib `result` constructors and
   exhaustive `match` reuse that same tagged-tuple contract without earning a
   JS-only encoding, plus `0012_list_recursion_sum`, which proves stdlib
   `list` constructors and one exhaustive recursive `match` reuse that same
   tagged-tuple contract while packing the two `::` arguments into one shared
   tuple payload instead of introducing a JS-only cons-cell encoding. The
   native corpus suite snapshots `0009_variants_and_match`
   through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. The same native lane now also includes
   `0010_option_pipeline`, which proves nested stdlib `option` control flow
   reuses the shared tagged-tuple contract while source-level `=` lowers
   through the existing native equality helper boundary, and
   `0106_prelude_option_match`, which proves stdlib `option` constructors plus
   one exhaustive `match` reach the same native snapshot surface, plus
   `0012_list_recursion_sum`, which proves stdlib `list` constructors plus
   one exhaustive recursive `match` reuse that same tagged-tuple contract
   through the same native snapshot surface while packing the two `::`
   arguments into one shared tuple payload, plus
   `0116_prelude_result_match`, which proves stdlib `result` constructors plus
   one exhaustive `match` reuse that same tagged-tuple contract through the
   same native snapshot surface.

### Example 01: Hello World

These are the immediate tasks needed to make the first example work.

1. [x] Add one dedicated fixture family for the hello-world example.
   Snapshot the source example through `Raml Core IR`, `JIR`, JS output, and
   any future native/wasm projections so every layer is forced to agree on the
   same source program.
2. [x] Decide how `open Std` appears in shared lowering.
   The shared `Typ -> Core_ir` handoff now treats top-level `open Std` as a
   compile-time-only scope item once the semantic tree has already resolved
   later references through that open. The JS source-driven suites now include
   `0107_open_std_hello_world`, which proves the explicit prelude open
   disappears before `Core_ir`, `JIR`, and emitted JS. The native corpus suite
   now includes the same example, reaching `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin` without adding any
   open-specific native lowering.
3. [x] Lower top-level unit bindings used only for side effects.
   `Typ -> Raml Core IR` now lowers `let () = expr` into explicit init-time
   `Eval` items instead of forcing fake named bindings.
4. [x] Make `Raml Core IR` represent module-entry side effects directly.
   `Binding_group.items` now carries both named `Binding` items and effectful
   `Eval` items.
5. [x] Extend `JIR` lowering for side-effecting entry statements.
   The JS path now lowers eval items into ordered top-level expression
   statements.
6. [x] Decide the first runtime/FFI story for `println`.
   The current JS path lowers direct `print_endline` calls through an explicit
   named import from `./riot-runtime.js` instead of relying on an ambient
   global, while the shared/compiler contract keeps that choice out of
   `Core_ir` by preserving `print_endline` as a direct callee name.

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
   `let` groups, runtime-neutral top-level type declarations and top-level
   `open` statements, variable and unit top-level binders, constants,
   symbolic variables, positional direct/indirect applies, top-level lambdas,
   tuple construction, tuple-lowered immutable record
   construction/access/update, source `if/then/else` expressions, source
   sequence expressions, and supported local `let` groups with variable or
   tuple binders.
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
    fixtures. The current `JST` lowering also materializes compatible
    named/default imports from one module as a single ESM import declaration
    before emission, while keeping namespace imports separate. The current
    `JIR` pass stack now also removes immutable identifier-only alias temps
    after alpha stabilization when the target name is never assigned and the
    alias is not exported, which keeps tuple-destructure and match-scrutinee
    wrappers out of emitted JS. The first dead-binding slice now also removes
    unexported immutable `const` bindings whose initializer is already
    effect-free when the name is unused in scope, and the final normalize pass
    recomputes imports from the live body so dead helper references do not
    leak into emitted JS. The late import-materialization slice now also
    rewrites collected `Imported` and `Runtime_helper` references into plain
    local identifiers before `JST` lowering, leaving `program.imports` as the
    only import-declaration surface for the printer-facing backend boundary.

### Native Path

18. [x] Define the first `NIR` modules.
    Make `NIR` the first native-only late IR after `Core_ir`.
19. [x] Lower one tiny `Raml Core IR` slice into `NIR`.
    Start with constants, direct calls, top-level lambdas, and module entry.
    The current `NIR` slice now also preserves `Core_ir.Sequence` ordering by
    lowering it through explicit `let`-bound evaluation before later native
    passes.
20. [x] Define the first `NIR -> MIR -> LIR` contracts.
    Freeze the ownership boundary between native/runtime-oriented lowering,
    machine-oriented lowering, and final linear emission.
21. [x] Decide and document the first native codegen route.
   Keep the first native backend on one locked target and direct assembly from
   a restartable `Linear IR`; do not route v1 through LLVM or Zig.
22. [x] Add the first native scaffold snapshots.
   Reuse `core_ir` fixtures and snapshot `NIR`, `MIR`, `LIR`, and host/target-
   aware native emitter output for the same inputs. The first corpus-backed
   native example now also snapshots pass-local `NIR`, `MIR`, and `LIR`
   outputs for every currently named native pass.
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
