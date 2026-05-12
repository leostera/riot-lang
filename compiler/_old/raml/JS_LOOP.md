# JS Loop

Pick up a task from the list below, and start working on it. Always make sure
to make progress towards the goal, and document your process here so you know
how far along you've gone and what you'll do next.

---

This file is the operational loop for extending `compiler/raml`'s JavaScript
backend.

Use it when the task is:

- make the JS fixtures pass
- extend `JIR`, `JST`, or the JS runtime boundary
- add a JS-only pass or optimization
- study Melange `jscomp` and decide whether a pass is worth copying

This is not the architecture spec.
That lives under `docs/`.
This file is the day-to-day work loop.

## Goal

Grow the JS backend from:

`Core_ir -> Js.Jir -> Js.Jst -> .js`

into a backend that:

- preserves shared semantics from `Core_ir`
- keeps JS-only decisions in `src/js/`
- emits runnable JS
- earns each new pass with a concrete fixture and a concrete invariant

## Non-Negotiables

1. `Core_ir` stays backend-neutral.
2. JS runtime, import, module, and FFI choices live in `src/js/`.
3. The emitter stays dumb. If the printer needs cleverness, add structure or a
   pass before it.
4. Shared source programs live in `tests/fixtures/corpus/`.
5. Backend-owned snapshots live under:
   - `tests/fixtures/js/`
   - `tests/fixtures/native/`
   - `tests/fixtures/wasm/`
6. Shared IR input fixtures may stay under:
   - `tests/fixtures/core_ir/`
   - `tests/fixtures/jir/`
   - `tests/fixtures/jir_lowering/`
   But their backend-owned `.expected` files still belong under the backend
   snapshot directories.
7. Do not copy Melange passes just because they exist. Copy the invariant, not
   the ceremony.
8. A JS slice is not done just because snapshots say `"status": "ok"`. If the
   emitted code still depends on ambient globals such as `print_endline`, the
   JS runtime/import boundary is still incomplete.
9. JS loop work does not own native fixture migration. Do not delete or rename
   `tests/fixtures/native/*` or rewrite `tests/native_fixture_tests.ml` from a
   JS slice unless the shared compiler contract changed deliberately and the
   native coverage remains equivalent or better.

## Read Order

Read these first:

1. [docs/index.md](./docs/index.md)
2. [docs/architecture.md](./docs/architecture.md)
3. [docs/js/index.md](./docs/js/index.md)
4. [docs/js/architecture.md](./docs/js/architecture.md)
5. [docs/js/pipeline.md](./docs/js/pipeline.md)
6. [docs/js/ir.md](./docs/js/ir.md)
7. [docs/js/runtime-and-ffi.md](./docs/js/runtime-and-ffi.md)
8. [docs/js/multi-backend-compatibility.md](./docs/js/multi-backend-compatibility.md)

Then use these Melange source anchors by concern:

- driver/front half:
  - `3rdparty/melange/jscomp/core/js_implementation.cppo.ml`
  - `3rdparty/melange/jscomp/core/initialization.cppo.ml`
- `Lam` lowering and middle-end:
  - `3rdparty/melange/jscomp/core/lam_convert.cppo.ml`
  - `3rdparty/melange/jscomp/core/lam_compile_main.cppo.ml`
  - `3rdparty/melange/jscomp/core/lam_group.mli`
  - `3rdparty/melange/jscomp/core/lam_coercion.mli`
- JS passes:
  - `3rdparty/melange/jscomp/core/js_pass_flatten.ml`
  - `3rdparty/melange/jscomp/core/js_pass_tailcall_inline.ml`
  - `3rdparty/melange/jscomp/core/js_pass_flatten_and_mark_dead.ml`
  - `3rdparty/melange/jscomp/core/js_pass_scope.ml`
  - `3rdparty/melange/jscomp/core/js_shake.ml`
- runtime and FFI:
  - `3rdparty/melange/jscomp/core/lam_compile_external_call.ml`
  - `3rdparty/melange/jscomp/core/lam_ffi.ml`
  - `3rdparty/melange/jscomp/common/external_arg_spec.mli`
  - `3rdparty/melange/jscomp/common/external_ffi_types.mli`

## Current Backend Shape

The current JS backend source lives under:

- `src/js/jir/`
- `src/js/jst/`

Key files today:

- `src/js/jir/types.ml`
- `src/js/jir/lowering.ml`
- `src/js/jir/passes/normalize.ml`
- `src/js/jst/types.ml`
- `src/js/jst/lowering.ml`
- `src/js/jst/emitter.ml`

The current backend already sketches the whole `Core_ir` surface:

- constants
- direct calls
- indirect calls
- lambdas
- `let` / `let rec`
- sequences
- conditionals
- primitives
- top-level recursive groups
- basic namespace-import lowering for dotted module refs

That does not mean the backend is done.
It means we now have a complete enough seam to start adding deliberate passes.

## Fixture Map

Use the fixtures by purpose:

- shared source corpus:
  - `tests/fixtures/corpus/*.ml`
- shared `Core_ir` fixtures:
  - `tests/fixtures/core_ir/*.json`
- direct `JIR` shape fixtures:
  - `tests/fixtures/jir/*.json`
- `Core_ir -> JIR` lowering fixtures:
  - `tests/fixtures/jir_lowering/*.json`
- JS backend snapshots:
  - `tests/fixtures/js/*.expected`
- native backend snapshots:
  - `tests/fixtures/native/*.expected`
- wasm backend snapshots:
  - `tests/fixtures/wasm/*.expected`

Current JS fixture bins:

- `tests/jir_fixture_tests.ml`
- `tests/jir_lowering_fixture_tests.ml`
- `tests/js_fixture_tests.ml`
- `tests/example_fixture_tests.ml`
- `tests/compilation_fixture_tests.ml`

## Work Loop

Use this loop for every JS slice:

1. Pick one example or one IR fixture.
2. Decide the missing invariant.
3. Decide the ownership boundary:
   - `Core_ir`
   - `Js.Jir`
   - `Js.Jir.Passes`
   - `Js.Jst`
   - `Js.Jst` pass
   - runtime boundary
   - emitter
4. Add or update the smallest snapshot that proves the missing invariant.
5. Implement the smallest change that makes that snapshot true.
6. Run the validation stack.
7. If the diff starts touching `tests/native_fixture_tests.ml` or
   `tests/fixtures/native/`, stop and justify why. If it is not a deliberate
   shared-contract migration, split or revert that churn.
8. If the output is runnable JS, run it with `bun`.
9. Update docs when the contract changes.
10. Only record a slice as landed when `riot build raml` and
    `riot test -p raml` both pass. If they fail, write down the blocker
    explicitly instead of claiming progress.

Do not start by inventing a pass name.
Start by naming the invariant.

## Questions To Ask Before Adding A Pass

For every Melange pass you study, answer these before you add anything:

1. What invariant does this pass create?
2. What later pass or emitter simplification depends on that invariant?
3. Is the invariant shared, JS-only, or printer-only?
4. Can we get the same value with a simpler lowering change?
5. What one fixture will prove the pass is needed?
6. What one fixture will prove the pass is correct?

If you cannot answer those, do not add the pass yet.

## Pass Ladder To Evaluate

Evaluate these in roughly this order.
Each item names the value it brings and the Melange source to study.

### 1. Top-Level Grouping

Value:

- stable execution order
- clearer recursive-group handling
- cleaner distinction between declarations and init statements

Study:

- `lam_group.mli`
- `lam_coercion.mli`

Likely home:

- `src/js/jir/passes/group_top_level.ml`

### 2. Import And Dependency Materialization

Value:

- separate early semantic dependency discovery from late JS import emission
- avoid smuggling imports through string hacks

Study:

- `lam_compile_env.mli`
- `js_packages_info.ml`
- `js_name_of_module_id.ml`
- `js_dump_import_export.ml`

Likely homes:

- `src/js/jir/imports.ml`
- `src/js/jir/passes/materialize_imports.ml`

### 3. Alias Cleanup

Value:

- fewer temps
- smaller closures
- easier later DCE and shaking

Study:

- `lam_pass_remove_alias`

Likely home:

- `src/js/jir/passes/remove_aliases.ml`

### 4. Alpha / Name Stabilization

Value:

- no accidental capture
- predictable temp naming
- easier readable snapshots

Study:

- `lam_pass_alpha_conversion`

Likely home:

- `src/js/jir/passes/alpha.ml`

### 5. Statementification / Flattening

Value:

- fewer nested IIFEs
- simpler printer-facing `JST`
- fewer parenthesization edge cases

Study:

- `js_pass_flatten.ml`
- `js_pass_flatten_and_mark_dead.ml`

Likely home:

- first effect-position IIFE slice:
  - `src/js/jir/passes/flatten.ml`
- if a later printer-facing cleanup still proves necessary:
  - `src/js/jst/passes/flatten.ml`

### 6. Dead Binding Elimination

Value:

- remove unused locals introduced by normalization
- shrink emitted JS before full shaking

Study:

- `lam_pass_lets_dce`
- `js_pass_flatten_and_mark_dead.ml`

Likely homes:

- `src/js/jir/passes/dce.ml`
- or `src/js/jst/passes/mark_dead.ml`

### 7. Scope / Free-Variable Analysis

Value:

- understand captures
- identify unused params
- prepare tree shaking and future closure/runtime work

Study:

- `js_pass_scope.ml`

Likely home:

- `src/js/jst/passes/scope.ml`

### 8. Tree Shaking

Value:

- keep only exported or effectful definitions and their dependencies
- reduce emitted JS and imports

Study:

- `js_shake.ml`

Likely home:

- `src/js/jst/passes/shake.ml`

### 9. Tailcall Strategy

Value:

- replace accidental recursion blowups with a chosen JS strategy
- make recursion semantics explicit

Study:

- `js_pass_tailcall_inline.ml`

Likely home:

- `src/js/jst/passes/tailcalls.ml`

Do not add this early unless a real fixture justifies it.

### 10. FFI And `external`

Value:

- move from opaque primitives to typed JS interop
- separate runtime helpers from user foreign bindings

Study:

- `lam_compile_external_call.ml`
- `lam_ffi.ml`
- `external_arg_spec.mli`
- `external_ffi_types.mli`

Likely homes:

- `src/js/jir/runtime.ml`
- `src/js/jir/passes/lower_external.ml`

## Running The JS Output

For runnable JS snapshots, materialize the `"js"` field and run it with `bun`.

Example:

```sh
mkdir -p /tmp/raml-js
jq -r '.js' compiler/raml/tests/fixtures/js/0001_hello_world.js.expected > /tmp/raml-js/hello_world.mjs
bun run /tmp/raml-js/hello_world.mjs
```

When the emitted program imports local runtime or sibling modules:

1. write the emitted file into a temp directory
2. materialize any required sibling files there too
3. run `bun run <file>`

Do not patch snapshots by hand just to make `bun` happy.
Fix the lowering or runtime boundary.

If the emitted file still references an ambient global such as
`print_endline`, treat that as a JS runtime-boundary gap, not as
"close enough". A green snapshot is still only partial progress until the
output runs under `bun` with explicit imports/helpers.

## Validation Stack

Use this stack for JS work:

```sh
riot fix ./compiler/raml
riot fmt ./compiler/raml
riot build raml
riot test -p raml
git diff --check -- compiler/raml
```

If the JS slice emits runnable code, also run:

```sh
bun run <file>
```

If validation is blocked by an unrelated package outside `compiler/raml`,
call that out explicitly and do not bury the blocker inside JS backend changes.

## Definition Of Done For A JS Slice

A slice is done when:

1. the smallest relevant fixture passes
2. snapshots moved in a readable way
3. the invariant is now explicit in code
4. the emitter got no smarter than necessary
5. the docs match the new contract

## Immediate Next Targets

Given the current backend shape, the next useful JS tasks are:

1. keep the first sum-type encoding honest:
   - `0009_variants_and_match` is now the accepted first runnable source
     example with closed variants plus one exhaustive `match`
   - `0057_phantom_length_vector` is now the accepted first phantom-index-only
     GADT-style source example proving that constructors and exhaustive
     matches whose type indices erase at runtime still reuse that same shared
     tagged-tuple contract instead of earning a JS-only vector encoding early
   - `0012_list_recursion_sum` is now the accepted first runnable source
     example proving that stdlib `list` reuses the same tagged-tuple
     contract, with `::` packing its head and tail into one shared tuple
     payload instead of earning a JS-only cons-cell encoding early
   - `0106_prelude_option_match` is now the accepted first runnable source
     example proving that stdlib `option` reuses the same tagged-tuple
     contract instead of earning a JS-only encoding early
   - `0116_prelude_result_match` is now the accepted first runnable source
     example proving that stdlib `result` reuses that same tagged-tuple
     contract instead of earning a JS-only encoding early
   - only widen sum types when a new fixture proves the current tagged-tuple
     lowering is insufficient or when a clearer shared/backend split is ready
2. keep immutable-record lowering honest:
   - the current shared contract lowers immutable record construction, field
     access, and functional update through `Core_ir.Expr.Tuple` /
     `Core_ir.Expr.Tuple_get`
   - do not add JS-only record object syntax or a record-specific runtime
     helper unless a new fixture proves the shared tuple path is insufficient
3. stabilize the first explicit JS runtime surface before `external` work:
   - keep backend-selected helpers in `./riot-runtime.js`
   - keep source-visible stdlib modules such as `./Printf.js` separate from
     low-level primitive dispatch
   - `0121_print_newline` now proves that direct `print_newline ()` calls
     lower through an explicit `print_newline` import from
     `./riot-runtime.js` instead of emitting a bare `print_newline`
     identifier or relying on an ambient global
   - `0124_print_int` now proves that direct `print_int` calls lower through
     an explicit `print_int` import from `./riot-runtime.js` instead of
     emitting a bare `print_int` identifier or relying on an ambient global
   - `0125_print_string` now proves that direct `print_string` calls lower
     through an explicit `print_string` import from `./riot-runtime.js`
     instead of emitting a bare `print_string` identifier or relying on an
     ambient global
   - `0128_print_char` now proves that direct `print_char` calls lower
     through an explicit `print_char` import from `./riot-runtime.js`
     instead of emitting a bare `print_char` identifier or relying on an
     ambient global, while shared char literals lower through
     `Core_ir.Constant.Char` instead of staying unsupported
   - `0003_float_arithmetic` now proves that direct `+.`, `*.`, and `sqrt`
     calls lower through `callPrimitive("%addfloat" | "%mulfloat" |
     "%sqrtfloat", ...)` instead of emitting bare float operators or an
     ambient global
   - `0119_string_concat` now proves that direct `^` calls lower through
     `callPrimitive("%concatstring", ...)` instead of emitting a bare `^`
     identifier
   - `0120_string_of_int` now proves that direct `string_of_int` calls lower
     through `callPrimitive("%string_of_int", ...)` instead of emitting a bare
     `string_of_int` identifier
   - `0126_string_of_float` now proves that finite-input direct
     `string_of_float` calls lower through
     `callPrimitive("%string_of_float", ...)` instead of emitting a bare
     `string_of_float` identifier
   - `0122_int_of_string` now proves that valid-input direct
     `int_of_string` calls lower through `callPrimitive("%int_of_string", ...)`
     instead of emitting a bare `int_of_string` identifier
   - `0127_float_of_string` now proves that finite-input direct
     `float_of_string` calls lower through
     `callPrimitive("%float_of_string", ...)` instead of emitting a bare
     `float_of_string` identifier
4. keep native fixture migration separate; do not count native snapshot churn
   as JS progress, even when a shared source example expands both suites
5. use the tail-position sequence and conditional slices as proof points for a
   deliberate flattening pass instead of accumulating more ad hoc IIFE logic:
   - the first effect-position zero-arg IIFE flattening slice now lives in
     `src/js/jir/passes/flatten.ml` and runs before alpha stabilization
   - the first declaration-initializer zero-arg IIFE flattening slice now
     lives there too, lowering through a temp binding plus lexical block when
     a dedicated fixture proves the final binding name must survive local
     shadowing
6. keep recursion coverage honest:
   - `0013_tail_recursive_factorial` now proves local `let rec` inside a
     function body
   - `0114_top_level_mutual_recursion` is now the accepted first module-scope
     mutual recursion slice using the current shared recursive-group plus JS
     `let`-prelude-and-assignment strategy
   - keep tailcall strategy and broader recursive-group rewrites separate
     until a narrower fixture proves the current lowering is insufficient
7. start `external` work by separating:
   - runtime primitives
   - user foreign bindings
   - module import materialization

## Progress Notes

### 2026-04-11: Native `0128_print_char` Approval Debt Is The Current Package Blocker

The JS slice for `0128_print_char` is landed, but the full `raml` package is
currently red because the shared corpus example has pending native snapshot
approvals.

- fixture:
  - `tests/fixtures/corpus/0128_print_char.ml`
  - `tests/fixtures/native/0128_print_char.*.expected.new`
- invariant:
  - the current red state is native approval debt for a shared corpus
    expansion, not a JS backend regression
  - keep JS progress accounting honest: do not describe the full package as
    green while the native suite still needs snapshot promotion for `0128`
  - keep this recorded as shared corpus/native approval debt, not as a new JS
    runtime or lowering blocker
- ownership:
  - `tests/native_fixture_tests.ml`
  - `tests/fixtures/native/0128_print_char.*.expected.new`
  - `JS_LOOP.md`
- effect so far:
  - the JS lanes for the latest runtime-boundary work are green
  - the only current package blocker is the eleven pending native approvals
    for `0128_print_char`
- validation:
  - `riot build raml` passed
  - `riot test -p raml` is currently red with 11 approval misses, all for
    `0128_print_char`
  - `git diff --check -- compiler/raml` passed
- next:
  - review and promote or reject the
    `tests/fixtures/native/0128_print_char.*.expected.new` files
  - only treat the full package as green again after those native approvals
    land

### 2026-04-11: Phantom-Indexed Vector Coverage Landed In The JS Source-Driven Lane

Picked immediate next target 1 again, but kept the slice narrower than
general GADT support by proving only one phantom-index-only vector example
whose runtime constructors erase to the same ordinary tagged-tuple shape that
the shared compiler already uses for variants.

- fixture:
  - `tests/fixtures/corpus/0057_phantom_length_vector.ml`
  - `tests/fixtures/js/0057_phantom_length_vector.core_ir.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.jir.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.js.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.pipeline.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.lowering.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.codegen.expected`
  - `tests/fixtures/js/0057_phantom_length_vector.compilation.expected`
- invariant:
  - a source-level phantom-index-only GADT-style declaration such as
    `type _ vec = VNil | VCons of int * 'n vec` should keep lowering through
    the existing shared ordinary-variant constructor and exhaustive-match
    path when its type indices erase away at runtime
  - this slice is coverage-only unless the current backend proves
    insufficient: do not add a JS-only vector encoding or a GADT-specific
    runtime helper when the existing tagged-tuple contract already carries the
    runtime semantics
  - keep existential witnesses, equality proofs, and broader GADT-specific
    typing/runtime work separate until a narrower fixture proves the current
    erased-constructor path is insufficient
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0057_phantom_length_vector.*.expected`
  - `docs/architecture.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suites now cover `0057_phantom_length_vector`
    instead of leaving that phantom-indexed recursive constructor example
    native-only
  - the approved snapshots show the intended erased-runtime split directly:
    shared `Core_ir` still lowers `VNil` and `VCons` through the existing
    tagged-tuple contract, while JS lowering keeps reusing `%tuple_make`,
    `%tuple_get`, `%eq`, `%addint`, and `Printf.printf`
  - no compiler, runtime, or emitter changes were needed; the current shared
    lowering and JS backend were already sufficient once the seven missing JS
    approvals were promoted
- validation:
  - `riot test -p raml phantom_length_vector` initially failed only because
    the seven `0057_phantom_length_vector.*.expected` snapshots were missing;
    the existing native `0057` lane stayed green and unchanged
  - after promoting those seven snapshots,
    `riot test -p raml phantom_length_vector` passed
  - `bun run /tmp/raml-js-0057.XXXXXX/phantom_length_vector.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `6`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed at that point; the current worktree is now red
    only because the native `0128_print_char` approvals are still pending
  - `git diff --check -- compiler/raml` passed
- next:
  - keep broader GADT runtime work separate; this slice only proves the
    erased phantom-index constructor case that already fits the shared
    tagged-tuple contract
  - keep `0011_result_pipeline`, `try/with`, and other effectful control-flow
    follow-ups separate until a narrower fixture proves the next missing gap
    is not just coverage

### 2026-04-11: Source-Level `ignore` Direct Calls Started Under An Earlier `typ` Worktree Blocker

Picked immediate next target 5 again, but kept the slice narrower than a JS
cleanup pass by settling only one shared semantic invariant: direct
source-level `ignore expr` should lower to backend-neutral sequencing instead
of surviving as a backend/runtime call.

- fixture:
  - `tests/fixtures/corpus/0026_sequence_and_ignore.ml`
  - `tests/fixtures/js/0026_sequence_and_ignore.core_ir.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.jir.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.js.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.pipeline.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.lowering.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.codegen.expected`
  - `tests/fixtures/js/0026_sequence_and_ignore.compilation.expected`
- invariant:
  - a direct source-level `ignore expr` call should stay out of the JS runtime
    surface and out of native symbol lowering; the shared `Typ -> Core_ir`
    handoff should materialize it as `Core_ir.Expr.Sequence { first = expr;
    second = () }` instead of emitting a bare direct callee `ignore`
  - this is a shared lowering choice, not an emitter trick and not a new
    runtime helper; first-class value uses of `ignore` stay outside this slice
- ownership:
  - `packages/typ/src/LanguagePrelude.ml`
  - `compiler/raml-core/src/typ_lowering.ml`
  - `compiler/raml/tests/js_fixture_tests.ml`
  - `compiler/raml/tests/example_fixture_tests.ml`
  - `compiler/raml/tests/compilation_fixture_tests.ml`
  - `compiler/raml/tests/fixtures/js/0026_sequence_and_ignore.*.expected`
  - `compiler/raml/docs/architecture.md`
  - `compiler/raml/docs/js/ir.md`
  - `compiler/raml/docs/js/runtime-and-ffi.md`
  - `compiler/raml/AGENTS.md`
  - `compiler/raml/TODO.md`
  - `compiler/raml/JS_LOOP.md`
- effect:
  - the shared prelude now types `ignore` as `'a -> unit`
  - direct `ignore (step "c" n)` lowering now becomes shared `Sequence` plus
    `Unit`, so JS emission reuses ordinary effect statements and native
    lowering no longer has to treat `ignore` as a foreign symbol
  - `0026_sequence_and_ignore` now reaches the JS source-driven bins through
    `Core_ir`, `JIR`, `Raml.Example_pipeline`, `Raml.Compilation`, and
    runnable emitted JS
- validation:
  - `riot test -p raml sequence_and_ignore` passed after promoting the seven
    JS snapshots above
  - `riot fmt ./packages/typ` passed
  - `riot fmt ./compiler/raml` passed
  - `riot fix ./compiler/raml` passed
  - `riot fix ./packages/typ` still reports the existing package-wide lint
    backlog; that is unrelated to this slice
  - `bun run /tmp/raml-js-0026.XXXXXX/sequence_and_ignore.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `a:1 b:2 c:3 `
  - `git diff --check -- compiler/raml packages/typ/src/LanguagePrelude.ml`
    passed
  - an earlier validation pass was blocked by unrelated in-flight changes
    under `packages/typ/src/check.ml` in this worktree, not by the `ignore`
    slice
  - an earlier `riot test -p raml` attempt was blocked by the same unrelated
    `typ` worktree failure before `raml` ran
- next:
  - this slice started under an unrelated `packages/typ/src/check.ml`
    worktree break; keep that context as history only, not as a current
    blocker
  - keep first-class `ignore` values and any JS-only cleanup of effect-position
    `undefined;` no-op statements separate until a narrower fixture proves
    those are the next real gap

### 2026-04-11: Native `0026_sequence_and_ignore` Approval Debt Was An Earlier Package Blocker

The latest JS runtime-boundary slices were green, but at this point in the
loop the full `raml` package was still red because the shared
`0026_sequence_and_ignore` corpus example had pending native approvals.

- fixture:
  - `tests/fixtures/corpus/0026_sequence_and_ignore.ml`
  - `tests/fixtures/native/0026_sequence_and_ignore.*.expected.new`
- invariant:
  - the current red state is native approval debt for a shared corpus
    expansion, not a JS backend regression
  - keep JS progress accounting honest: do not describe the package as green
    while the native suite still needs snapshot promotion for `0026`
  - keep this recorded as shared corpus/native approval debt, not as a new JS
    runtime or lowering blocker
- ownership:
  - `tests/native_fixture_tests.ml`
  - `tests/fixtures/native/0026_sequence_and_ignore.*.expected.new`
  - `JS_LOOP.md`
- effect so far:
  - the JS lanes for the latest runtime-boundary work are green
  - at that point, the only package blocker was the eleven pending native
    approvals for `0026_sequence_and_ignore`
- validation:
  - `riot build raml` passed
  - at that point, `riot test -p raml` was red with 11 approval misses, all
    for `0026_sequence_and_ignore`
  - `git diff --check -- compiler/raml` passed
- next:
  - review and promote or reject the
    `tests/fixtures/native/0026_sequence_and_ignore.*.expected.new` files
  - treat this note as historical once those native approvals land and the
    full package goes green again

### 2026-04-11: Source-Level `print_char` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again, but kept the slice narrower than
`0026_sequence_and_ignore` by proving only one source-level direct char-stdout
helper call plus the smallest shared char-literal lowering needed to reach the
owned JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0128_print_char.ml`
  - `tests/fixtures/js/0128_print_char.core_ir.expected`
  - `tests/fixtures/js/0128_print_char.jir.expected`
  - `tests/fixtures/js/0128_print_char.js.expected`
  - `tests/fixtures/js/0128_print_char.pipeline.expected`
  - `tests/fixtures/js/0128_print_char.lowering.expected`
  - `tests/fixtures/js/0128_print_char.codegen.expected`
  - `tests/fixtures/js/0128_print_char.compilation.expected`
- invariant:
  - a source-level direct `print_char` call must lower through an explicit JS
    runtime helper import; emitted JS is still wrong if it prints a bare
    `print_char(':')` identifier or depends on an ambient global
  - this slice also needed the narrowest shared `Typ -> Core_ir` widening that
    keeps JS decisions out of the middle: char literals should become
    backend-neutral `Core_ir.Constant.Char` values instead of staying
    unsupported or being lowered straight to JS syntax
  - keep `ignore` and the broader `0026_sequence_and_ignore` fixture separate
    until the next missing gap is really sequence/ignore shaping instead of
    char literals or the direct JS runtime boundary
- ownership:
  - `src/core_ir.ml`
  - `src/core_ir.mli`
  - `src/core_ir_fixture_support.ml`
  - `src/typ_lowering.ml`
  - `src/example_pipeline.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/runtime.mli`
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/riot-runtime.js`
  - `src/native/nir/lowering.ml`
  - `tests/fixtures/corpus/0128_print_char.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0128_print_char.*.expected`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0128_print_char` now snapshots the first source-driven char/stdout helper
    example through shared `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    now carries `Core_ir.Constant.Char ":"`, while the JS path lowers that
    payload to a one-character JS string literal and lowers the direct call to
    `import { print_char as __print_char } from "./riot-runtime.js"` plus
    `__print_char(":")`
  - `compiler/raml`'s source pipeline now types `print_char` explicitly
    through its ambient example config instead of blocking the source-driven
    JS bins on an avoidable unbound-name diagnostic
  - the shared `Core_ir` change required one narrow compile-only native
    lowering accommodation so `compiler/raml` still builds, but native fixture
    migration remains out of scope for this JS slice
- validation:
  - the first targeted `riot test -p raml print_char` run exposed two narrow
    blockers rather than a wider backend failure: `Typ -> Core_ir` still
    rejected char literals, and the example/compilation lanes were still red
    on unbound name `print_char`
  - after adding shared char-constant lowering, the JS helper import path, and
    the example-pipeline ambient type, rerunning
    `riot test -p raml print_char` failed only because the seven
    `0128_print_char.*.expected` approvals were missing
  - after promoting those seven snapshots, `riot test -p raml print_char`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `bun run /tmp/raml-js-0128.XXXXXX/print_char.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `:`
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `0026_sequence_and_ignore` separate until the next missing invariant
    is really ignore/sequence shaping rather than char literals or stdout
    helper coverage
  - keep native char coverage separate until a deliberate shared-contract
    slice proves it is worth promoting beyond the current compile-only
    accommodation

### 2026-04-11: Source-Level Finite `float_of_string` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one string-to-float helper without
widening into invalid-input parsing, OCaml-exact float parsing, or richer
exception semantics.

- fixture:
  - `tests/fixtures/corpus/0127_float_of_string.ml`
  - `tests/fixtures/js/0127_float_of_string.core_ir.expected`
  - `tests/fixtures/js/0127_float_of_string.jir.expected`
  - `tests/fixtures/js/0127_float_of_string.js.expected`
  - `tests/fixtures/js/0127_float_of_string.pipeline.expected`
  - `tests/fixtures/js/0127_float_of_string.lowering.expected`
  - `tests/fixtures/js/0127_float_of_string.codegen.expected`
  - `tests/fixtures/js/0127_float_of_string.compilation.expected`
- invariant:
  - a source-level direct finite-input `float_of_string` call must lower
    through the same explicit JS runtime primitive boundary already used for
    arithmetic, string conversion, and valid-input `int_of_string`; emitted JS
    is still wrong if it prints `float_of_string("3.1415")`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the source-pipeline lane needs one matching ambient typing addition for
    the standalone example pipeline, but that stays a `compiler/raml` config
    choice instead of widening `Typ.Config.default`
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0127_float_of_string.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0127_float_of_string.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0127_float_of_string` now snapshots the first source-driven finite
    string-to-float parsing example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"float_of_string"`, while the JS path lowers
    it to `__callPrimitive("%float_of_string", "3.1415")`
  - `compiler/raml`'s source pipeline now types this helper explicitly through
    its ambient example config instead of blocking the source-driven JS bins on
    an avoidable unbound-name diagnostic
  - `./riot-runtime.js` now owns one narrow finite-input float parser; invalid
    input, OCaml-exact float parsing details, and exception semantics still
    stay out of scope for this slice
- validation:
  - `riot test -p raml float_of_string` initially failed only because the
    seven `0127_float_of_string.*.expected` snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml float_of_string`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `bun run /tmp/raml-js-0127.XXXXXX/float_of_string.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `3.1415`
  - `git diff --check -- compiler/raml` passed
- next:
  - keep invalid-input `float_of_string`, JS exception mapping, and `try/with`
    separate until a narrower fixture proves the next gap is still just the JS
    runtime boundary
  - keep `print_char` and `0026_sequence_and_ignore` separate until the next
    missing gap is really char/shared-lowering or ignore/sequence shaping

### 2026-04-11: Source-Level Finite `string_of_float` Direct Call Runtime Boundary Progress

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one float-to-string helper without
widening into OCaml-exact float formatting, `float_of_string`, or richer
float/stdout I/O.

- fixture:
  - `tests/fixtures/corpus/0126_string_of_float.ml`
  - `tests/fixtures/js/0126_string_of_float.core_ir.expected`
  - `tests/fixtures/js/0126_string_of_float.jir.expected`
  - `tests/fixtures/js/0126_string_of_float.js.expected`
  - `tests/fixtures/js/0126_string_of_float.pipeline.expected`
  - `tests/fixtures/js/0126_string_of_float.lowering.expected`
  - `tests/fixtures/js/0126_string_of_float.codegen.expected`
  - `tests/fixtures/js/0126_string_of_float.compilation.expected`
- invariant:
  - a source-level direct finite-input `string_of_float` call must lower
    through the same explicit JS runtime primitive boundary already used for
    arithmetic, string concatenation, `string_of_int`, and `int_of_string`;
    emitted JS is still wrong if it prints `string_of_float(3.1415)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the source-pipeline lane needed one matching ambient typing addition for
    the standalone example pipeline, but that stays a `compiler/raml` config
    choice instead of widening `Typ.Config.default`
  - keep OCaml-exact float formatting, `float_of_string`, `print_char`, and
    `0026_sequence_and_ignore` separate until a narrower fixture proves the
    next gap is still just the JS runtime boundary
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0126_string_of_float.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0126_string_of_float.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0126_string_of_float` now snapshots the first source-driven finite
    float-to-string conversion example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"string_of_float"`, while the JS path lowers
    it to `__callPrimitive("%string_of_float", 3.1415)`
  - `compiler/raml`'s source pipeline now types this helper explicitly through
    its ambient example config instead of blocking the source-driven JS bins on
    an avoidable unbound-name diagnostic
  - `./riot-runtime.js` now owns one narrow finite-input float-string helper;
    OCaml-exact formatting edge cases such as trailing `.0`, infinities, and
    NaN still stay out of scope for this slice
- validation:
  - `riot test -p raml string_of_float` initially failed only because the
    seven `0126_string_of_float.*.expected` snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml string_of_float`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `bun run /tmp/raml-js-0126/string_of_float.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `3.1415`
  - `git diff --check -- compiler/raml` passed
  - `riot test -p raml` currently fails only because the worktree already has
    eleven pending native approvals for
    `tests/fixtures/native/0073_trampoline_factorial.*.expected.new`; that is
    unrelated native approval debt, not new JS-lowering drift from this slice
- next:
  - keep OCaml-exact float formatting and `float_of_string` separate until a
    narrower fixture proves the current `%string_of_float` boundary is
    insufficient
  - keep `print_char` and `0026_sequence_and_ignore` separate until the next
    missing gap is really char/shared-lowering or ignore/sequence shaping

### 2026-04-11: Source-Level `print_string` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one string-stdout helper without
widening into `print_char`, `ignore`, or the broader
`0026_sequence_and_ignore` fixture.

- fixture:
  - `tests/fixtures/corpus/0125_print_string.ml`
  - `tests/fixtures/js/0125_print_string.core_ir.expected`
  - `tests/fixtures/js/0125_print_string.jir.expected`
  - `tests/fixtures/js/0125_print_string.js.expected`
  - `tests/fixtures/js/0125_print_string.pipeline.expected`
  - `tests/fixtures/js/0125_print_string.lowering.expected`
  - `tests/fixtures/js/0125_print_string.codegen.expected`
  - `tests/fixtures/js/0125_print_string.compilation.expected`
- invariant:
  - a source-level direct `print_string` call must lower through an explicit
    JS runtime helper import; emitted JS is still wrong if it prints a bare
    `print_string("hello")` identifier or depends on an ambient global
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the source-pipeline lane needed one matching ambient typing addition for
    the standalone example pipeline, but that stays a `compiler/raml` config
    choice instead of widening `Typ.Config.default`
  - keep `print_char` and `0026_sequence_and_ignore` separate until a narrower
    fixture proves the next gap is still just the JS runtime boundary
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/runtime.mli`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0125_print_string.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0125_print_string.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0125_print_string` now snapshots the first source-driven string-stdout
    helper example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"print_string"`, while the JS path lowers it
    to `import { print_string as __print_string } from "./riot-runtime.js"`
    plus `__print_string("hello")`
  - `compiler/raml`'s standalone example pipeline now types this helper
    explicitly through its ambient example config instead of blocking the
    source-driven pipeline/compilation lanes on an avoidable unbound-name
    diagnostic
  - `./riot-runtime.js` now owns one narrow string-stdout helper that writes
    the provided string without a trailing newline, so the current JS runtime
    surface no longer has to fake this slice through `print_endline`,
    `Printf.printf`, or ambient globals
- validation:
  - the first targeted `riot test -p raml print_string` run exposed two narrow
    gaps rather than a wider backend failure: the direct JS lane still emitted
    a bare `print_string("hello")` call, and the example/compilation lanes
    were still red on unbound name `print_string`
  - after adding the JS helper import path and the example-pipeline ambient
    type, rerunning `riot test -p raml print_string` failed only because the
    seven `0125_print_string.*.expected` approvals were missing
  - after promoting those seven snapshots, `riot test -p raml print_string`
    passed
  - the first full `riot test -p raml` pass then failed only because the
    shared source fixture also expanded the existing native corpus lane and
    the eleven `tests/fixtures/native/0125_print_string.*.expected` approvals
    were still pending; that was native approval debt for the shared corpus
    example, not a new JS-lowering regression
  - after promoting those eleven native approvals, `riot test -p raml`
    passed
  - `bun run /tmp/raml-js-0125/print_string.mjs` passed after materializing
    the emitted module next to `src/js/riot-runtime.js`, printing `hello`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `print_char` separate until a narrower fixture proves the next
    missing gap is still just the JS runtime boundary
  - keep `0026_sequence_and_ignore` separate until those narrower stdout
    helper boundaries land and the remaining problem is really sequence/ignore
    shaping rather than stdout coverage

### 2026-04-11: Source-Level `print_int` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one integer-stdout helper without
widening into `print_string`, `print_char`, `ignore`, or the broader
`0026_sequence_and_ignore` fixture.

- fixture:
  - `tests/fixtures/corpus/0124_print_int.ml`
  - `tests/fixtures/js/0124_print_int.core_ir.expected`
  - `tests/fixtures/js/0124_print_int.jir.expected`
  - `tests/fixtures/js/0124_print_int.js.expected`
  - `tests/fixtures/js/0124_print_int.pipeline.expected`
  - `tests/fixtures/js/0124_print_int.lowering.expected`
  - `tests/fixtures/js/0124_print_int.codegen.expected`
  - `tests/fixtures/js/0124_print_int.compilation.expected`
- invariant:
  - a source-level direct `print_int` call must lower through an explicit JS
    runtime helper import; emitted JS is still wrong if it prints a bare
    `print_int(42)` identifier or depends on an ambient global
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the source-pipeline lane needed one matching ambient typing addition for
    the standalone example pipeline, but that stays a `compiler/raml` config
    choice instead of widening `Typ.Config.default`
  - keep `print_string`, `print_char`, and `0026_sequence_and_ignore`
    separate until a narrower fixture proves the next gap is still just the JS
    runtime boundary
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/runtime.mli`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0124_print_int.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0124_print_int.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0124_print_int` now snapshots the first source-driven integer-stdout
    helper example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"print_int"`, while the JS path lowers it to
    `import { print_int as __print_int } from "./riot-runtime.js"` plus
    `__print_int(42)`
  - `compiler/raml`'s standalone example pipeline now types this helper
    explicitly through its ambient example config instead of blocking the
    source-driven pipeline/compilation lanes on an avoidable unbound-name
    diagnostic
  - `./riot-runtime.js` now owns one narrow integer-stdout helper that writes
    digits without a trailing newline, so the current JS runtime surface no
    longer has to fake this slice through `print_endline` or ambient globals
  - the fixture was renumbered to `0124_print_int` instead of `0123_print_int`
    so the source corpus keeps a stable ordering alongside the existing
    `0123_module_identity` slice
- validation:
  - the first targeted `riot test -p raml print_int` run exposed two narrow
    gaps rather than a wider backend failure: the direct JS lane still emitted
    a bare `print_int(42)` call, and the example/compilation lanes were still
    red on unbound name `print_int`
  - after adding the JS helper import path and the example-pipeline ambient
    type, rerunning `riot test -p raml print_int` failed only because the
    seven `0124_print_int.*.expected` approvals were missing
  - after promoting those seven snapshots, `riot test -p raml print_int`
    passed
  - `bun run /tmp/raml-js-0124/print_int.mjs` passed after materializing the
    emitted module next to `src/js/riot-runtime.js`, printing `42`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `print_string` and `print_char` separate until a narrower fixture
    proves the next missing gap is still just the JS runtime boundary
  - keep `0026_sequence_and_ignore` separate until those narrower stdout
    helper boundaries land and the remaining problem is really sequence/ignore
    shaping rather than stdout coverage

### 2026-04-11: Source-Level Valid-Input `int_of_string` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one parsing helper without widening
into `try/with`, parse-failure semantics, or richer string/stdout I/O.

- fixture:
  - `tests/fixtures/corpus/0122_int_of_string.ml`
  - `tests/fixtures/js/0122_int_of_string.core_ir.expected`
  - `tests/fixtures/js/0122_int_of_string.jir.expected`
  - `tests/fixtures/js/0122_int_of_string.js.expected`
  - `tests/fixtures/js/0122_int_of_string.pipeline.expected`
  - `tests/fixtures/js/0122_int_of_string.lowering.expected`
  - `tests/fixtures/js/0122_int_of_string.codegen.expected`
  - `tests/fixtures/js/0122_int_of_string.compilation.expected`
- invariant:
  - a source-level valid-input direct `int_of_string` call must lower through
    the same explicit JS runtime primitive boundary already used for
    arithmetic, comparisons, tuples, and string conversion; emitted JS is
    still wrong if it prints `int_of_string("42")`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the source-pipeline lane also needed the same narrow ambient typing
    treatment as `string_of_int`: `compiler/raml`'s example pipeline did not
    yet provide the `string -> int` helper type for `int_of_string`, so the
    source-driven pipeline/compilation snapshots needed one explicit ambient
    addition without widening `Typ.Config.default`
  - keep parse-failure behavior, JS exception mapping, `try/with`, and
    `0011_result_pipeline` separate until a narrower fixture proves the next
    gap is still just the JS runtime boundary
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0122_int_of_string.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0122_int_of_string.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0122_int_of_string` now snapshots the first source-driven valid-input
    string-to-int parsing example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"int_of_string"`, while the JS path lowers it
    to `__callPrimitive("%int_of_string", "42")`
  - `compiler/raml`'s source pipeline now types this helper explicitly through
    its ambient example config instead of blocking the source-driven JS bins on
    an avoidable unbound-name diagnostic
  - `./riot-runtime.js` now owns one narrow decimal-string implementation for
    `%int_of_string`; invalid-input and exception semantics still stay out of
    scope for this slice
- validation:
  - `riot test -p raml int_of_string` initially failed only because the seven
    `0122_int_of_string.*.expected` snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml int_of_string`
    passed
  - the first full `riot test -p raml` pass then failed only because the
    shared source fixture also expanded the existing native corpus lane and the
    eleven `tests/fixtures/native/0122_int_of_string.*.expected` approvals were
    still pending; that was native approval debt for the shared corpus example,
    not a new JS-lowering regression
  - after promoting those eleven native approvals, `riot test -p raml` passed
  - `bun run /tmp/raml-js-0122/int_of_string.mjs` passed after materializing
    the emitted module next to `src/js/Printf.js` and `src/js/riot-runtime.js`,
    printing `42`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep invalid-input parsing, JS exception mapping, and `try/with` separate
    until a narrower fixture proves the next gap is still just the JS runtime
    boundary
  - keep `print_string`, `print_int`, and `print_char` separate until a
    narrower fixture proves the next gap is stdout/runtime shaping rather than
    shared `char` support or richer I/O semantics

### 2026-04-11: Source-Level `print_newline` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one newline-only stdout helper
without widening into `print_string`, `print_int`, `print_char`, or `try/with`.

- fixture:
  - `tests/fixtures/corpus/0121_print_newline.ml`
  - `tests/fixtures/js/0121_print_newline.core_ir.expected`
  - `tests/fixtures/js/0121_print_newline.jir.expected`
  - `tests/fixtures/js/0121_print_newline.js.expected`
  - `tests/fixtures/js/0121_print_newline.pipeline.expected`
  - `tests/fixtures/js/0121_print_newline.lowering.expected`
  - `tests/fixtures/js/0121_print_newline.codegen.expected`
  - `tests/fixtures/js/0121_print_newline.compilation.expected`
- invariant:
  - a source-level direct `print_newline ()` call must lower through an
    explicit JS runtime helper import; emitted JS is still wrong if it prints
    a bare `print_newline` identifier or depends on an ambient global
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep `print_string`, `print_int`, `print_char`, and richer stdout
    semantics separate until a narrower fixture proves the current
    `print_newline` boundary is insufficient
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/runtime.mli`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0121_print_newline.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0121_print_newline.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0121_print_newline` now snapshots the first source-driven newline-only
    stdout helper example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"print_newline"`, while the JS path lowers it
    to `import { print_newline as __print_newline } from "./riot-runtime.js"`
    plus `__print_newline(undefined)`
  - `compiler/raml`'s source pipeline now types this helper explicitly through
    its ambient example config instead of blocking the source-driven JS bins on
    an avoidable unbound-name diagnostic
- validation:
  - `riot test -p raml print_newline` initially failed only because the seven
    `0121_print_newline.*.expected` snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml print_newline`
    passed
  - `bun run /tmp/raml-js-0121/print_newline.mjs` passed after materializing
    the emitted module next to `src/js/riot-runtime.js`, printing one blank
    line
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `print_string`, `print_int`, and `print_char` separate until a
    narrower fixture proves the next gap is still just the JS runtime boundary
    rather than shared `char` support or stdout-buffer semantics
  - keep `int_of_string` separate until a narrower fixture proves the next gap
    is just the JS runtime boundary rather than `try/with` or exception
    semantics

### 2026-04-11: Source-Level `string_of_int` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface with one source-visible conversion helper
without widening into `try/with`, `int_of_string`, or richer string I/O.

- fixture:
  - `tests/fixtures/corpus/0120_string_of_int.ml`
  - `tests/fixtures/js/0120_string_of_int.core_ir.expected`
  - `tests/fixtures/js/0120_string_of_int.jir.expected`
  - `tests/fixtures/js/0120_string_of_int.js.expected`
  - `tests/fixtures/js/0120_string_of_int.pipeline.expected`
  - `tests/fixtures/js/0120_string_of_int.lowering.expected`
  - `tests/fixtures/js/0120_string_of_int.codegen.expected`
  - `tests/fixtures/js/0120_string_of_int.compilation.expected`
- invariant:
  - a source-level direct `string_of_int` call must lower through the same
    explicit JS runtime primitive boundary already used for arithmetic,
    comparisons, string concatenation, tuples, and tracing; emitted JS is
    still wrong if it prints `string_of_int(42)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    source helper as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - the first targeted run also exposed one narrower source-pipeline blocker
    that belongs in `compiler/raml`, not in `packages/typ`: the example
    pipeline's ambient typing config did not yet provide the `int -> string`
    type for `string_of_int`, so source-driven pipeline/compilation snapshots
    were blocked on an unbound-name typing diagnostic even though direct
    `Core_ir -> JIR` lowering could still sketch the intended call shape
  - keep `int_of_string`, `string_of_float`, `print_string`, `print_int`,
    `print_char`, `print_newline`, and `0011_result_pipeline` separate until a
    narrower fixture proves the current `%string_of_int` boundary is
    insufficient
- ownership:
  - `src/example_pipeline.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0120_string_of_int.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0120_string_of_int.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0120_string_of_int` now snapshots the first source-driven integer-to-
    string conversion example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: shared `Core_ir`
    still carries direct callee `"string_of_int"`, while the JS path lowers it
    to `__callPrimitive("%string_of_int", 42)` and keeps `print_endline` on
    the existing explicit sibling-runtime import path
  - `compiler/raml`'s source pipeline now types this helper explicitly through
    its ambient example config instead of blocking the source-driven JS bins on
    an avoidable unbound-name diagnostic
- validation:
  - the first targeted `riot test -p raml string_of_int` run generated the
    seven JS snapshot candidates for `0120_string_of_int` and exposed the real
    source-pipeline blocker in those candidates: typing was still red on
    unbound name `string_of_int`
  - after adding the ambient `int -> string` helper type in
    `src/example_pipeline.ml` and routing JS direct-call lowering through
    `%string_of_int`, rerunning `riot test -p raml string_of_int` failed only
    because the seven approvals above were still missing
  - after promoting those seven snapshots, `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0120/string_of_int.mjs` passed after materializing
    the emitted module next to `src/js/riot-runtime.js`, printing `42`
- next:
  - keep `int_of_string` separate until a narrower fixture proves the next gap
    is just the JS runtime boundary rather than `try/with` or exception
    semantics
  - keep richer string/int I/O helpers separate until one narrower fixture
    proves the current `%string_of_int` boundary is insufficient

### 2026-04-11: Source-Level `^` Direct Call Runtime Boundary Landed

Picked immediate next target 3 again in the narrowest source-driven slice that
extends the owned JS runtime surface without widening into `try/with`,
`external`, or source-visible string conversion helpers.

- fixture:
  - `tests/fixtures/corpus/0119_string_concat.ml`
  - `tests/fixtures/js/0119_string_concat.core_ir.expected`
  - `tests/fixtures/js/0119_string_concat.jir.expected`
  - `tests/fixtures/js/0119_string_concat.js.expected`
  - `tests/fixtures/js/0119_string_concat.pipeline.expected`
  - `tests/fixtures/js/0119_string_concat.lowering.expected`
  - `tests/fixtures/js/0119_string_concat.codegen.expected`
  - `tests/fixtures/js/0119_string_concat.compilation.expected`
- invariant:
  - a source-level direct `^` call must lower through the same explicit JS
    runtime primitive boundary already used for arithmetic and comparisons;
    emitted JS is still wrong if it prints `^("hello, ", "world")`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: `Core_ir` should keep exposing the source
    operator as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep `string_of_int`, `int_of_string`, richer string stdlib coverage, and
    `0011_result_pipeline` separate until a narrower fixture proves they are
    involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0119_string_concat.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0119_string_concat.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0119_string_concat` now snapshots the first source-driven string
    concatenation example through `Core_ir`, `JIR`,
    `Raml.Example_pipeline`, `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended split directly: `Core_ir` still
    carries direct callee `"^"`, while the JS path lowers it to
    `__callPrimitive("%concatstring", "hello, ", "world")`
  - JS direct-call lowering now routes `^` through `%concatstring` in
    `./riot-runtime.js`, keeping the emitter unchanged and the runtime/helper
    choice owned by `src/js/`
- validation:
  - `riot test -p raml string_concat` initially failed only because the seven
    `0119_string_concat.*.expected` snapshots were missing, which confirmed
    the new example already reached every owned JS snapshot surface
  - after promoting those seven snapshots, `riot test -p raml string_concat`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0119.XXXXXX/string_concat.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `hello, world`
- next:
  - keep `0011_result_pipeline` separate until a narrower fixture proves the
    current missing gap is shared `try/with`, result-pattern lowering, or
    another source-visible string/int helper boundary
  - keep typed `external` work, string conversion helpers, and richer stdlib
    string coverage separate until one narrower fixture proves the current
    `%concatstring` boundary is insufficient

### 2026-04-11: `0002_integer_arithmetic` JS Coverage Landed

Picked a narrow source-driven JS coverage slice around the existing shared
corpus arithmetic example instead of widening into more runtime or FFI work.

- fixture:
  - `tests/fixtures/corpus/0002_integer_arithmetic.ml`
  - `tests/fixtures/js/0002_integer_arithmetic.core_ir.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.jir.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.js.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.pipeline.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.lowering.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.codegen.expected`
  - `tests/fixtures/js/0002_integer_arithmetic.compilation.expected`
- invariant:
  - the existing shared straight-line integer arithmetic example should reach
    the owned JS source-driven bins too, not only the native lane
  - this slice stays coverage-only unless the current backend proves
    insufficient: shared `Core_ir` should keep direct callees `+`, `-`, `*`,
    `/`, and `mod`, while JS lowering should keep routing them through the
    existing `%addint`, `%subint`, `%mulint`, `%divint`, and `%modint`
    `callPrimitive` runtime boundary
  - keep richer integer semantics, comparison follow-ups, and any new runtime
    helper work separate unless this narrower fixture exposes a real gap
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0002_integer_arithmetic.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suites now cover `0002_integer_arithmetic` instead of
    leaving that arithmetic proof point native-only
  - the approved snapshots show the intended split directly: shared
    `Core_ir` keeps direct integer operator callees, while JS lowering emits
    `__callPrimitive("%addint" | "%subint" | "%mulint" | "%divint" |
    "%modint", ...)`
  - no compiler, runtime, or emitter changes were needed; the current JS path
    was already sufficient once the seven missing approvals were promoted
- validation:
  - `riot test -p raml integer_arithmetic` initially failed only because the
    seven JS approvals above were missing; native `0002_integer_arithmetic`
    coverage stayed green and unchanged
  - after promoting those seven approvals, `riot test -p raml integer_arithmetic`
    passed
  - `bun run /tmp/raml-js-0002.XXXXXX/integer_arithmetic.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42 121 17 2`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `0006_let_shadowing` as the first arithmetic-plus-shadowing proof
    point and keep `0002_integer_arithmetic` as the first straight-line
    top-level arithmetic proof point
  - keep broader integer/runtime work separate until a narrower fixture proves
    the current `callPrimitive` boundary is insufficient

### 2026-04-11: `0049_function_composition_pipeline` Shared Higher-Order Coverage Landed

The earlier approval-blocker note is now stale. The shared
`0049_function_composition_pipeline` slice is green across the JS source-driven
bins and the native pass bins.

- fixture:
  - `tests/fixtures/corpus/0049_function_composition_pipeline.ml`
  - `tests/fixtures/js/0049_function_composition_pipeline.*.expected`
  - `tests/fixtures/native/0049_function_composition_pipeline.*.expected`
- invariant:
  - the shared higher-order composition example should project coherently
    through the JS source-driven bins and the native pass bins without
    inventing a JS-only encoding or regressing the existing native path
  - expression-position anonymous `fun` values should lower through shared
    `Core_ir.Lambda`, while calls through higher-order parameters such as `f`
    and `g` stay indirect and calls to the known top-level `compose` binding
    stay direct
  - on the JS side, that higher-order shape should keep reusing the existing
    `makeCurried` runtime boundary instead of teaching the emitter new
    currying or partial-application tricks
- ownership:
  - `src/typ_lowering.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/native_fixture_tests.ml`
  - `tests/fixtures/js/0049_function_composition_pipeline.*.expected`
  - `tests/fixtures/native/0049_function_composition_pipeline.*.expected`
  - `JS_LOOP.md`
- effect:
  - the worktree is no longer blocked on pending `0049` approvals; the example
    is green across `core_ir`, `jir`, `js`, `pipeline`, `lowering`,
    `codegen`, `compilation`, `nir`, `nir.normalize`, `nir.simplify`, `mir`,
    `mir.canonicalize`, `mir.insert_polls`, `lir`, `lir.layout_frames`,
    `lir.schedule`, `native`, and `link`
  - the approved JS snapshots now show the intended higher-order split
    directly: `compose` lowers through `__makeCurried`, the anonymous `fun`
    arguments lower to plain JS function expressions, and the emitted module
    stays runnable under `bun`
  - the approved native snapshots keep the same source example honest through
    the existing closure/lifted-wrapper path instead of a JS-only shortcut
- validation:
  - `riot build raml` passed
  - `riot test -p raml function_composition_pipeline` passed
  - `bun run /tmp/raml-js-0049.XXXXXX/function_composition_pipeline.mjs`
    passed after materializing the emitted module next to `src/js/Printf.js`
    and `src/js/riot-runtime.js`, printing `45`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep broader higher-order call analysis, tree shaking, and import/shaping
    follow-ups separate until a narrower fixture proves the current direct
    versus indirect call split is insufficient

### 2026-04-11: `0005_if_then_else` JS Coverage Started, Then Blocked By `typ`

Picked a narrow source-driven JS coverage slice around the simplified shared
corpus example `0005_if_then_else`, but stopped before claiming progress as
landed because package validation is currently blocked outside `compiler/raml`.

- fixture:
  - `tests/fixtures/corpus/0005_if_then_else.ml`
- invariant:
  - the existing minimal conditional corpus example should reach the owned JS
    source-driven fixture bins too, not only the native lane
  - this slice is coverage-only unless the current backend proves insufficient:
    shared `Core_ir.If_then_else`, existing `JIR` conditional lowering, and
    the current `Printf.printf` / `riot-runtime.js` boundary should already be
    enough for `let choose cond = if cond then 1 else 0`
  - keep broader conditional rewrites, native pass churn, and new runtime work
    separate unless this narrower fixture exposes a real JS lowering gap
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `JS_LOOP.md`
- effect so far:
  - added `if_then_else` to the JS/example/compilation fixture filters so the
    shared corpus example is queued for JS approval coverage once the package
    builds again
  - no compiler, runtime, or emitter changes were needed yet; this slice is
    still testing whether the existing backend already owns the simpler
    conditional proof point
- validation:
  - `riot test -p raml if_then_else` did not reach `raml`; it failed while
    rebuilding the unrelated `typ` package because the current worktree has an
    in-flight `Array`/`CArray` rename under `packages/typ/src/session/`
  - the observed blocker at the time of the retry was:
    `typ: Error: Unbound module Array` with hint `Did you mean CArray?`
  - because `typ` failed before `raml` rebuilt, no JS snapshot candidates were
    generated for `0005_if_then_else` yet
  - `git diff --check -- compiler/raml` passed
- next:
  - once the unrelated `typ` build break is resolved, rerun
    `riot test -p raml if_then_else` to generate the seven JS approvals for:
    `core_ir`, `jir`, `js`, `pipeline`, `lowering`, `codegen`, and
    `compilation`
  - if those approvals land cleanly, materialize the emitted module next to
    `src/js/Printf.js` and `src/js/riot-runtime.js`, then run it with `bun`

### 2026-04-11: `0005_if_then_else` JS Coverage Landed

Picked up the earlier blocked `0005_if_then_else` coverage-only slice after the
unrelated `typ` failure stopped reproducing and finished it without widening
the problem beyond the already-simplified shared corpus example.

- fixture:
  - `tests/fixtures/corpus/0005_if_then_else.ml`
  - `tests/fixtures/js/0005_if_then_else.core_ir.expected`
  - `tests/fixtures/js/0005_if_then_else.jir.expected`
  - `tests/fixtures/js/0005_if_then_else.js.expected`
  - `tests/fixtures/js/0005_if_then_else.pipeline.expected`
  - `tests/fixtures/js/0005_if_then_else.lowering.expected`
  - `tests/fixtures/js/0005_if_then_else.codegen.expected`
  - `tests/fixtures/js/0005_if_then_else.compilation.expected`
- invariant:
  - the simplified shared corpus example `let choose cond = if cond then 1 else 0`
    should reach the owned JS source-driven fixture bins too, not only the
    native lane
  - this slice stays coverage-only unless the current backend proves
    insufficient: shared `Core_ir.If_then_else`, existing `JIR` conditional
    lowering, and the current `Printf.printf` sibling-module boundary should
    already be enough for the first minimal conditional proof point
  - keep broader conditional rewrites, native pass churn, and new runtime work
    separate unless this narrower fixture exposes a real JS lowering gap
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0005_if_then_else.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suites now cover the shared `0005_if_then_else`
    example instead of leaving that minimal conditional proof point native-only
  - the approved snapshots show the intended shared/backend split directly:
    shared `Core_ir` keeps one tail-position `If_then_else`, while JS lowering
    preserves it as a structured function-body `if` with branch-local returns
    before final emission
  - no compiler, runtime, or emitter changes were needed; the current backend
    was already sufficient once the seven missing JS approvals were promoted
- validation:
  - rerunning `riot test -p raml if_then_else` reached `raml` cleanly and
    failed only because the seven JS approvals above were missing; native
    `0005_if_then_else` coverage stayed green and unchanged
  - after promoting those seven approvals, `bun run
    /tmp/raml-js-0005.XXXXXX/if_then_else.mjs` passed after materializing the
    emitted module next to `src/js/Printf.js`, printing `1`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `0005_if_then_else` as the first minimal JS-owned source-driven proof
    point for the already-landed conditional lowering path
  - keep broader conditional rewrites, extra control-flow examples, and any
    future flattening follow-up separate until a narrower fixture proves the
    current structured `if` lowering is insufficient

### 2026-04-11: Mutual Recursion Plus Short-Circuit Boolean Coverage Landed

Picked immediate next target 6 again, but kept the slice narrower than new
recursion lowering work by reusing the existing shared corpus example and only
proving that the current JS backend stays honest when top-level mutual
recursion combines source-level `=`, `<>`, `||`, and `&&`.

- fixture:
  - `tests/fixtures/corpus/0014_mutual_recursion_even_odd.ml`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.core_ir.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.jir.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.js.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.pipeline.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.lowering.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.codegen.expected`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.compilation.expected`
- invariant:
  - once the shared source program already lowers one top-level recursive
    binding group, the JS backend should keep that group explicit through the
    current `let`-prelude-and-assignment strategy even when one branch uses
    short-circuit `||` over `=` and the other uses short-circuit `&&` over
    `<>`
  - this slice is still JS-owned coverage, not a new shared lowering or
    runtime contract: `Core_ir` keeps exposing direct callees `"="`, `"<>"`,
    `"||"`, and `"&&"`, while JS lowering keeps choosing `%eq`, `%neq`, and
    nested conditional expressions before emission
  - keep broader recursion rewrites, tailcall strategy, and standalone source-
    driven `%neq` coverage separate until a narrower fixture proves the
    current runnable path is insufficient
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0014_mutual_recursion_even_odd.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suites now cover the existing shared corpus example
    `0014_mutual_recursion_even_odd` instead of leaving that proof point
    native-only
  - the approved snapshots show the intended split directly: shared `Core_ir`
    keeps one recursive binding group, while JS lowering materializes
    `let even; let odd; even = function ...; odd = function ...;` and keeps
    `||` / `&&` as nested conditionals with `%eq` / `%neq` calls in the
    condition position
  - no compiler or runtime code changes were needed; the current JS lowering,
    runtime boundary, and mutual-recursion strategy were already sufficient
    once the source-driven coverage existed
- validation:
  - `riot test -p raml mutual_recursion_even_odd` initially failed only
    because the seven JS approvals above were missing; native `0014` coverage
    stayed green and unchanged
  - after promoting those seven snapshots, `bun run
    /tmp/raml-js-0014/mutual_recursion_even_odd.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true true`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `0114_top_level_mutual_recursion` as the first minimal mutual-
    recursion proof point and keep `0014_mutual_recursion_even_odd` as the
    first composite proof point that also exercises `<>` plus short-circuit
    boolean control flow inside that recursive shape
  - keep richer boolean recursion, tailcall rewrites, and tree-shaking work
    separate until a narrower fixture proves the current lowering is
    insufficient

### 2026-04-11: Late JIR Import Materialization Landed

Picked pass-ladder item 2 in the narrowest late-JIR slice that stops making
`JST` own import-reference materialization and landed it after regenerating
the stale JS snapshot candidates from before the `JST` re-normalization fix.

- fixture:
  - `tests/fixtures/corpus/0118_printf_and_print_endline.ml`
- invariant:
  - after the final JS-owned `JIR` normalize step has already discovered the
    backend import requirements, the final `JIR` body should reference those
    bindings by plain local identifiers instead of keeping `Imported` and
    `Runtime_helper` nodes all the way into `JST` lowering
  - this is a late JS-owned import-materialization choice in `src/js/`, not a
    `Core_ir` change and not an emitter trick: the emitter should only see
    already-materialized JS locals plus the collected `program.imports`
  - keep richer dependency provenance, package-path policy, and earlier
    import-discovery passes separate until a narrower fixture proves the late
    materialization boundary itself is insufficient
- ownership:
  - `src/js/jir/passes/materialize_imports.ml`
  - `src/js/jir/passes/materialize_imports.mli`
  - `src/js/jir/passes/passes.ml`
  - `src/js/jir/passes/passes.mli`
  - `src/js/jir/lowering.ml`
  - `src/js/jst/lowering.ml`
  - `tests/fixtures/corpus/0118_printf_and_print_endline.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
- effect:
  - a new late `Js.Jir.Passes.Materialize_imports` pass now rewrites
    `Imported requirement` and `Runtime_helper helper` expressions into plain
    identifier references after the final import-collection normalize step, so
    the final `JIR` contract is closer to "imports in `program.imports`,
    references in the body"
  - the dedicated source fixture `0118_printf_and_print_endline` is the
    smallest current source proof point that exercises both sides of that
    boundary at once: one source-visible namespace import through
    `Printf.printf`, plus one runtime-helper import through `print_endline`
  - the first implementation draft exposed a real ownership bug rather than a
    snapshot-only drift: `JST` lowering was still calling
    `Source.Passes.Normalize.program`, which dropped the already-materialized
    import list and produced JS missing its import declarations; that
    re-normalization step was removed so final `JIR` now owns import
    collection/materialization
- validation:
  - the first targeted `riot test -p raml printf_and_print_endline` run
    generated the expected seven JS snapshot candidates for the new source
    fixture, but those candidates were stale because they were produced before
    the `JST` re-normalization bug was fixed
  - a broader `riot test -p raml` run exposed that stale-candidate problem
    directly: older `.expected.new` files in `tests/fixtures/js/` still showed
    missing top-level imports even though the current final `JIR` still
    carried `program.imports`
  - after clearing those stale JS `.expected.new` files and rerunning the full
    package, the remaining snapshot drift collapsed to the intended contract
    change only: final `JIR`, lowering, pipeline, codegen, and compilation
    snapshots now reference imports and runtime helpers through plain
    identifiers in the body while keeping collected import requirements in
    `program.imports`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0118/printf_and_print_endline.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42` then `done`
- next:
  - keep earlier dependency discovery and package-path policy separate; this
    landed slice only settles the late "body references become plain locals
    before `JST`" invariant
  - if a later slice needs richer dependency provenance or per-import policy,
    prove that with a narrower fixture instead of widening this late
    materialization boundary implicitly

### 2026-04-11: Source-Driven Boolean Short-Circuit Lowering Landed

Picked the smallest existing-corpus direct-call slice that made the current
JS backend honest about boolean control flow without inventing a fake runtime
primitive or a smarter emitter.

- fixture:
  - `tests/fixtures/corpus/0004_boolean_logic.ml`
  - `tests/fixtures/js/0004_boolean_logic.core_ir.expected`
  - `tests/fixtures/js/0004_boolean_logic.jir.expected`
  - `tests/fixtures/js/0004_boolean_logic.js.expected`
  - `tests/fixtures/js/0004_boolean_logic.pipeline.expected`
  - `tests/fixtures/js/0004_boolean_logic.lowering.expected`
  - `tests/fixtures/js/0004_boolean_logic.codegen.expected`
  - `tests/fixtures/js/0004_boolean_logic.compilation.expected`
- invariant:
  - source-level direct `not`, `&&`, and `||` calls must lower to runnable JS;
    emitted JS is still wrong if it prints bare `not`, `&&`, or `||`
    identifiers as ordinary calls
  - `&&` and `||` must preserve short-circuit behavior in the JS backend, so
    this first slice must not route them through `callPrimitive` or any other
    runtime helper that would eagerly evaluate both operands
  - this is a JS-owned `JIR` lowering choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep exposing the
    original direct callee names while `src/js/` chooses how those boolean
    operators become runnable JS
- ownership:
  - `src/js/jir/lowering.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0004_boolean_logic.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0004_boolean_logic` now snapshots the first source-driven boolean-logic
    slice through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final emitted JS instead of leaving direct boolean
    operator lowering unproved in the JS lane
  - JS direct-call lowering now materializes `not`, `&&`, and `||` through
    nested `JIR` conditional expressions such as
    `(q ? false : true)`, `(left ? right : false)`, and `(left ? true : right)`
    so short-circuit behavior stays explicit before `JST` lowering
  - the existing runtime surface stayed narrow: comparisons such as `<` still
    lower through `callPrimitive("%lt", ...)`, `<>` still lowers through
    `%neq` when covered elsewhere, and this slice added no new runtime helper
- validation:
  - `riot test -p raml boolean_logic` initially failed only because the seven
    `0004_boolean_logic.*.expected` JS snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml boolean_logic`
    passed
  - `bun run /tmp/raml-js-0004/boolean_logic.mjs` passed after materializing
    the emitted module next to `src/js/Printf.js` and `src/js/riot-runtime.js`,
    printing `true`
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep partial application or first-class value use of `not`, `&&`, and `||`
    separate until a narrower fixture proves the current direct-call lowering
    is insufficient
  - keep broader recursion-plus-boolean coverage separate; `0114` still owns
    the first top-level mutual-recursion proof point, while `0004` now owns
    the first source-driven JS short-circuit boolean-operator proof point

### 2026-04-11: Float Arithmetic And `sqrt` Runtime Boundary Progress

Picked immediate next target 3 in the narrowest existing-corpus slice that
closed the remaining ambient-runtime gap for direct float operators and
top-level `sqrt`.

- fixture:
  - `tests/fixtures/corpus/0003_float_arithmetic.ml`
  - `tests/fixtures/js/0003_float_arithmetic.core_ir.expected`
  - `tests/fixtures/js/0003_float_arithmetic.jir.expected`
  - `tests/fixtures/js/0003_float_arithmetic.js.expected`
  - `tests/fixtures/js/0003_float_arithmetic.pipeline.expected`
  - `tests/fixtures/js/0003_float_arithmetic.lowering.expected`
  - `tests/fixtures/js/0003_float_arithmetic.codegen.expected`
  - `tests/fixtures/js/0003_float_arithmetic.compilation.expected`
- invariant:
  - source-level direct `+.`, `-.`, `*.`, `/.`, and `sqrt` calls must lower
    through the owned JS runtime boundary instead of emitting bare float
    operator identifiers or an ambient `sqrt` call
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: shared `Core_ir` should keep carrying the
    original direct callee names while `src/js/` chooses how they become
    runnable JS
  - keep richer float stdlib coverage, structural float comparison, and
    typed `external` provenance separate until a narrower fixture proves the
    current runtime boundary is insufficient
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0003_float_arithmetic.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the existing shared corpus example `0003_float_arithmetic` now has JS
    coverage through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final emitted JS instead of staying native-only
  - JS direct-call lowering now routes `+.` / `-.` / `*.` / `/.` through
    `%addfloat` / `%subfloat` / `%mulfloat` / `%divfloat`, and routes
    `sqrt` through `%sqrtfloat` in `./riot-runtime.js`
  - emitted JS for the covered float slice is now explicit and runnable:
    `const y = __callPrimitive("%sqrtfloat", __callPrimitive("%addfloat", ...));`
    instead of relying on bare `+.` / `*.` identifiers or an ambient `sqrt`
- validation:
  - `riot test -p raml float_arithmetic` initially failed only because the
    seven JS snapshots above were missing, while the native `float_arithmetic`
    lane stayed green and unchanged
  - after promoting those seven JS snapshots, `riot test -p raml float_arithmetic`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - an earlier full-package pass was still blocked by 11 unrelated native
    approval snapshots for `0022_local_functions_and_closures`, not by the new
    `0003_float_arithmetic` JS slice; after those native approvals were
    promoted, `riot test -p raml` passed for the full package again
  - `bun run /tmp/raml-js-0003/float_arithmetic.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `5.0000 5.4772`
- next:
  - keep richer float math, structural float comparison, and typed foreign
    bindings separate until a narrower fixture proves the current
    `callPrimitive` boundary is insufficient
  - keep native fixture migration separate; this slice only added JS coverage
    for an existing shared corpus example and did not change the native lane

### 2026-04-11: First Dead-Binding Elimination Slice Landed

Picked pass-ladder item 6 in the narrowest JS-owned slice that removes dead
immutable local bindings without widening into effect analysis for calls,
assignments, or export rewriting.

- fixture:
  - `tests/fixtures/corpus/0117_dead_local_bindings.ml`
  - `tests/fixtures/js/0117_dead_local_bindings.core_ir.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.jir.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.js.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.pipeline.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.lowering.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.codegen.expected`
  - `tests/fixtures/js/0117_dead_local_bindings.compilation.expected`
  - `tests/fixtures/jir_lowering/constants_and_direct_calls.json`
  - `tests/fixtures/js/constants_and_direct_calls.expected`
- invariant:
  - after alpha stabilization and alias cleanup, an unexported immutable
    `const` binding should disappear from `JIR` when its name is unused in the
    current scope and its initializer is already effect-free
  - this first slice keeps the purity boundary narrow and JS-owned: literals,
    identifiers, imports/runtime-helper references, member access over pure
    expressions, pure conditionals, and function expressions may disappear;
    calls, assignments, exported bindings, and recursive prelude bindings stay
    out of scope until a narrower fixture proves broader DCE is worth it
  - when dead bindings were the only remaining owners of a runtime/helper
    import, the final `JIR` normalize step should recompute imports from the
    live body so emitted JS does not keep stale helper imports around
- ownership:
  - `src/js/jir/passes/dce.ml`
  - `src/js/jir/passes/dce.mli`
  - `src/js/jir/passes/normalize.ml`
  - `src/js/jir/passes/passes.ml`
  - `src/js/jir/passes/passes.mli`
  - `src/js/jir/lowering.ml`
  - `tests/fixtures/corpus/0117_dead_local_bindings.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0117_dead_local_bindings.*.expected`
  - `tests/fixtures/js/constants_and_direct_calls.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0117_dead_local_bindings` now proves that a dead captured local helper and
    its captured immutable literal binding disappear from the flattened
    declaration-initializer block before JS emission, leaving only the live
    assignment to `result`
  - the same slice keeps the runtime boundary honest: once the dead local
    helper is gone, the final `JIR` normalize step also drops the now-unused
    `callPrimitive` import instead of leaking it into emitted JS
  - the direct `constants_and_direct_calls` `Core_ir -> JIR` fixture now keeps
    the same rule honest at the non-source-driven surface by dropping the dead
    unexported `nothing = ()` binding from the final lowered `JIR`
- validation:
  - the default repo `_build/debug` lane was still blocked by an existing
    `riot.lock`, so validation ran from a temporary workspace manifest rooted
    in `/tmp` with `[riot].target_dir = "/tmp/riot-js-loop-build"` and
    symlinked repo sources instead of mutating the locked in-repo build root
  - `riot test -p raml dead_local_bindings` initially failed only because the
    seven `0117_dead_local_bindings.*.expected` snapshots were missing
  - the first full-package pass also produced stale `.expected.new` files from
    an earlier buggy DCE draft that dropped live identifiers; those candidates
    were rejected, the identifier-use bug was fixed, and the full package went
    green again before this slice was recorded as landed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run` passed after materializing the emitted module next to
    `src/js/Printf.js`, printing `42`
- next:
  - keep broader dead-binding elimination separate until a narrower fixture
    proves calls, assignments, exported bindings, or recursive prelude
    declarations can be removed safely without a wider effect model
  - keep tree shaking separate; this slice only settles local immutable dead
    bindings plus live-import recomputation inside the current `JIR` pass stack

### 2026-04-11: Prelude `list` Reuses The Tagged-Tuple Contract

Picked immediate next target 1 again, but kept the slice narrower than
general list support by proving only prelude `[]` / `::` constructor lowering,
one exhaustive recursive `match`, and list-literal construction through the
existing shared variant path.

- fixture:
  - `tests/fixtures/corpus/0012_list_recursion_sum.ml`
  - `tests/fixtures/js/0012_list_recursion_sum.core_ir.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.jir.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.js.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.pipeline.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.lowering.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.codegen.expected`
  - `tests/fixtures/js/0012_list_recursion_sum.compilation.expected`
- invariant:
  - stdlib `[]` / `::` expressions, list literals, and one exhaustive
    constructor-only recursive `match` over `list` should lower through the
    same shared tagged-tuple contract already accepted for source-local
    ordinary variants and prelude `option` / `result`
  - this remains a shared `Typ -> Core_ir` choice, not a JS-only runtime
    decision: do not introduce a JS-specific cons-cell or array encoding just
    because `::` carries two source arguments
  - keep non-empty list literal patterns, guards, wildcard/default cases,
    `as` patterns, and `try/with` out of this slice until a narrower fixture
    proves the current shared list-constructor contract is insufficient
- ownership:
  - `src/typ_lowering.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0012_list_recursion_sum.*.expected`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the shared constructor-resolution path now accepts prelude `list`
    alongside prelude `option` and `result` through the existing
    `Typ.Config.default.ambient_type_decls` surface
  - the shared variant encoding now keeps slot `0` as the constructor tag and
    keeps slot `1` as one shared payload slot even when a constructor has more
    than one source argument, by packing those arguments into a tuple payload;
    `::` is the first proof point for that rule
  - `0012_list_recursion_sum` proves that recursive list matching and list
    literal construction reuse the existing `%eq`, `%tuple_make`, and
    `%tuple_get` JS/runtime path without needing a list-specific node, helper,
    or emitter branch
- validation:
  - `riot test -p raml list_recursion_sum` initially failed only because the
    seven `0012_list_recursion_sum.*.expected` snapshots were missing
  - after promoting those seven snapshots, `riot test -p raml list_recursion_sum`
    passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0012/list_recursion_sum.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `15`
- next:
  - keep `0011_result_pipeline` separate until a narrower fixture proves the
    current shared constructor contract is insufficient for `try/with`,
    `as` patterns, or richer result control flow
  - keep non-empty list literal patterns, wildcard/default cases, guards, and
    any JS-only list encoding separate until one new fixture proves the shared
    tagged-tuple contract should widen again

### 2026-04-11: Prelude `result` Reuses The Tagged-Tuple Contract

Picked immediate next target 1 again, but kept the slice narrower than
`0011_result_pipeline` by proving only stdlib `result` constructor lowering and
one exhaustive constructor-only `match`.

- fixture:
  - `tests/fixtures/corpus/0116_prelude_result_match.ml`
  - `tests/fixtures/js/0116_prelude_result_match.core_ir.expected`
  - `tests/fixtures/js/0116_prelude_result_match.jir.expected`
  - `tests/fixtures/js/0116_prelude_result_match.js.expected`
  - `tests/fixtures/js/0116_prelude_result_match.pipeline.expected`
  - `tests/fixtures/js/0116_prelude_result_match.lowering.expected`
  - `tests/fixtures/js/0116_prelude_result_match.codegen.expected`
  - `tests/fixtures/js/0116_prelude_result_match.compilation.expected`
- invariant:
  - stdlib `Ok payload` / `Error payload` expressions and exhaustive
    constructor-only matches over `result` should lower through the same shared
    tagged-tuple contract already accepted for source-local ordinary variants
    and prelude `option`
  - this remains a shared `Typ -> Core_ir` choice, not a JS-only runtime
    decision: do not introduce a JS-specific result object or exception-shaped
    encoding just because the constructors come from the prelude instead of a
    source type declaration
  - keep `try/with`, `as` patterns, guards, wildcard/default cases, and list
    constructors out of this slice; `0011_result_pipeline` stays separate
    because it widens the problem beyond prelude constructor resolution
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0116_prelude_result_match.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0116_prelude_result_match.*.expected`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the shared constructor-resolution path now accepts prelude `result`
    constructors through the existing `Typ.Config.default.ambient_type_decls`
    surface alongside prelude `option`
  - `0116_prelude_result_match` proves that `Ok` and `Error` reuse the same
    tag-and-payload tuple layout already used by `0009_variants_and_match` and
    `0106_prelude_option_match`
  - JS lowering and emission needed no new result-specific node, runtime
    helper, or emitter branch; the existing `%tuple_make`, `%tuple_get`, and
    `%eq` path was already sufficient once shared lowering resolved the
    constructors
- validation:
  - `riot test -p raml prelude_result_match` initially failed in the JS fixture
    bin with unsupported constructor expr/pattern errors because `Ok` and
    `Error` did not resolve to a visible ordinary variant declaration
  - after extending the prelude variant-layout lookup and promoting the seven
    `0116_prelude_result_match.*.expected` snapshots,
    `riot test -p raml prelude_result_match` passed
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0116/prelude_result_match.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
- next:
  - keep `0011_result_pipeline` separate until a narrower fixture proves the
    current shared result-constructor contract is insufficient for
    `try/with`, `as` patterns, or richer control flow
  - keep list constructors, guards, wildcard/default cases, and any JS-only
    result encoding separate until one new fixture proves the tagged-tuple
    contract should widen again

### 2026-04-11: Explicit `external print_endline` Slice Landed

Picked immediate next target 7 in the narrowest source-driven slice that
starts explicit `external` work without pretending user foreign bindings or
module-backed imports are solved already.

- fixture:
  - `tests/fixtures/corpus/0115_external_print_endline.ml`
  - `tests/fixtures/js/0115_external_print_endline.core_ir.expected`
  - `tests/fixtures/js/0115_external_print_endline.jir.expected`
  - `tests/fixtures/js/0115_external_print_endline.js.expected`
  - `tests/fixtures/js/0115_external_print_endline.pipeline.expected`
  - `tests/fixtures/js/0115_external_print_endline.lowering.expected`
  - `tests/fixtures/js/0115_external_print_endline.codegen.expected`
  - `tests/fixtures/js/0115_external_print_endline.compilation.expected`
- invariant:
  - a top-level source `external print_endline : string -> unit = "print_endline"`
    should remain compile-time-only at the shared `Typ -> Core_ir` boundary;
    it contributes name/type information for later references, but it must not
    materialize a runtime init item in `Core_ir`
  - once the later direct call still targets the already-owned runtime helper
    name `print_endline`, the JS backend should keep using its existing
    explicit sibling-runtime import path instead of inventing a new emitter
    rule or widening into general typed FFI metadata
  - keep user foreign bindings, primitive payload provenance, and module import
    materialization separate until a narrower fixture proves the current
    compile-time-only declared-value boundary is insufficient
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0115_external_print_endline.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0115_external_print_endline.{core_ir,jir,js,pipeline,lowering,codegen,compilation}.expected`
- effect:
  - `Typ -> Core_ir` now filters top-level `ItemTree.DeclaredValue` items out
    of runtime lowering alongside the already compile-time-only type/open
    items, so the explicit external declaration no longer blocks the later
    direct call from lowering
  - the intended JS shape for the first explicit external slice stays narrow:
    emitted JS still imports `print_endline` from `./riot-runtime.js` and
    calls `__print_endline("hello, external")`
  - the same source proof point now reaches `Raml.Example_pipeline` and
    `Raml.Compilation`, so the first explicit top-level `external` declaration
    is covered across the JS source-driven fixture bins instead of only the
    direct JS fixture lane
- validation:
  - the default repo `_build/debug` lane was still blocked by external `riot`
    processes holding `riot.lock`, so validation ran from a temporary workspace
    manifest rooted in `/tmp` with `[riot].target_dir = "/tmp/riot-js-loop-build"`
    and symlinked repo sources instead of mutating the locked in-repo build
    root
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml external_print_endline` initially failed only because
    the four example/compilation snapshots were missing and the three existing
    JS snapshots lacked a trailing newline
  - after promoting the seven `0115_external_print_endline.*.expected`
    snapshots, `riot test -p raml external_print_endline` passed
  - `bun run /tmp/raml-js-0115/external_print_endline.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `hello, external`
  - `riot test -p raml` passed for the full package as well
  - `git diff --check -- compiler/raml` passed
- next:
  - keep user foreign bindings, typed FFI metadata, and module import
    materialization separate until a narrower source fixture proves the current
    compile-time-only declared-value boundary is insufficient
  - do not widen this slice into general external provenance just because the
    first explicit runtime-helper name now has full source-driven JS coverage

### 2026-04-11: Native `0014` Approval Debt Was The Earlier Package Blocker

This was the earlier package-level blocker before the native `0014`
approvals were promoted and the full package went green again.

- fixture:
  - `tests/fixtures/corpus/0014_mutual_recursion_even_odd.ml`
  - `tests/fixtures/native/0014_mutual_recursion_even_odd.*.expected.new`
- invariant:
  - that earlier red state was native approval debt, not a JS backend
    regression
  - keep JS progress accounting honest: when the package is red on native
    approvals, say so explicitly instead of treating JS slices as blocked on
    runtime correctness
  - keep the JS loop focused on JS/backend/runtime ownership even when the
    shared corpus expands both backend suites
- ownership:
  - `tests/fixtures/native/0014_mutual_recursion_even_odd.*.expected`
  - `tests/fixtures/native/0014_mutual_recursion_even_odd.*.expected.new`
  - `JS_LOOP.md`
- effect:
  - the JS, example, compilation, `JIR`, and `JST` lanes are green for the
    current corpus, including `0114_top_level_mutual_recursion`
  - at that point, the only package blocker was the eleven pending native approvals for
    `0014_mutual_recursion_even_odd`
- validation:
  - `riot build raml` passed
  - `riot test -p raml` at that point failed only because the eleven
    `0014_mutual_recursion_even_odd.*.expected.new` native snapshots are
    pending approval
  - `git diff --check -- compiler/raml` passed

### 2026-04-11: Post-Alpha Identifier Alias Cleanup Landed

Picked pass-ladder item 3 in the narrowest JS-owned slice that shrinks
existing tuple/match temp noise without widening into general DCE or a shared
lowering rewrite.

- fixture:
  - `tests/fixtures/jir_lowering/tail_sequences_in_let_bodies.json`
  - `tests/fixtures/jir_lowering/tail_conditionals_in_function_bodies.json`
  - `tests/fixtures/js/tail_sequences_in_let_bodies.expected`
  - `tests/fixtures/js/tail_conditionals_in_function_bodies.expected`
  - `tests/fixtures/js/{0007_tuples_and_patterns,0008_records_and_updates,0009_variants_and_match,0010_option_pipeline,0106_prelude_option_match}.*.expected`
- invariant:
  - once alpha stabilization has made visible JS names explicit, immutable
    `const alias = identifier;` temps that only preserve an already-stable
    name should disappear from `JIR` when the aliased target is never assigned
    and the alias is not exported
  - this is a JS-only cleanup choice, not a `Core_ir` change and not an
    emitter trick: keep the rewrite in `src/js/` and keep wider dead-binding
    elimination separate until a narrower fixture proves it is needed
  - do not widen this slice to aliasing arbitrary expressions, exported
    bindings, or names that may observe later assignment
- ownership:
  - `src/js/jir/passes/remove_aliases.ml`
  - `src/js/jir/passes/remove_aliases.mli`
  - `src/js/jir/passes/passes.ml`
  - `src/js/jir/passes/passes.mli`
  - `src/js/jir/lowering.ml`
  - `tests/fixtures/js/tail_sequences_in_let_bodies.expected`
  - `tests/fixtures/js/tail_conditionals_in_function_bodies.expected`
  - `tests/fixtures/js/{0007_tuples_and_patterns,0008_records_and_updates,0009_variants_and_match,0010_option_pipeline,0106_prelude_option_match}.*.expected`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - tuple-destructure and identifier-scrutinee temps such as
    `const __raml_tuple_4 = value;` and `const __raml_match_8 = value;` now
    disappear from `JIR` and emitted JS once the existing alpha pass has
    stabilized names
  - the same rule also removes dead identifier-only helpers that were left
    behind by earlier record/match lowering, such as the unused
    `__raml_record_11 = p` wrapper in `0008_records_and_updates`
  - exported aliases such as `const result = __raml_init_result;` stay out of
    scope for this slice, which keeps readable exported names intact and keeps
    wider dead-binding/export rewriting separate
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the 32 affected JS
    snapshots above still reflected the pre-cleanup alias temps
  - after promoting those JS snapshots, `riot test -p raml` passed
  - `bun run /tmp/raml-js-alias-*/tuples_and_patterns.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `raml 5 5 raml`
  - `git diff --check -- compiler/raml` passed
- next:
  - keep broader dead-binding elimination separate until a narrower fixture
    proves that removing non-alias temps is worth the extra scope analysis
  - keep `external` work separate; this slice only settles post-alpha cleanup
    for immutable identifier aliases already introduced by the current shared
    lowering

### 2026-04-11: Top-Level Mutual Recursion JS Coverage Landed

Picked the next recursion proof point after local `let rec`, but kept the
slice source-driven and JS-owned so native fixture migration stayed separate.

- fixture:
  - `tests/fixtures/corpus/0114_top_level_mutual_recursion.ml`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.core_ir.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.jir.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.js.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.pipeline.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.lowering.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.codegen.expected`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.compilation.expected`
- invariant:
  - a source-level top-level `let rec ... and ...` group should stay explicit
    in shared `Core_ir` as one recursive `Binding_group` instead of being
    split into ad hoc per-binding JS rewrites
  - once that shared recursive group exists, the JS backend should keep using
    its current explicit recursive-top-level strategy:
    `let even; let odd; even = function ...; odd = function ...;`
    rather than inventing a new grouping pass or smarter emitter logic first
  - keep native fixture migration separate; this slice only proves the current
    JS recursive-group lowering against one real mutually recursive source
    program
- ownership:
  - `tests/fixtures/corpus/0114_top_level_mutual_recursion.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0114_top_level_mutual_recursion.*.expected`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suites now cover the first top-level mutually
    recursive function group instead of only self-recursive locals and
    nonrecursive top-level functions
  - the approved snapshots show the intended shared/backend split directly:
    `Core_ir` keeps `even` and `odd` in one recursive init group, while JS
    lowering materializes the existing `let`-prelude plus ordered assignment
    strategy before the later `result = even 10` binding and `Printf.printf`
    eval item
  - no compiler or runtime code changes were needed; the current shared
    lowering, JS recursive-group lowering, and runtime boundary were already
    sufficient once the source-driven coverage existed
- validation:
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the seven
    `0114_top_level_mutual_recursion.*.expected` snapshots were missing
  - after promoting those seven JS snapshots, `riot test -p raml` passed
  - `bun run /tmp/raml-js-0114/top_level_mutual_recursion.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true`
- next:
  - keep broader top-level grouping work separate until a narrower fixture
    proves the current recursive-group lowering is insufficient for execution
    order, declaration shaping, or later import/dependency work
  - keep tailcall strategy and broader recursion optimization separate until a
    narrower fixture proves the current runnable lowering is insufficient

### 2026-04-11: First JST Import Materialization Slice Landed

Picked pass-ladder item 2 in the narrowest printer-facing slice that changes
emitted JS structure without widening `JIR` or inventing `external` work.

- fixture:
  - `tests/fixtures/js/0008_records_and_updates.{js,pipeline,codegen,compilation}.expected`
  - `tests/fixtures/js/0010_option_pipeline.{js,pipeline,codegen,compilation}.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.{js,pipeline,codegen,compilation}.expected`
  - `tests/fixtures/js/0023_partial_application.{js,pipeline,codegen,compilation}.expected`
  - `tests/fixtures/js/0106_prelude_option_match.{js,pipeline,codegen,compilation}.expected`
- invariant:
  - when several collected `JIR` import requirements target the same module and
    all use named/default bindings, `JST` lowering should materialize them as
    one ESM import declaration instead of printing repeated sibling imports such
    as three separate `from "./riot-runtime.js"` lines
  - namespace imports such as `import * as Printf from "./Printf.js"` stay
    separate in this first slice; keep namespace/default combinations and later
    dependency discovery refinements out of scope until a narrower fixture
    proves they matter
  - this is a JS-owned import-materialization choice in `src/js/`, not a
    `Core_ir` change and not an emitter trick
- ownership:
  - `src/js/jst/lowering.ml`
  - `tests/fixtures/js/0008_records_and_updates.*.expected`
  - `tests/fixtures/js/0010_option_pipeline.*.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.*.expected`
  - `tests/fixtures/js/0023_partial_application.*.expected`
  - `tests/fixtures/js/0106_prelude_option_match.*.expected`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `JIR` still owns collected import requirements, but `JST` lowering now
    groups compatible named/default imports by module before emission
  - emitted JS for the current runtime-heavy slices now prints one grouped
    runtime import such as
    `import { callPrimitive as __callPrimitive, makeCurried as __makeCurried } from "./riot-runtime.js";`
    instead of repeated runtime import statements
  - source-visible namespace imports such as `Printf` stay on their own import
    line, which keeps the first stdlib-module boundary explicit while making
    the emitted runtime surface less noisy
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0010-imports/option_pipeline.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `14`
- next:
  - keep later dependency discovery, import provenance, and namespace/default
    coalescing separate until a narrower fixture proves the current grouped
    named-import rule is insufficient
  - keep `external` lowering separate; this slice only settles late ESM import
    materialization for already-owned runtime and stdlib surfaces

### 2026-04-11: Local Recursive Factorial JS Coverage Landed

Picked a narrower source-driven recursion slice than top-level mutual recursion
or tailcall strategy by reusing the existing factorial corpus example and only
proving that a supported local `let rec` inside a function body survives
through shared lowering and the current JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0013_tail_recursive_factorial.ml`
  - `tests/fixtures/js/0013_tail_recursive_factorial.core_ir.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.jir.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.js.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.pipeline.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.lowering.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.codegen.expected`
  - `tests/fixtures/js/0013_tail_recursive_factorial.compilation.expected`
- invariant:
  - a supported source `let rec` inside a function body should remain explicit
    in shared `Core_ir` as `Expr.Let { rec_flag = Recursive; ... }` instead of
    being erased into ad hoc JS recursion during lowering
  - once that shared recursive local group exists, the JS backend should keep
    using its existing explicit recursive-local strategy:
    `let loop; loop = __makeCurried(...); return loop(...)`
    rather than teaching the emitter new recursion tricks
  - this slice proves local recursive function lowering only; do not widen it
    into mutual recursion, loops, or a JS tailcall strategy without a narrower
    fixture
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0013_tail_recursive_factorial.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS/example/compilation fixture bins now cover the first source-driven
    local recursive function example instead of only nonrecursive local lets
    plus top-level recursive groups sketched in lowering
  - the approved snapshots show the intended shared/backend split directly:
    `Core_ir` keeps the recursive local group explicit, while JS lowering uses
    the already-owned runtime boundary with `makeCurried`, `%le`, `%mulint`,
    and `%subint`
  - no compiler or runtime code changes were needed; the current shared
    lowering plus JS local-recursion strategy were already sufficient once the
    source-driven coverage existed
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0013/tail_recursive_factorial.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `3628800`
  - `riot test -p raml` now passes for the full package as well
- next:
  - keep top-level mutual recursion as its own source-driven slice if the next
    proof point needs recursive binding groups visible at module scope
  - keep tailcall strategy separate until one narrower fixture proves the
    current runnable recursion coverage is insufficient

### 2026-04-11: Declaration-Initializer Zero-Arg IIFE Flattening Landed

Picked immediate next target 5 again, but kept the slice narrower than a
general scope rewrite by only taking declaration-initializer zero-arg IIFEs
whose body is already statement-shaped and value-producing.

- fixture:
  - `tests/fixtures/corpus/0113_initializer_shadowing.ml`
  - `tests/fixtures/js/0113_initializer_shadowing.core_ir.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.jir.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.js.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.pipeline.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.lowering.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.codegen.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.compilation.expected`
- invariant:
  - when a declaration initializer is a zero-arg IIFE whose body is already
    statement-shaped and ends in a value, the JS backend should flatten it
    before `JST` emission by routing the returned value through an explicit
    temp binding instead of printing another wrapper call
  - the rewrite must preserve lexical scope for initializer locals; widening a
    local shadow into module scope is still wrong if the final exported binding
    loses its source name or later code can see an initializer-only binding
  - this is a JS-only flattening choice, not a `Core_ir` change and not an
    emitter trick: keep the rewrite in `src/js/` and keep the printer dumb
- ownership:
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/jir/passes/flatten.ml`
  - `src/js/jir/passes/alpha.ml`
  - `src/js/jir/passes/normalize.ml`
  - `src/js/jst/types.ml`
  - `src/js/jst/types.mli`
  - `src/js/jst/lowering.ml`
  - `src/js/jst/emitter.ml`
  - `tests/fixtures/corpus/0113_initializer_shadowing.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0006_let_shadowing.*.expected`
  - `tests/fixtures/js/0010_option_pipeline.*.expected`
  - `tests/fixtures/js/0113_initializer_shadowing.*.expected`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - declaration-initializer IIFEs such as `const y = ((function() { ... })())`
    in `0006_let_shadowing` and `const result = ((function() { ... })())` in
    `0010_option_pipeline` now flatten to:
    `let __raml_init_name; { ...; (__raml_init_name = value); } const name = __raml_init_name;`
    instead of a printer-visible wrapper call
  - `JIR` and `JST` now own a lexical `Block` statement so initializer-local
    declarations remain scoped inside the flattened body instead of leaking
    into module scope
  - `0113_initializer_shadowing` now snapshots the dedicated proof point for
    the safety property that motivated the block form: a local `let result =`
    inside the initializer does not force the outer exported `result` binding
    to rename after flattening
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the seven
    `0113_initializer_shadowing.*.expected` snapshots were missing, which
    confirmed the new dedicated source fixture already reached every owned JS
    snapshot surface without causing unrelated JS or native drift
  - after promoting the seven `0113_initializer_shadowing.*.expected`
    snapshots, `riot test -p raml` passed
  - `bun run /tmp/raml-js-0113/initializer_shadowing.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `10 11`
  - `bun run /tmp/raml-js-0010/option_pipeline.mjs` passed after the same
    materialization, printing `14` and proving the branchy initializer rewrite
    stays runnable in the earlier option-flow slice too
  - `git diff --check -- compiler/raml` passed
- next:
  - keep broader block-introducing scope rewrites and later printer-facing
    flattening separate until a narrower fixture proves the current `JIR`-level
    slices are insufficient

### 2026-04-11: Effect-Position Zero-Arg IIFE Flattening Landed

Picked immediate next target 5 in the smallest JS-only slice that turns the
existing statement-shaped top-level IIFEs into plain statements without making
the emitter smarter.

- fixture:
  - `tests/fixtures/corpus/0112_effect_position_local_let.ml`
  - `tests/fixtures/js/0112_effect_position_local_let.core_ir.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.jir.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.js.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.pipeline.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.lowering.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.codegen.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.compilation.expected`
- invariant:
  - when an effect-position expression is a zero-arg IIFE whose body is already
    statement-shaped, the JS backend should flatten it before `JST` emission by
    rewriting tail returns into effect statements instead of printing another
    wrapper call
  - this is a JS-only flattening choice, not a `Core_ir` change and not an
    emitter trick: keep the rewrite in `src/js/` and keep the printer dumb
  - do not widen this slice to declaration-initializer IIFEs or to cases whose
    body would need early-return semantics preserved across a wider scope
- ownership:
  - `src/js/jir/passes/flatten.ml`
  - `src/js/jir/passes/flatten.mli`
  - `src/js/jir/passes/passes.ml`
  - `src/js/jir/passes/passes.mli`
  - `src/js/jir/lowering.ml`
  - `tests/fixtures/corpus/0112_effect_position_local_let.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0007_tuples_and_patterns.*.expected`
  - `tests/fixtures/js/0008_records_and_updates.*.expected`
  - `tests/fixtures/js/0010_option_pipeline.*.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.*.expected`
  - `tests/fixtures/js/0106_prelude_option_match.*.expected`
  - `tests/fixtures/js/0112_effect_position_local_let.*.expected`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0112_effect_position_local_let` now snapshots the first dedicated
    source-driven proof that a top-level local-`let` side-effect slice can
    emit as:
    `const message = "flattened"; __print_endline(message);`
    instead of a top-level zero-arg IIFE
  - the new `Js.Jir.Passes.Flatten` pass runs before alpha stabilization, so
    widened statement scope still gets fresh-name handling from the existing
    alpha pass instead of relying on emitter tricks
  - existing JS examples that previously emitted statement-shaped top-level
    IIFEs now flatten in the same way:
    `0007_tuples_and_patterns`, `0008_records_and_updates`,
    `0010_option_pipeline`, `0022_local_functions_and_closures`, and
    `0106_prelude_option_match`
  - declaration-initializer IIFEs such as `const result = ((function() { ... })())`
    in `0010_option_pipeline` still stayed explicit in this earlier slice,
    which kept it narrower than the later dedicated initializer-flattening
    rewrite
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - the JS/example/compilation fixture bins passed after promoting the JS
    snapshots for the five affected existing examples plus the seven new `0112`
    snapshots
  - `bun run /tmp/raml-js-0112/effect_position_local_let.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `flattened`
  - `bun run /tmp/raml-js-0106/prelude_option_match.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
  - `git diff --check -- compiler/raml` passed
  - `riot test -p raml` passed
- next:
  - declaration-initializer IIFE flattening landed later as its own temp-
    binding plus lexical-block rewrite; keep any broader scope rewrites
    separate from this earlier effect-position-only slice
  - keep later printer-facing flattening or dead-binding cleanup separate until
    a narrower fixture proves the current `JIR`-level pass is insufficient

### 2026-04-11: Source-Level `>=` Direct Call Runtime Boundary Landed

Picked the last narrow ordered-comparison follow-up from the earlier `%le`
slice, but kept the work strictly in the JS/example/compilation lane plus the
owned JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0111_greater_or_equal_comparison.ml`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.core_ir.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.jir.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.js.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.pipeline.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.lowering.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.codegen.expected`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.compilation.expected`
- invariant:
  - a source-level direct `>=` call must lower through the same explicit JS
    runtime primitive boundary already used for arithmetic, `=`, `<`, `<=`,
    and `>`; emitted JS is still wrong if it prints `>=(5, 3)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: `Core_ir` should keep exposing the source
    operator as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep structural ordered comparison out of this slice unless a narrower
    fixture proves it is involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0111_greater_or_equal_comparison.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0111_greater_or_equal_comparison.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0111_greater_or_equal_comparison` now snapshots the first source-driven
    `>=` example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended backend split directly:
    `Core_ir` still carries direct callee `">="`, while the JS path lowers it
    to `__callPrimitive("%ge", 5, 3)`
  - JS direct-call lowering now routes `>=` through `%ge` in
    `./riot-runtime.js`, keeping the emitter unchanged and the runtime/helper
    choice owned by `src/js/`
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the seven
    `0111_greater_or_equal_comparison.*.expected` snapshots were missing
  - after promoting the seven `0111_greater_or_equal_comparison.*.expected`
    snapshots, `_build/debug/aarch64-apple-darwin/out/raml/js_fixture_tests run-tests greater_or_equal_comparison`
    passed
  - `_build/debug/aarch64-apple-darwin/out/raml/example_fixture_tests run-tests greater_or_equal_comparison`
    passed
  - `_build/debug/aarch64-apple-darwin/out/raml/compilation_fixture_tests run-tests greater_or_equal_comparison`
    passed
  - `bun run /tmp/raml-js-0111/greater_or_equal_comparison.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true`
  - `git diff --check -- compiler/raml` passed
  - `riot test -p raml` passed
- next:
  - keep structural ordered comparison separate until one narrower fixture
    proves the current `%ge` boundary is insufficient

### 2026-04-11: Source-Level `<=` Direct Call Runtime Boundary Progress

Picked the next narrower ordered-comparison follow-up from the earlier `%gt`
slice, but kept the work strictly in the JS/example/compilation lane plus the
owned JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0110_less_or_equal_comparison.ml`
  - `tests/fixtures/js/0110_less_or_equal_comparison.core_ir.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.jir.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.js.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.pipeline.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.lowering.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.codegen.expected`
  - `tests/fixtures/js/0110_less_or_equal_comparison.compilation.expected`
- invariant:
  - a source-level direct `<=` call must lower through the same explicit JS
    runtime primitive boundary already used for arithmetic, `=`, `<`, and `>`;
    emitted JS is still wrong if it prints `<=(3, 5)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: `Core_ir` should keep exposing the source
    operator as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep `>=` and structural ordered comparison out of this slice unless a
    narrower fixture proves they are involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0110_less_or_equal_comparison.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0110_less_or_equal_comparison.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0110_less_or_equal_comparison` now snapshots the first source-driven
    `<=` example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended backend split directly:
    `Core_ir` still carries direct callee `"<="`, while the JS path lowers it
    to `__callPrimitive("%le", 3, 5)`
  - JS direct-call lowering now routes `<=` through `%le` in
    `./riot-runtime.js`, keeping the emitter unchanged and the runtime/helper
    choice owned by `src/js/`
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the new `0110`
    snapshots were missing, which confirmed the example already reached every
    owned JS snapshot surface
  - after promoting the seven `0110_less_or_equal_comparison.*.expected`
    snapshots, `_build/debug/aarch64-apple-darwin/out/raml/js_fixture_tests run-tests less_or_equal_comparison`
    passed
  - `_build/debug/aarch64-apple-darwin/out/raml/example_fixture_tests run-tests less_or_equal_comparison`
    passed
  - `_build/debug/aarch64-apple-darwin/out/raml/compilation_fixture_tests run-tests less_or_equal_comparison`
    passed
  - `bun run /tmp/raml-js-0110/less_or_equal_comparison.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true`
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
- next:
  - keep `>=` and structural ordered comparison separate until one narrower
    fixture proves the current `%le` boundary is insufficient
  - do not widen this direct-call rule into structural ordered comparison or a
    broader JS comparison pass without a dedicated fixture proving the need

### 2026-04-11: Source-Level `>` Direct Call Runtime Boundary Progress

Picked the next narrower ordered-comparison follow-up from the earlier `%lt`
slice, but kept the work strictly in the JS/example/compilation lane plus the
owned JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0109_greater_than_comparison.ml`
  - `tests/fixtures/js/0109_greater_than_comparison.core_ir.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.jir.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.js.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.pipeline.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.lowering.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.codegen.expected`
  - `tests/fixtures/js/0109_greater_than_comparison.compilation.expected`
- invariant:
  - a source-level direct `>` call must lower through the same explicit JS
    runtime primitive boundary already used for arithmetic, `=`, and `<`;
    emitted JS is still wrong if it prints `>(5, 3)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: `Core_ir` should keep exposing the source
    operator as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep `<=`, `>=`, and structural ordered comparison out of this slice
    unless a narrower fixture proves they are involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/fixtures/corpus/0109_greater_than_comparison.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0109_greater_than_comparison.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0109_greater_than_comparison` now snapshots the first source-driven `>`
    example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended backend split directly:
    `Core_ir` still carries direct callee `">"`, while the JS path lowers it
    to `__callPrimitive("%gt", 5, 3)`
  - JS direct-call lowering now routes `>` through `%gt` in
    `./riot-runtime.js`, keeping the emitter unchanged and the runtime/helper
    choice owned by `src/js/`
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` initially failed only because the seven
    `0109_greater_than_comparison.*.expected` snapshots were missing, which
    confirmed the new example already reached every owned JS snapshot surface
  - after promoting the seven `0109_greater_than_comparison.*.expected`
    snapshots, the JS/example/compilation fixture bins passed for the new
    example inside `riot test -p raml`
  - `bun run /tmp/raml-js-0109/greater_than_comparison.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true`
  - `git diff --check -- compiler/raml` passed
  - later package validation moved again as newer slices landed; treat the
    unrelated native blocker named during this slice as historical context,
    not as the current package state
- next:
  - keep this slice landed as the `%gt` proof point, and record later package
    blockers in newer progress notes instead of retrofitting them here
  - keep `<=`, `>=`, and structural ordered comparison separate until one
    narrower fixture proves the current `%gt` boundary is insufficient

### 2026-04-11: Source-Level `<` Direct Call Runtime Boundary Progress

Picked the first narrower comparison follow-up from the earlier `=` slice, but
kept the work strictly in the JS/example/compilation lane plus the owned JS
runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0108_less_than_comparison.ml`
  - `tests/fixtures/js/0108_less_than_comparison.core_ir.expected`
  - `tests/fixtures/js/0108_less_than_comparison.jir.expected`
  - `tests/fixtures/js/0108_less_than_comparison.js.expected`
  - `tests/fixtures/js/0108_less_than_comparison.pipeline.expected`
  - `tests/fixtures/js/0108_less_than_comparison.lowering.expected`
  - `tests/fixtures/js/0108_less_than_comparison.codegen.expected`
  - `tests/fixtures/js/0108_less_than_comparison.compilation.expected`
- invariant:
  - a source-level direct `<` call must lower through the same explicit JS
    runtime primitive boundary already used for arithmetic and `=`;
    emitted JS is still wrong if it prints `<(3, 5)`
  - this is a JS runtime-boundary choice, not a shared `Typ -> Core_ir`
    change and not an emitter trick: `Core_ir` should keep exposing the source
    operator as a direct callee while `src/js/` chooses how that becomes
    runnable JS
  - keep `>`, `<=`, `>=`, and structural ordered comparison out of this slice
    unless a narrower fixture proves they are involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0108_less_than_comparison.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0108_less_than_comparison` now snapshots the first source-driven `<`
    example through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and final JS emission
  - the approved snapshots show the intended backend split directly:
    `Core_ir` still carries direct callee `"<"`, while the JS path lowers it
    to `__callPrimitive("%lt", 3, 5)`
  - JS direct-call lowering now routes `<` through `%lt` in
    `./riot-runtime.js`, keeping the emitter unchanged and the runtime/helper
    choice owned by `src/js/`
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/js_fixture_tests run-tests less_than_comparison` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/example_fixture_tests run-tests less_than_comparison` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/compilation_fixture_tests run-tests less_than_comparison` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0108/less_than_comparison.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `true`
  - later package validation moved again as newer slices landed; treat the
    unrelated native blocker named during this slice as historical context,
    not as the current package state
- next:
  - keep this slice landed as the `%lt` proof point, and record later package
    blockers in newer progress notes instead of retrofitting them here
  - keep `>`, `<=`, `>=`, and structural ordered comparison separate until one
    narrower fixture proves the current `%lt` boundary is insufficient

### 2026-04-11: Option Pipeline Coverage Exposed And Fixed `=` JS Lowering

Picked immediate next target 1 again and kept the slice in the
JS/example/compilation lane plus one owned JS runtime-boundary fix.

- fixture:
  - `tests/fixtures/corpus/0010_option_pipeline.ml`
  - `tests/fixtures/js/0010_option_pipeline.core_ir.expected`
  - `tests/fixtures/js/0010_option_pipeline.jir.expected`
  - `tests/fixtures/js/0010_option_pipeline.js.expected`
  - `tests/fixtures/js/0010_option_pipeline.pipeline.expected`
  - `tests/fixtures/js/0010_option_pipeline.lowering.expected`
  - `tests/fixtures/js/0010_option_pipeline.codegen.expected`
  - `tests/fixtures/js/0010_option_pipeline.compilation.expected`
- invariant:
  - nested stdlib `option` flow such as `safe_div` plus a later `match` chain
    should keep reusing the existing shared tagged-tuple contract instead of
    earning a JS-only option representation
  - a source-level direct `=` call inside that slice must lower through the
    same explicit JS runtime primitive boundary already used for `+`, `-`,
    `*`, `/`, and `mod`; emitted JS is still wrong if it prints `=(b, 0)`
  - keep richer comparisons, typed externals, and general import provenance
    out of this slice unless a narrower fixture proves they are involved
- ownership:
  - `src/js/jir/lowering.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0010_option_pipeline.*.expected`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0010_option_pipeline` now snapshots the first source-driven option-flow
    example that mixes nested exhaustive constructor-only matches with a
    source-level equality test and later `Printf.printf` / `print_endline`
    output
  - adding the fixture exposed a real JS bug rather than a pure coverage gap:
    `safe_div` initially emitted `if (=(b, 0))`, which is invalid runnable JS
  - JS direct-call lowering now routes `=` through
    `__callPrimitive("%eq", ...)`, keeping the emitter unchanged and the
    runtime/helper choice owned by `src/js/`
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot test -p raml` initially failed only because the new `0010` snapshots
    were missing, which confirmed the example already reached every JS
    snapshot surface
  - the same new fixture then exposed the real runtime-boundary bug in the
    generated JS output: bare `=(b, 0)` instead of `__callPrimitive("%eq", b, 0)`
  - after routing `=` through the JS runtime primitive boundary and promoting
    the seven `0010_option_pipeline.*.expected` snapshots, `riot build raml`
    passed and `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0010/option_pipeline.mjs` passed after materializing
    the emitted module next to `src/js/Printf.js` and `src/js/riot-runtime.js`,
    printing `14`
- next:
  - if a later source slice needs `<`, `>`, `<=`, `>=`, or structural
    equality, prove each one separately instead of widening the current `=`
    direct-call rule by habit
  - keep native coverage for `0010` separate until a deliberate shared
    migration decides that example belongs in the native corpus lane too

### 2026-04-11: Explicit Top-Level `open Std` Resolves Away Before `Core_ir`

Picked the remaining Example 01 shared-lowering gap, but kept it in the
JS/example/compilation lane plus the shared `Typ -> Core_ir` boundary.

- fixture:
  - `tests/fixtures/corpus/0107_open_std_hello_world.ml`
  - `tests/fixtures/js/0107_open_std_hello_world.core_ir.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.jir.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.js.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.pipeline.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.lowering.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.codegen.expected`
  - `tests/fixtures/js/0107_open_std_hello_world.compilation.expected`
- invariant:
  - an explicit top-level `open Std` should remain a compile-time-only scope
    item once `typ` has already resolved later identifiers through that open
  - this is a shared `Typ -> Core_ir` choice, not a JS import/materialization
    choice: do not invent a `Core_ir` node or JS backend hook for a scope-only
    open that carries no runtime effect
  - keep local opens, includes, and module aliases outside this slice unless a
    narrower fixture proves they need a different ownership boundary
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0107_open_std_hello_world.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - top-level `ItemTree.Open` items are now treated as compile-time-only at
    the shared lowering boundary instead of failing `Typ -> Core_ir`
  - `0107_open_std_hello_world` proves that an explicit prelude open lowers to
    the same `Core_ir`, `JIR`, and emitted JS shape as the existing
    hello-world side-effect slice
  - JS lowering and emission needed no new import rule, runtime helper, or
    emitter branch once the shared lowering stopped materializing the open
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/js_fixture_tests run-tests open_std_hello_world` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/example_fixture_tests run-tests open_std_hello_world` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/compilation_fixture_tests run-tests open_std_hello_world` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0107/open_std_hello_world.mjs` passed after
    materializing the emitted module next to `src/js/riot-runtime.js`,
    printing `hello, world`
- next:
  - keep local-open lowering separate; this slice only settles the top-level
    compile-time-only `open` case
  - if a later slice needs include or module-alias lowering, prove that with a
    separate fixture instead of widening this scope-only rule implicitly

### 2026-04-11: Prelude `option` Reuses The Tagged-Tuple Contract

Picked immediate next target 1 again, but kept the slice narrower than a full
stdlib sum-type expansion.

- fixture:
  - `tests/fixtures/corpus/0106_prelude_option_match.ml`
  - `tests/fixtures/js/0106_prelude_option_match.core_ir.expected`
  - `tests/fixtures/js/0106_prelude_option_match.jir.expected`
  - `tests/fixtures/js/0106_prelude_option_match.js.expected`
  - `tests/fixtures/js/0106_prelude_option_match.pipeline.expected`
  - `tests/fixtures/js/0106_prelude_option_match.lowering.expected`
  - `tests/fixtures/js/0106_prelude_option_match.codegen.expected`
  - `tests/fixtures/js/0106_prelude_option_match.compilation.expected`
- invariant:
  - stdlib `None` / `Some payload` expressions and exhaustive constructor-only
    matches over `option` should lower through the same shared tagged-tuple
    contract already accepted for source-local ordinary variants
  - this is still a shared `Typ -> Core_ir` choice, not a JS-only runtime
    decision: do not introduce a JS-specific `option` encoding or helper
    surface just because the constructors come from the prelude instead of a
    source type declaration
  - keep `result`, list constructors, guards, and wildcard/default cases out of
    this slice unless one narrower fixture proves they are involved
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0106_prelude_option_match.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the shared constructor-resolution path now accepts prelude `option`
    constructors through the public `Typ.Config.default.ambient_type_decls`
    surface instead of only through top-level source type declarations
  - `0106_prelude_option_match` proves that `None` and `Some` reuse the same
    tag-and-payload tuple layout already used by `0009_variants_and_match`
  - JS lowering and emission needed no new option-specific node, runtime
    helper, or emitter branch; the existing `%tuple_make`, `%tuple_get`, and
    `%eq` path was already sufficient once shared lowering resolved the
    constructors
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/js_fixture_tests run-tests prelude_option_match` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/example_fixture_tests run-tests prelude_option_match` passed
  - `_build/debug/aarch64-apple-darwin/out/raml/compilation_fixture_tests run-tests prelude_option_match` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0106/prelude_option_match.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
- next:
  - if the next sum-type slice needs `result`, list constructors, or a
    different prelude representation, prove that with a separate fixture
    instead of widening this option-only contract implicitly
  - keep JS-only option/null/undefined interop encoding out of `Core_ir` until
    typed JS interop work actually demands it

### 2026-04-11: Partial Application Runtime Boundary Landed

Picked the narrow higher-order follow-up that the earlier indirect-call slice
pointed at, but kept it strictly in the JS/example/compilation lane plus the
owned JS runtime boundary.

- fixture:
  - `tests/fixtures/corpus/0023_partial_application.ml`
  - `tests/fixtures/js/0023_partial_application.core_ir.expected`
  - `tests/fixtures/js/0023_partial_application.jir.expected`
  - `tests/fixtures/js/0023_partial_application.js.expected`
  - `tests/fixtures/js/0023_partial_application.pipeline.expected`
  - `tests/fixtures/js/0023_partial_application.lowering.expected`
  - `tests/fixtures/js/0023_partial_application.codegen.expected`
  - `tests/fixtures/js/0023_partial_application.compilation.expected`
  - `tests/fixtures/js/full_core_ir.expected`
  - `tests/fixtures/js/tail_conditionals_in_function_bodies.expected`
  - `tests/fixtures/js/effect_conditionals_in_function_bodies.expected`
  - `tests/fixtures/js/0008_records_and_updates.*.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.*.expected`
  - `tests/fixtures/js/0103_local_function_capture.*.expected`
- invariant:
  - a source-defined multi-parameter function such as `let mul x y z = ...`
    must stay runnable when later source call sites are under-applied as in
    `mul 2` and `double_then 3`; a green snapshot is not enough if emitted JS
    still treats that as one raw JS call on a plain 3-argument function
  - this is a JS runtime-boundary choice, not an emitter trick: the JS backend
    should make currying explicit before `JST` emission instead of teaching the
    printer about partial application
  - existing full-application examples with multi-parameter compiled lambdas
    should keep working after the same boundary is applied consistently
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/jir/types.ml`
  - `src/js/jir/types.mli`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/runtime.mli`
  - `src/js/riot-runtime.js`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - multi-parameter compiled lambdas now lower through
    `__makeCurried(function(...), arity)` from `./riot-runtime.js` instead of
    emitting a bare JS function value and hoping raw JS arity matches source
    currying semantics
  - `0023_partial_application` now proves the first source-driven under-
    application slice through `Core_ir`, `JIR`, `Raml.Example_pipeline`,
    `Raml.Compilation`, and runnable emitted JS
  - the same runtime-boundary choice is now visible in existing JS snapshots
    that export or bind multi-parameter compiled lambdas, without touching the
    native fixture lane
- validation:
  - `riot build raml` passed
  - `riot test -p raml` initially exposed the real bug: `0023` compiled to
    snapshots but emitted `const double_then = mul(2)`, which is not runnable
    JS when `mul` is a plain 3-parameter function
  - after the JS/runtime fix, the `0023_partial_application` cases passed in
    the JS/example/compilation bins
  - `riot test -p raml` now passes for the full package as well
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0023/partial_application.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
  - `bun run /tmp/raml-js-0008/records_and_updates.mjs` passed after the same
    materialization, printing `(1,2) -> (4,6)` and proving full application of
    a wrapped 3-parameter function still behaves correctly
- next:
  - if a later slice needs source-accurate partial application for imported
    externals or stdlib functions, decide that explicitly instead of assuming
    the current compiled-function `makeCurried` boundary generalizes for free
  - keep tree shaking, import materialization, and `external` work separate
    unless a new fixture proves the currying boundary is involved

### 2026-04-11: Source-Driven Indirect Call Coverage Landed

Picked the smallest follow-up to the earlier closure-escape slice and kept it
strictly in the JS/example/compilation lane.

- fixture:
  - `tests/fixtures/corpus/0105_indirect_call_via_returned_closure.ml`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.core_ir.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.jir.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.js.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.pipeline.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.lowering.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.codegen.expected`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.compilation.expected`
- invariant:
  - when a source-level call site uses a callee expression such as
    `(make_adder 7) 35`, shared lowering should materialize the outer call as
    `Core_ir.Apply` with `callee = Indirect <expr>` instead of collapsing it
    back to a named direct call
  - the JS backend should preserve that indirect-call shape through `JIR` as a
    nested call expression, not a printer-only special case
  - the emitted JS should stay runnable as standalone ESM when materialized
    next to the owned sibling `./Printf.js` and `./riot-runtime.js` surfaces
- ownership:
  - `tests/fixtures/corpus/0105_indirect_call_via_returned_closure.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0105_indirect_call_via_returned_closure.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suite now covers the first source example whose
    outermost callee is an expression, so indirect application is proven from
    source through `Core_ir`, `JIR`, `Raml.Example_pipeline`, and
    `Raml.Compilation`
  - the approved snapshots show the intended shape directly:
    `Printf.printf("%d\\n", make_adder(7)(35))`
  - no compiler or runtime code changes were needed; the existing lowering and
    runtime boundary were already sufficient for this narrower invariant
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0105/indirect_call_via_returned_closure.mjs` passed
    after materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
- next:
  - if the next JS slice needs broader higher-order-call coverage, use a new
    source fixture to decide whether partial application or closure-return
    chains need more than the current nested-call lowering
  - keep this slice separate from `external`, import-materialization, or
    native fixture migration unless one new invariant proves they are involved

### 2026-04-11: Local Functions And Closures JS Coverage Landed

Picked the next source-driven JS example using the existing corpus fixture for
closure escape, but kept the slice strictly in the JS/example/compilation lane.

- fixture:
  - `tests/fixtures/corpus/0022_local_functions_and_closures.ml`
  - `tests/fixtures/js/0022_local_functions_and_closures.core_ir.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.jir.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.js.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.pipeline.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.lowering.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.codegen.expected`
  - `tests/fixtures/js/0022_local_functions_and_closures.compilation.expected`
- invariant:
  - a nested local function that captures an outer parameter and is returned
    from its defining scope should remain explicit in shared `Core_ir` as a
    local `let`-bound lambda whose result escapes as a first-class value
  - the JS backend should preserve that lexical capture without adding a new
    runtime helper or emitter-side special case
  - the emitted JS should stay runnable as standalone ESM when materialized
    next to the owned sibling `./Printf.js` and `./riot-runtime.js` surfaces
- ownership:
  - `tests/js_fixture_tests.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/fixtures/js/0022_local_functions_and_closures.*.expected`
  - `JS_LOOP.md`
- effect:
  - the JS source-driven suite now covers the first returned closure example
    beyond body-local capture, using a nested `add` function that closes over
    `base` and escapes through `make_adder`
  - no compiler or runtime code changes were needed; the existing shared
    lowering plus JS runtime boundary were already sufficient for this slice
  - review of the approved snapshots showed that `0022` proves closure escape,
    but not source-driven `Core_ir.Indirect`; the call `add7 35` still lowers
    as a named direct callee, so a later indirect-call slice needs a different
    source example
- validation:
  - `riot fix ./compiler/raml` passed
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - the new `0022_local_functions_and_closures` cases pass in the JS/example/
    compilation bins after promoting the seven JS snapshots above
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0022/local_functions_and_closures.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `42`
  - `riot test -p raml` passed
- next:
  - use `0022_local_functions_and_closures` for closure-escape coverage and
    `0105_indirect_call_via_returned_closure` for actual source-driven
    indirect-call coverage; do not collapse those two regression roles again

### 2026-04-11: Variants And Match Slice Landed

Picked immediate next target 1 and landed the first source-driven
closed-ordinary-variant plus exhaustive-`match` slice.

- fixture:
  - `tests/fixtures/corpus/0009_variants_and_match.ml`
  - `tests/fixtures/js/0009_variants_and_match.*.expected`
  - `tests/fixtures/native/0009_variants_and_match.*.expected`
- invariant:
  - top-level ordinary variant type declarations should contribute compile-time
    constructor layout information only and must not materialize runtime init
    items in `Core_ir`
  - closed constructor expressions should lower through shared tagged tuples,
    with slot `0` as the constructor tag and slot `1` as the optional payload
  - exhaustive constructor-only matches over one visible ordinary variant
    declaration should lower through shared `%eq` tag checks and tuple payload
    projection instead of introducing JS-only variant nodes or emitter tricks
- ownership:
  - `src/typ_lowering.ml`
  - `src/native/nir/lowering.ml`
  - `src/native/nir/runtime.ml`
  - `src/native/nir/runtime.mli`
  - `tests/fixtures/corpus/0009_variants_and_match.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `tests/native_fixture_tests.ml`
  - `docs/architecture.md`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
- effect:
  - `0009` now snapshots the first accepted shared sum-type representation
    through `Core_ir`, `JIR`, JS emission, `Raml.Example_pipeline`,
    `Raml.Compilation`, and the native `NIR`/`MIR`/`LIR` pipeline
  - JS lowering and emission needed no new variant-specific node or emitter
    logic because the existing tuple/runtime path already carried the needed
    structure
  - native `NIR` now lowers the shared `%eq` primitive through an explicit
    runtime helper so the example stays green across the existing backend lane
- validation:
  - `riot fix ./compiler/raml` still reports the existing unrelated warnings
    in `src/js/jir/types.ml*`, `src/native/emitter/*`, and
    `src/native/linker/*`
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed after promoting the seven
    `0009_variants_and_match.*.expected` JS snapshots
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0009/variants_and_match.mjs` passed after
    materializing the emitted module next to `src/js/Printf.js` and
    `src/js/riot-runtime.js`, printing `18`
- next:
  - keep wildcard/default cases, guards, open matches, and inline-record
    constructors out of the next slice unless one fixture proves they are
    needed
  - keep `result`, list constructors, wildcard/default cases, and guards
    separate until one narrower fixture proves the current tagged-tuple
    contract should widen again

### 2026-04-11: Immutable Record And Functional Update Slice Is Runnable

Picked the next source-driven data-representation slice after tuples, but kept
it deliberately narrower than variants or record patterns.

- fixture:
  - `tests/fixtures/corpus/0008_records_and_updates.ml`
  - `tests/fixtures/js/0008_records_and_updates.*.expected`
- invariant:
  - top-level immutable record type declarations should contribute compile-time
    layout information only and must not materialize runtime init items in
    `Core_ir`
  - immutable record construction, field access, and functional update should
    lower through the existing backend-neutral tuple construction/projection
    forms when the record labels resolve to one visible declaration
  - emitted JS should stay runnable by reusing the existing
    `callPrimitive("%tuple_make", ...)` / `callPrimitive("%tuple_get", ...)`
    path instead of inventing a JS-only record helper surface early
- ownership:
  - `src/typ_lowering.ml`
  - `src/core_ir.ml`
  - `src/core_ir.mli`
  - `src/js/jir/lowering.ml`
  - `docs/architecture.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the shared `Typ -> Core_ir` boundary now treats top-level type declarations
    as compile-time-only items for this record slice
  - the first immutable record operations lower to `Core_ir.Expr.Tuple` /
    `Core_ir.Expr.Tuple_get` rather than a backend-specific record node
  - `0008_records_and_updates` now emits runnable JS that keeps using the
    existing sibling `./Printf.js` and `./riot-runtime.js` module surfaces,
    with no new JS runtime helper added for records
- validation:
  - `riot fix ./compiler/raml` still reports the existing unrelated warnings
    in `src/native/emitter/*`, `src/native/linker/*`, and `src/js/jir/types.ml*`
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed, including `0008_records_and_updates` in the
    JS/example/compilation and native bins
  - `git diff --check -- compiler/raml` passed
  - `bun run` passed for a materialized `0008_records_and_updates` module
    after copying `src/js/Printf.js` and `src/js/riot-runtime.js` alongside it
- next:
  - start the first variants-and-`match` slice as a separate example-driven
    task instead of widening this record slice retroactively
  - only introduce a dedicated JS record representation or helper if a future
    fixture proves the shared tuple path is no longer enough

### 2026-04-11: Explicit `Printf.js` Module Surface For Runnable Tuple Examples

Picked immediate next targets 2 and 3 in the smallest slice that resolves the
existing `Printf` import hole without changing shared lowering or teaching the
emitter new tricks.

- fixture:
  - `tests/fixtures/corpus/0006_let_shadowing.ml`
  - `tests/fixtures/corpus/0007_tuples_and_patterns.ml`
- invariant:
  - when JS lowering materializes a source-visible dotted module reference such
    as `Printf.printf` as `import * as Printf from "./Printf.js"`, that
    sibling module must exist as an owned JS backend surface
  - low-level backend-selected helpers and source-visible stdlib namespace
    imports should stay separate: `./riot-runtime.js` owns primitives such as
    `print_endline` and `callPrimitive`, while `./Printf.js` owns formatted
    output
- ownership:
  - `src/js/Printf.js`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - the current JS backend now owns the first sibling stdlib module surface in
    `src/js/Printf.js` with a minimal `printf` / `sprintf` implementation for
    the currently covered `%s`, `%d`, `%b`, `%f`, `%S`, `%c`, and `%%`-style
    formatting slice, plus the currently emitted escaped newline/tab string
    forms
  - `0006_let_shadowing` and `0007_tuples_and_patterns` no longer depend on a
    temp handwritten `Printf.js` to run under `bun`
  - the compiler/lowering contract stays the same: `Printf.printf` continues
    to lower through a namespace import from `./Printf.js`
- validation:
  - `riot fix ./compiler/raml` still reports the existing unrelated warnings
    in `src/js/jir/types.ml*`, `src/native/emitter/*`, and
    `src/native/linker/*`
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed, including the later
    `0008_records_and_updates` approvals in both the JS and native suites
  - `git diff --check -- compiler/raml` passed
  - `bun run` passed for materialized `0006_let_shadowing` and
    `0007_tuples_and_patterns` modules after copying `src/js/Printf.js` and
    `src/js/riot-runtime.js` alongside the emitted files
- next:
  - keep this slice in the landed set as the first explicit source-visible
    stdlib-module surface for the JS backend
  - if more stdlib module references land before typed `external`, keep them
    as explicit sibling JS modules instead of folding them into
    `callPrimitive`

### 2026-04-11: Let Shadowing Example Emits Runnable JS

Picked immediate next target 1 and landed the smallest JS-only slice that
fixes `0006_let_shadowing` without touching shared `Core_ir` or the native
fixture lane.

- fixture:
  - `tests/fixtures/corpus/0006_let_shadowing.ml`
  - `tests/fixtures/js/0006_let_shadowing.jir.expected`
  - `tests/fixtures/js/0006_let_shadowing.js.expected`
  - `tests/fixtures/js/0006_let_shadowing.pipeline.expected`
  - `tests/fixtures/js/0006_let_shadowing.lowering.expected`
  - `tests/fixtures/js/0006_let_shadowing.codegen.expected`
  - `tests/fixtures/js/0006_let_shadowing.compilation.expected`
- invariant:
  - nonrecursive local binders that shadow visible names must lower to stable
    fresh JS identifiers so emitted code preserves OCaml `let` semantics
    instead of triggering JS TDZ or duplicate-`const` failures
  - the built-in integer operators used by this example must lower through an
    explicit JS runtime path, not ambient `+`, `-`, or `*` identifiers
- ownership:
  - `src/js/jir/lowering.ml`
  - `src/js/jir/passes/alpha.ml`
  - `src/js/jir/passes/passes.ml`
  - `src/js/jir/passes/passes.mli`
  - `src/js/riot-runtime.js`
  - `docs/js/ir.md`
  - `docs/js/runtime-and-ffi.md`
  - `AGENTS.md`
  - `TODO.md`
  - `JS_LOOP.md`
- effect:
  - `0006` now emits legal shadowing-preserving names such as `x$1` / `x$2`
    inside the local `let` body instead of repeated `const x = ...`
  - the first explicit `Js.Jir.Passes.Alpha` pass now stabilizes local binder
    names against visible imports and outer bindings before `JST` lowering
  - the example's integer operators now lower to
    `__callPrimitive("%addint" | "%mulint" | "%subint", ...)` through the
    existing sibling `./riot-runtime.js` surface
  - the emitter stayed unchanged
- validation:
  - `riot fix ./compiler/raml` still reports the existing unrelated warnings
    in `src/js/jir/types.ml*`, `src/native/emitter/*`, and
    `src/native/linker/*`
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed after promoting the six
    `0006_let_shadowing.*.expected` snapshots
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-0006/let_shadowing.mjs` passed after materializing
    the emitted module next to `src/js/riot-runtime.js` and a temp sibling
    `Printf.js`
- next:
  - decide whether the current arithmetic-through-`callPrimitive` path should
    stay as the first runtime boundary or graduate into richer `JIR` operator
    nodes before more arithmetic-heavy examples land
  - keep `0007_tuples_and_patterns` and the broader `Printf` import/runtime
    question separate from this local-shadowing slice

### 2026-04-11: Earlier Live Blockers Resolved Later The Same Night

These earlier blockers are now resolved by the later entries above:

- `0006_let_shadowing` now emits runnable JS through fresh-name alpha
  stabilization plus explicit integer primitive lowering
- `0007_tuples_and_patterns` now has an owned sibling `src/js/Printf.js`
  surface, approved snapshots, and runnable emitted JS under `bun` when
  materialized next to `src/js/Printf.js` and `src/js/riot-runtime.js`

Keep those old concerns in mind as design constraints, but do not treat them
as the current live blockers for the loop anymore.

### 2026-04-11: Source-Driven Sequence Before Conditional Example

Picked immediate next target 5 in the smallest slice that adds body-local
sequencing without new FFI or package-import decisions.

- fixture:
  - `tests/fixtures/corpus/0104_sequence_before_conditional.ml`
  - `tests/fixtures/js/0104_sequence_before_conditional.core_ir.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.jir.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.js.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.pipeline.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.lowering.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.codegen.expected`
  - `tests/fixtures/js/0104_sequence_before_conditional.compilation.expected`
- invariant:
  - a supported source-level `expr1; expr2` inside a top-level binding or
    lambda body must lower from `typ` into backend-neutral
    `Core_ir.Expr.Sequence`
  - when that sequence is already in tail position, the existing JS path should
    keep lowering it to linear statements plus a final `return`, not another
    ad hoc printer-side trick
  - this example should stay runnable as standalone ESM without introducing new
    runtime helpers beyond what the current JS backend already owns
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0104_sequence_before_conditional.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/architecture.md`
  - `AGENTS.md`
  - `TODO.md`
- effect:
  - the shared source-driven slice now lowers source sequence expressions into
    explicit `Core_ir.Expr.Sequence` nodes instead of rejecting them at the
    `typ -> raml` boundary
  - the emitted JS for `choose` stays linear in the function body:
    `consume(undefined); const selected = ...; return selected;`
  - the new example extends the runnable JS corpus with body-local sequencing
    plus a later local conditional result
- validation:
  - `riot fix ./compiler/raml` still reports the existing unrelated lint
    backlog in `src/js/jir/types.ml*`, `src/native/emitter/*`, and
    `src/native/linker/*`
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed after promoting the seven
    `0104_sequence_before_conditional.*.expected` snapshots
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js/sequence_before_conditional.mjs` passed after
    materializing the emitted `"js"` field
- next:
  - use `0104_sequence_before_conditional` alongside the tail-position IR
    fixtures as the proof point for any future flattening pass
  - keep the `0006_let_shadowing` legality/runtime issue separate instead of
    folding it into this source-sequence slice

### 2026-04-11: Source-Driven Local Function Capture Example

Picked TODO example 06 in the smallest slice that proves shared local `let`
lowering and keeps the JS output runnable without new runtime or import work.

- fixture:
  - `tests/fixtures/corpus/0103_local_function_capture.ml`
  - `tests/fixtures/js/0103_local_function_capture.core_ir.expected`
  - `tests/fixtures/js/0103_local_function_capture.jir.expected`
  - `tests/fixtures/js/0103_local_function_capture.js.expected`
  - `tests/fixtures/js/0103_local_function_capture.pipeline.expected`
  - `tests/fixtures/js/0103_local_function_capture.lowering.expected`
  - `tests/fixtures/js/0103_local_function_capture.codegen.expected`
  - `tests/fixtures/js/0103_local_function_capture.compilation.expected`
- invariant:
  - a supported source-level local `let` inside a top-level function body must
    lower from `typ` into backend-neutral `Core_ir.Expr.Let`
  - a local function bound by that `let` must keep lexical capture explicit in
    shared IR, so the nested lambda body can still reference the outer `flag`
    parameter
  - once that shared `let` exists, the JS path should lower it to local
    statement-level declarations in the enclosing function body and stay
    runnable as standalone ESM
- ownership:
  - `src/typ_lowering.ml`
  - `tests/fixtures/corpus/0103_local_function_capture.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/architecture.md`
  - `AGENTS.md`
  - `TODO.md`
- effect:
  - the source-driven suite now covers the first local binding group in a
    function body instead of only top-level bindings and conditionals
  - the emitted JS keeps the local function as a nested `const` declaration
    inside the enclosing function body and remains runnable under `bun`
- validation:
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js/local_function_capture.mjs` passed after
    materializing the emitted `"js"` field
  - `riot fix ./compiler/raml` remains blocked by the existing unrelated
    warnings in `src/js/jir/types.ml*`, `src/native/emitter/*`, and
    `src/native/linker/*`
- next:
  - keep local shadowing and broader local-expression forms separate from this
    first local-`let` slice unless one source fixture proves they are needed
  - use the next runnable JS example to decide whether body-local flattening
    should stay as targeted lowering or earn a dedicated JS pass

### 2026-04-11: Grouped Initialization Order Example Landed

Picked the next source-driven JS slice for TODO example 04 and promoted the
pending snapshots once package validation confirmed the backend behavior.

- fixture:
  - `tests/fixtures/corpus/0102_grouped_initialization_order.ml`
- invariant:
  - interleaved top-level binding and eval groups must preserve source order
    through JS lowering
  - a later top-level binding whose initializer calls `print_endline` must stay
    after an earlier `let () = print_endline before`; if later binding init is
    hoisted ahead of eval items, the emitted JS prints in the wrong order
- ownership:
  - `tests/fixtures/corpus/0102_grouped_initialization_order.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
- current status:
  - the new source fixture and JS/example/compilation test-bin filters are in
    place
  - the generated JS for the example prints `before` then `after` under `bun`,
    which confirms the top-level init order is preserved
  - `riot build raml` passed
  - `riot test -p raml` passed after promoting the
    `0102_grouped_initialization_order.*.expected` snapshots
- next:
  - move on to the next runnable source-driven JS example
  - keep grouped top-level ordering as the proof point if a dedicated grouping
    pass becomes necessary later

### 2026-04-11: Source-Driven Tail Conditional Direct Call Example

Picked target 4 in the smallest slice that also clears target 5's current
source-driven blocker.

- fixture:
  - `tests/fixtures/corpus/0101_tail_conditional_direct_call.ml`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.core_ir.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.jir.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.js.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.pipeline.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.lowering.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.codegen.expected`
  - `tests/fixtures/js/0101_tail_conditional_direct_call.compilation.expected`
- invariant:
  - a source-level `if ... then ... else ...` inside a supported top-level
    function body must lower from `typ` into backend-neutral
    `Core_ir.If_then_else`
  - once that shared conditional exists, the JS path should preserve the
    already-landed tail-position conditional statementification and emit
    standalone runnable ESM with no new runtime imports
- ownership:
  - `src/typ_lowering.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
  - `docs/architecture.md`
  - `AGENTS.md`
  - `TODO.md`
- effect:
  - the JS source-driven suite now includes a function body that lowers through
    shared `if` semantics instead of only constants and identity-style calls
  - the emitted JS keeps the conditional as structured JS control flow inside
    the function body and stays runnable under `bun` as standalone ESM
- validation:
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js/tail_conditional_direct_call.mjs` passed after
    materializing the emitted `"js"` field
  - `riot fix ./compiler/raml` remains blocked by the existing unrelated
    warnings in `src/native/emitter/*`, `src/native/linker/*`, and the
    pre-existing `src/js/jir/types.ml*` naming warning
- next:
  - use the next JS source-driven example to separate grouped top-level init
    order from plain source conditional support
  - keep local `let` support as a separate shared-lowering slice so closure/env
    pressure does not get mixed into grouped-init or JS-flattening work

### 2026-04-11: Source-Driven Top-Level Function Direct Call Example

Picked target 4 again in the smallest slice that exercises source-driven
top-level lambdas plus direct calls without taking on new runtime imports,
package-import materialization, or `external`.

- fixture:
  - `tests/fixtures/corpus/0003_top_level_function_direct_call.ml`
  - `tests/fixtures/js/0003_top_level_function_direct_call.core_ir.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.jir.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.js.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.pipeline.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.lowering.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.codegen.expected`
  - `tests/fixtures/js/0003_top_level_function_direct_call.compilation.expected`
- invariant:
  - a top-level positional function and a later top-level direct call using a
    previously bound value lower from source to:
    - one exported `const` string binding
    - one exported `const` initialized with a JS function expression
    - one exported `const` initialized with a direct call to that binding
  - this example should not materialize runtime imports or backend-local
    helpers; the emitted JS should stay standalone ESM
- ownership:
  - `tests/fixtures/corpus/0003_top_level_function_direct_call.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
- effect:
  - the JS source-driven suite now covers the first exported top-level lambda
    plus source-driven direct-call path instead of only side effects and raw
    constants
  - emitted JS for the example is runnable under `bun` as a standalone ESM
    module with no sibling runtime files
- validation:
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js-top-level-function-direct-call.mjs` passed after
    materializing the emitted `"js"` field
  - `riot fix ./compiler/raml` is still blocked by pre-existing warnings in
    `src/native/emitter/*`, `src/native/linker/*`, and the existing
    `src/js/jir/types.ml*` naming warning; that blocker is outside this slice
- next:
  - use the next runnable source-driven JS example to prove grouped top-level
    initialization order before adding a named grouping pass
  - keep arithmetic-plus-`Printf` and list-driven examples out of the JS
    source-driven lane until the runtime and foreign-binding boundary is
    explicit enough

### 2026-04-11: Source-Driven Exported Constants Example

Picked target 4 in the smallest slice that adds the next runnable JS example
without taking on new runtime or FFI surface area.

- fixture:
  - `tests/fixtures/corpus/0002_exported_constants.ml`
  - `tests/fixtures/js/0002_exported_constants.core_ir.expected`
  - `tests/fixtures/js/0002_exported_constants.jir.expected`
  - `tests/fixtures/js/0002_exported_constants.js.expected`
  - `tests/fixtures/js/0002_exported_constants.pipeline.expected`
  - `tests/fixtures/js/0002_exported_constants.lowering.expected`
  - `tests/fixtures/js/0002_exported_constants.codegen.expected`
  - `tests/fixtures/js/0002_exported_constants.compilation.expected`
- invariant:
  - top-level exported `int`, `bool`, `float`, `string`, and `unit` values
    lower from source to plain JS `const` bindings plus a final `export` block
  - this example should not materialize runtime imports; `unit` stays JS-local
    as `undefined`
- ownership:
  - `tests/fixtures/corpus/0002_exported_constants.ml`
  - `tests/example_fixture_tests.ml`
  - `tests/js_fixture_tests.ml`
  - `tests/compilation_fixture_tests.ml`
- effect:
  - the JS source-driven suite now covers a module with exported values instead
    of only top-level side effects
  - emitted JS for the example is runnable under `bun` as a standalone ESM
    module with no sibling runtime files
- validation:
  - `riot fmt ./compiler/raml` passed
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - `bun run /tmp/raml-js/exported_constants.mjs` passed after materializing
    the emitted `"js"` field
  - `riot fix ./compiler/raml` is still blocked by pre-existing warnings in
    `src/native/emitter/*`, `src/native/linker/*`, and the existing
    `src/js/jir/types.ml*` naming warning; that blocker is outside this slice
- next:
  - add the next runnable source-driven JS example that exercises direct calls
    without immediately forcing `external` or package-import materialization
  - keep arithmetic-plus-`Printf` examples out of the JS source-driven lane
    until the runtime and foreign-binding boundary is explicit enough

### 2026-04-11: Effect-Position Conditional Statementification

Picked target 3 again, but only for conditionals that are already in effect
position inside statement-producing JS bodies.

- fixture:
  - `tests/fixtures/jir_lowering/effect_conditionals_in_function_bodies.json`
  - `tests/fixtures/js/effect_conditionals_in_function_bodies.expected`
- invariant:
  - when `If_then_else` is already in effect position inside a function body or
    a let-generated IIFE body, lower it to a structured JS `if` statement
  - keep top-level eval lowering and expression-position conditionals on their
    existing paths; this slice only widens the already-structured statement
    surface
- ownership:
  - `src/js/jir/lowering.ml`
  - `docs/js/ir.md`
  - `AGENTS.md`
- effect:
  - a sequence like `if flag then left value else (right value; finish ()); value`
    now lowers to branch-local statements plus a final `return value`
  - the emitter stays unchanged because the control-flow shape is made explicit
    in `JIR` before `JST` lowering
- next:
  - decide whether effect-position `let` should earn the same treatment or wait
    for the first dedicated flattening pass
  - keep top-level eval-item conditionals separate unless a fixture proves that
    broader ownership boundary is worth taking now

### 2026-04-11: Explicit JS Runtime Import For `print_endline`

Picked target 1 in the smallest slice that removes the remaining ambient-global
gap from `0001_hello_world`.

- fixture:
  - `tests/fixtures/js/0001_hello_world.jir.expected`
  - `tests/fixtures/js/0001_hello_world.js.expected`
  - `tests/fixtures/js/0001_hello_world.pipeline.expected`
  - `tests/fixtures/js/0001_hello_world.lowering.expected`
  - `tests/fixtures/js/0001_hello_world.codegen.expected`
  - `tests/fixtures/js/0001_hello_world.compilation.expected`
- invariant:
  - direct JS-lowered `print_endline` calls must materialize as explicit runtime
    imports from `./riot-runtime.js`, not as ambient globals
  - primitive dispatch and top-level I/O should share the same first runtime
    module surface instead of drifting into separate import stories
- ownership:
  - `src/js/jir/types.ml`
  - `src/js/jir/runtime.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/riot-runtime.js`
  - `docs/js/runtime-and-ffi.md`
- effect:
  - `0001_hello_world` now emits:
    - `import { print_endline as __print_endline } from "./riot-runtime.js"`
    - `__print_endline("hello, world")`
  - the existing primitive fixtures now import `callPrimitive` from the same
    sibling runtime module path instead of the earlier package-shaped placeholder
- validation:
  - `riot build raml` passed
  - `riot test -p raml` passed
  - the emitted `hello_world` JS runs under `bun` when materialized next to
    `src/js/riot-runtime.js`
  - JS fixture drift was promoted for the affected JS snapshots only
  - native pass-trace and snapshot expansion is green for the first three
    native corpus programs, but it is still a separate native slice and should
    not be counted as JS progress
- next:
  - decide whether ambient runtime names should stay limited to direct-call
    lowering until typed `external` work lands
  - keep native snapshot churn separate so the package-level red state does not
    get misreported as a JS regression

### 2026-04-10: Tail-Position Sequence Flattening

Picked target 3 in the smallest slice that had a concrete failing invariant.

- fixture:
  - `tests/fixtures/jir_lowering/tail_sequences_in_let_bodies.json`
  - `tests/fixtures/js/tail_sequences_in_let_bodies.expected`
- invariant:
  - when `Sequence` is already in tail position inside a function body or a
    let-generated IIFE body, lower it to linear JS statements ending in
    `return`
  - keep expression-position `Sequence` on the existing IIFE path so scope and
    evaluation order stay explicit without making the emitter smarter
- ownership:
  - `src/js/jir/lowering.ml`
  - no emitter changes
- effect:
  - nested zero-arg IIFEs stop accumulating for tail sequences inside
    let-generated bodies
  - the existing `full_core_ir` snapshot now stays on the flatter shape again
- validation:
  - `riot build raml` passed
  - `riot test -p raml` passed
  - `git diff --check -- compiler/raml` passed
  - the current hello-world JS snapshots are green through typing, `Core_ir`,
    `JIR`, and JS codegen
  - at this point the emitted `hello_world` JS still referenced bare
    `print_endline`; that runtime gap was resolved later by the explicit
    runtime-import slice on 2026-04-11
  - native snapshot churn showing up in the same diff is out of scope for this
    JS slice and should not be treated as JS progress
- next:
  - materialize an explicit JS runtime/import boundary for `print_endline`
    before calling hello-world "runnable JS"
  - keep the tail-sequence invariant as the first proof point for a deliberate
    flattening pass instead of adding more ad hoc IIFE lowering

### 2026-04-11: Scope Correction

The earlier `JST.Statement.If` type break was fixed, so the package is green
again.

- what moved:
  - there is real JS work in `JIR`/`JST` around statement-level `if`
    representation and normalization
  - there is also native pass-trace work for `NIR`, `MIR`, and `LIR`
    snapshot surfaces
- scope correction:
  - this is currently two slices mixed together: a JS `if` slice and a native
    pass-snapshot slice
  - that mix makes review harder and hides whether the JS loop is actually
    improving the JS backend
- next:
  - keep the JS `if` slice and the native pass-trace slice conceptually
    separate even when they validate together
  - keep the native pass-trace work separate unless the change is explicitly
    being treated as a shared pipeline contract update

### 2026-04-10: Tail-Position Conditional Statementification

Picked target 3 again, but only for the tail-position conditional case that the
previous sequence slice made more obvious.

- fixture:
  - `tests/fixtures/jir_lowering/tail_conditionals_in_function_bodies.json`
  - `tests/fixtures/js/tail_conditionals_in_function_bodies.expected`
- invariant:
  - when `If_then_else` is already in tail position inside a function body,
    lower it to a structured JS `if` statement whose branches end in explicit
    `return`
  - keep expression-position conditionals as expressions; only statementify the
    tail-position case that simplifies later JS printing without teaching the
    emitter new control-flow tricks
- ownership:
  - `src/js/jir/types.ml`
  - `src/js/jir/lowering.ml`
  - `src/js/jir/passes/normalize.ml`
  - `src/js/jst/types.ml`
  - `src/js/jst/lowering.ml`
  - `src/js/jst/emitter.ml`
- effect:
  - branch-local `let` and `sequence` nodes stop forcing branch-local IIFEs
    when both branches are already in tail position
  - `JIR` and `JST` now make this control-flow shape explicit before emission
- next:
  - extend the same statementification idea to effect-position conditionals or
    wait for a dedicated flattening pass
  - add the smallest runnable JS example once the JS source-driven fixture set
    grows beyond `hello_world`
