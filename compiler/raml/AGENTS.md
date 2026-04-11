# raml AGENTS

`compiler/raml` is Riot's multi-backend compiler package.

It sits between `typ`'s semantic tree and backend-specific lowerings for JS,
native, and wasm.

## Read First

Start here before changing code:

- `compiler/raml/docs/index.md`
- `compiler/raml/docs/architecture.md`
- `compiler/raml/JS_LOOP.md`
- `compiler/raml/NATIVE_LOOP.md`
- `compiler/raml/TODO.md`

Then read the owning backend manual:

- `compiler/raml/docs/js/index.md`
- `compiler/raml/docs/native/index.md`
- `compiler/raml/docs/wasm/index.md`

For native-backend work, also read:

- `compiler/raml/docs/native/strategy.md`
- `compiler/raml/NATIVE_LOOP.md`

If the change touches the `typ -> raml` handoff, also read:

- `packages/typ/AGENTS.md`

## Current Compiler Shape

The shared compiler center is `Raml Core IR`.

Today that means:

- `Core_ir.Compilation_unit` is the shared unit-level wrapper
- `Core_ir.Binding_group` owns ordered init groups plus exports
- `Core_ir.Init_item` distinguishes named `Binding` items from effectful `Eval`
  items
- `Core_ir.Expr` is Lambda-shaped: `Constant`, `Var`, `Apply`, `Lambda`,
  `Let`, `Sequence`, `Tuple`, `Tuple_get`, `If_then_else`, and `Primitive`

Backend ownership is:

- JS backend code lives under `compiler/raml/src/js/`
- `JIR` is JS-late and may become JS-shaped; grow
  `compiler/raml/src/js/jir/`
- `JST` is the final JS syntax/emission tree and should live under
  `compiler/raml/src/js/jst/`
- the current JS runtime/module surface materializes as explicit imports from
  sibling files under `compiler/raml/src/js/`; low-level helpers such as
  `print_endline`, `print_newline`, `print_int`, `print_string`,
  `print_char`,
  `callPrimitive`, and `makeCurried` live in `./riot-runtime.js`, while the
  current source-visible `Printf.printf` slice lives in `./Printf.js`; do not
  reintroduce ambient globals in emitted JS
- current JS lowering also owns lexical name stabilization for shadowing local
  binders before `JST` emission, and direct `print_endline` / `print_newline`
  / `print_int` / `print_string` / `print_char` calls now lower through
  explicit named imports from `./riot-runtime.js` instead of ambient globals;
  the first
  source-driven integer arithmetic, float arithmetic, string concatenation
  through `^`, source-visible `string_of_int`, finite-input
  `string_of_float`, valid-input `int_of_string`, finite-input
  `float_of_string`, `sqrt`, and `<`, `<=`, `>`, `>=`, `=`, and `<>` direct
  calls now lower through `callPrimitive` in `./riot-runtime.js` instead of
  bare operator identifiers, a bare `string_of_int`, a bare
  `string_of_float`, a bare `int_of_string`, a bare `float_of_string`, or an
  ambient `sqrt`; OCaml-exact float-string formatting and invalid
  `int_of_string` / `float_of_string` failure semantics still stay separate
  until narrower runtime/exception slices land;
  direct `not`, `&&`, and `||` calls now lower through
  nested `JIR` conditional expressions so short-circuit behavior stays
  explicit before emission; multi-parameter compiled lambdas now lower through
  `makeCurried` so under-applied calls stay source-correct under JS; the
  first dedicated flatten slices also rewrite
  effect-position zero-arg IIFEs in `JIR` before alpha stabilization when
  their body can be converted from tail returns into plain statements, and now
  rewrite statement-shaped declaration-initializer zero-arg IIFEs through a
  temp binding plus lexical `Block` plus final declaration so initializer-local
  shadowing does not leak into module scope or steal the exported binding name;
  the first alias-cleanup slice now runs after alpha stabilization and removes
  immutable identifier-only temps such as tuple-destructure or match-scrutinee
  aliases when their target name is never assigned and the alias is not
  exported; the first dead-binding slice now also removes unexported immutable
  `const` bindings whose initializer is already effect-free when the name is
  unused in scope, and a final `JIR` normalize step recomputes imports from
  the live body so helpers referenced only from eliminated dead bindings do
  not survive into emitted JS; the later import-materialization slice now also
  rewrites `Imported` and `Runtime_helper` expressions into plain local
  identifiers after that final import-collection step, so `program.imports`
  stays the only import-declaration surface handed to `JST`;
  compatible named/default imports from the same module now materialize as one
  `JST` import declaration before emission, while namespace imports stay
  separate
- tail- and effect-position JS control flow should become structured
  `JIR`/`JST` statements before emission; do not push that branching logic into
  the emitter
- the current source-driven `Typ -> Core_ir` slice now accepts backend-neutral
  `if ... then ... else ...`, source sequence expressions, direct source-level
  `ignore expr` calls lowered through that same shared sequence-plus-unit path,
  tuple construction,
  backend-neutral char literals as shared `Core_ir.Constant.Char` values,
  source anonymous function expressions inside supported bindings and lambda
  bodies,
  immutable record construction/field access/functional update via shared
  tuple lowering, the first closed ordinary-variant constructor slice via
  shared tagged tuples, including the current phantom-index-only GADT-style
  vector slice where the type indices erase to the same runtime constructor
  layout, the prelude `list`, `option`, and `result`
  constructor/match slices through that same tagged-tuple contract, with
  multi-argument constructor payloads such as prelude `::` packed into one
  shared tuple payload before backend lowering,
  exhaustive constructor-only `match` lowering via shared tag checks, local
  `let` expressions with variable or tuple binders inside supported top-level
  bindings and lambda bodies, and function-only recursive local `let` groups
  with variable binders; top-level type declarations, top-level declared
  values such as `external print_endline : string -> unit = "print_endline"`,
  and top-level `open` statements with no runtime effect should stay out of
  `Core_ir`; keep that boundary shared and leave JS statementification in
  `compiler/raml/src/js/`
- native backend code should grow under `compiler/raml/src/native/`
- the native scaffold now lives under:
  `compiler/raml/src/native/nir/`,
  `compiler/raml/src/native/mir/`,
  `compiler/raml/src/native/lir/`,
  and `compiler/raml/src/native/emitter/`
- the first native late IR is `NIR`, followed by `MIR` and `LIR`
- wasm backend code should grow under `compiler/raml/src/wasm/`
- wasm should get its own post-`Core_ir` runtime/host IR family
- only extract a shared post-`Core_ir` native/wasm layer later if the
  implemented backends prove they actually share one
- backend selection should be driven by explicit `host` and `target` triples;
  the target triple chooses the backend family, and the host triple informs
  toolchain/runtime decisions around that backend
- `Core_ir` must stay backend-neutral

Top-level compiler entrypoints are exposed through:

- `compiler/raml/src/raml.mli`
- `Raml.Config`
- `Raml.Event`
- `Raml.compile`
- `Raml.compile_source`

Use `Raml.Config.make ~host ~target ()` when backend selection matters.
Do not infer the backend family from the machine running the compiler.
The target triple decides the backend; the host triple only describes where
the compiler is executing.

Keep diagnostics/event emission structured. Do not replace it with ad hoc text
logging.

## Rules

1. Work example-first, not pass-first. Grow the compiler by making one source
   example move coherently through `Core_ir`, the implemented backend IRs, and
   codegen.
2. Do not move to the next example until the current one is supported across
   every backend layer that already exists.
3. Keep `Core_ir` backend-neutral. Do not leak raw JS, JS-specific optional
   encoding, ESM/CJS choices, wasm imports, or native ABI details into the
   shared IR.
4. Put JS-specific runtime and module-system choices in `JIR`, not in
   `Core_ir`.
5. Do not invent a shared native/wasm post-`Core_ir` layer in advance. Grow
   native and wasm separately until a real shared seam proves itself.
6. Preserve structured compiler events. If a top-level compiler stage changes,
   update `Raml.Event` payloads and their callers deliberately.
7. Prefer explicit unsupported cases with structured errors over silent
   fallback or implicit dropping of semantics.
8. When IR contracts move, update docs, snapshots, `TODO.md`, and this
   `AGENTS.md` in the same change.

## Testing

`compiler/raml` is snapshot-driven.

Use the existing fixture families under `compiler/raml/tests/fixtures/`:

- `corpus/`
- `core_ir/`
- `typ_lowering/`
- `jir/`
- `jir_lowering/`
- `js/`
- `native/`
- `wasm/`

The shared `*.ml` source corpus lives under `compiler/raml/tests/fixtures/corpus/`.
Backend-specific suites should read from that corpus when they are source-driven
and keep their approved snapshots under backend directories such as `js/`,
`native/`, or `wasm/`.
Shared IR input fixtures such as `core_ir/`, `jir/`, and `jir_lowering/` may
keep their source `.json` inputs where they are, but their backend-owned
`.expected` files should still live under the backend snapshot directories.
Ordered corpus filenames like `0001_hello_world.ml` are fixture names, not
compiler-facing module identities. When feeding corpus files into the compiler,
strip the numeric ordering prefix from the logical relpath first.

The native fixture family is corpus-driven under
`compiler/raml/tests/fixtures/corpus/` and keeps approved snapshots under
`compiler/raml/tests/fixtures/native/`.
The active native corpus coverage currently includes
`0001_hello_world.ml`,
`0002_exported_constants.ml`,
`0002_integer_arithmetic.ml`,
`0003_top_level_function_direct_call.ml`,
`0003_float_arithmetic.ml`,
`0004_boolean_logic.ml`,
`0005_if_then_else.ml`,
`0006_let_shadowing.ml`,
`0007_tuples_and_patterns.ml`,
`0008_records_and_updates.ml`,
`0009_variants_and_match.ml`,
`0010_option_pipeline.ml`,
`0012_list_recursion_sum.ml`,
`0013_tail_recursive_factorial.ml`,
`0014_mutual_recursion_even_odd.ml`,
`0022_local_functions_and_closures.ml`,
`0023_partial_application.ml`,
`0025_custom_infix_operators.ml`,
`0026_sequence_and_ignore.ml`,
`0049_function_composition_pipeline.ml`,
`0057_phantom_length_vector.ml`, and
`0101_tail_conditional_direct_call.ml` through
`0128_print_char.ml`, with
pass-local snapshots for `normalize`, `simplify`, `canonicalize`,
`insert_polls`, `layout_frames`, and `schedule`, plus final
`*.nir.expected`, `*.mir.expected`, `*.lir.expected`, `*.native.expected`,
and `*.link.expected`.
Keep native work corpus-driven and preserve explicit snapshot surfaces for
every named native pass.

For new compiler behavior:

1. Add or update the source example first.
2. Snapshot the shared IR and backend projections separately.
3. Prefer small, readable snapshots over one giant end-to-end dump.
4. Keep example fixtures cross-target so feature drift is obvious.

The example-driven suite is the cross-backend regression layer.
Keep it centered on `Raml.Example_pipeline`, so every example snapshots the
shared `Core_ir` view plus backend projections in one place.

Use the public `Raml.compile_source` / `Raml.compile` API to snapshot the
selected-backend contract separately through `Raml.Compilation`.

## Validate

Run this stack in order:

```sh
riot fix ./compiler/raml
riot fmt ./compiler/raml
riot build raml
riot test -p raml
git diff --check -- compiler/raml
```

Interpret results carefully:

- snapshot drift is not automatically a regression; inspect whether the old
  behavior or the new behavior is wrong
- if the IR contract changed intentionally, update the snapshots in the same
  change
- if `riot test -p raml` stops running the fixture bins, the harness regressed

## Common Pitfalls

- Do not fix a JS example by hardcoding runtime names directly in the emitter.
  Decide the JS boundary in `JIR` or in typing/runtime setup first.
- Do not add backend-specific fields to `Core_ir` just to get one example
  green.
- Do not hide top-level side effects behind fake named bindings. Use explicit
  init/eval items.
