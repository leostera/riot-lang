# Raml Native Loop

This file is the working loop for extending `compiler/raml-native`.

Use it to grow the native path one corpus program at a time until the shared
source corpus compiles cleanly through the implemented native layers and the
native snapshots stay honest.

The goal is not to clone `vendor/ocaml/asmcomp`.

The goal is to learn from `asmcomp`'s seams and pass order, borrow the useful
ideas, and adapt them to `raml`'s own `Core_ir -> NIR -> MIR -> LIR` pipeline.

## Read First

Always read these before changing native code:

- [AGENTS.md](./AGENTS.md)
- [compiler/raml/TODO.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/TODO.md)
- [compiler/raml/docs/architecture.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/architecture.md)
- [compiler/raml/docs/native/index.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/native/index.md)
- [compiler/raml/docs/native/strategy.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/native/strategy.md)
- [compiler/raml/docs/native/pipeline.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/native/pipeline.md)
- [compiler/raml/docs/native/mach.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/native/mach.md)
- [compiler/raml/docs/native/targets.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml/docs/native/targets.md)

Then read the `asmcomp` source that matches the layer you are touching:

- `vendor/ocaml/asmcomp/asmgen.ml`
- `vendor/ocaml/asmcomp/selectgen.ml`
- `vendor/ocaml/asmcomp/mach.mli`
- `vendor/ocaml/asmcomp/polling.ml`
- `vendor/ocaml/asmcomp/comballoc.ml`
- `vendor/ocaml/asmcomp/CSEgen.ml`
- `vendor/ocaml/asmcomp/deadcode.ml`
- `vendor/ocaml/asmcomp/liveness.ml`
- `vendor/ocaml/asmcomp/spill.ml`
- `vendor/ocaml/asmcomp/split.ml`
- `vendor/ocaml/asmcomp/reloadgen.ml`
- `vendor/ocaml/asmcomp/linearize.ml`
- `vendor/ocaml/asmcomp/stackframegen.ml`
- `vendor/ocaml/asmcomp/schedgen.ml`
- `vendor/ocaml/asmcomp/asmlink.ml`

If the change touches textual assembly or target profiles, also read:

- `compiler/asm/AGENTS.md`

## Current Native Shape

Today the intended native stack is:

```text
Typ Semantic Tree
  -> Raml Core IR
  -> shared lowering/passes
  -> Native.Nir
  -> Native.Mir
  -> Native.Lir
  -> Native.Emitter
  -> Native.Linker
```

Current ownership is:

- `Core_ir` stays backend-neutral
- `NIR` is the first native-only layer
- `MIR` is the machine-oriented late IR
- `LIR` is the flat pre-emission IR
- `Emitter` owns target text generation
- `Linker` owns assembler/link planning and invocation

Do not route through the native backend unless the target triple selects a
native backend family.

Examples:

- `host = aarch64-apple-darwin`, `target = aarch64-apple-darwin`
  means native AArch64 Darwin
- `host = aarch64-apple-darwin`, `target = x86_64-unknown-linux-gnu`
  means native cross-compilation
- `host = aarch64-apple-darwin`, `target = js-unknown-ecma`
  means the JS backend, not native

The target triple chooses the backend family.
The host triple only describes where the compiler is running.

## Native Work Loop

Every native slice should follow this loop:

1. Pick one corpus program from `compiler/raml/tests/fixtures/corpus/`.
2. Decide the exact host and target triples you are supporting in this slice.
3. Run the corpus-driven native fixture tests and find the first failing stage.
4. Fix the earliest wrong layer, not the latest visible symptom.
5. Add the smallest IR shape or pass needed for that program to move forward.
6. Snapshot every native stage the program reaches.
7. Snapshot every named pass that changed the program.
8. Re-run the full native verification stack.
9. Only move to the next corpus program when the current one is green across
   all implemented native layers.
10. If you are migrating the harness or snapshot layout, do not reduce existing
    coverage unless equivalent corpus-backed coverage has already replaced it.
11. Do not widen the active native corpus filter to a new program until its
    approved `*.expected` snapshot family exists. Pending `*.expected.new`
    files are review artifacts, not finished coverage.
12. Do not mark a native backlog item `[x]` unless the native verification
    stack is green in the current worktree.
13. If `riot test -p raml` is red for another backend while the native suite is
    green, call that out explicitly instead of downgrading already-verified
    native items.

Treat the corpus as the contract.
Do not add passes or IR nodes only because they sound generally useful.
Add them because a concrete source program needs them.

## Earliest-Failure Rule

When a corpus program fails, fix the first bad layer:

- if typing is wrong, do not patch native lowering
- if `Core_ir` is wrong, do not patch `NIR`
- if `NIR` cannot express the runtime obligation, do not fake it in the emitter
- if `MIR` lacks an operation or legality rule, do not encode it as raw text in
  `LIR`
- if emission is wrong, do not mutate `LIR` into target syntax

This keeps the layers honest and prevents emitter-driven compiler design.

## What To Borrow From `asmcomp`

Borrow pass boundaries and sequencing.
Do not copy OCaml's runtime vocabulary blindly.

Good things to borrow:

- explicit instruction-selection stage
- explicit polling/safepoint insertion
- local canonicalization before optimization
- dead-code removal and CSE as named passes
- liveness as its own analysis
- spill suggestion and live-range splitting
- reload/regalloc as an iterative legality loop
- linearization as a real IR boundary
- stack-frame analysis before emit
- post-linear scheduling
- explicit assembler/linker planning

Do not copy directly:

- OCaml's exact `Cmm` primitive set
- OCaml object-model helpers
- OCaml-specific runtime symbols and metadata scheme
- assumptions that `zort` and OCaml share the same raw value model

## Pass Map

This is the intended borrowing map from `asmcomp` into `raml`.

### Shared / pre-native

These belong before native lowering:

- Lambda-like normalization
- match lowering
- closure and application shaping
- init ordering
- recursive binding lowering

If a pass is still backend-neutral, keep it out of native.

### `NIR`

`NIR` is where the compiler becomes native-runtime-shaped.

It should own:

- runtime imports and helper entrypoints
- unit entry and top-level init sequencing
- low-level value and constant materialization choices
- allocation, poll, barrier, and metadata hooks
- target-profile-sensitive calling boundaries when they affect IR shape

Existing pass slots:

- `Native.Nir.Passes.Normalize`
- `Native.Nir.Passes.Simplify`

These should stay plain function calls in the stage pipeline.
Do not introduce a generic pass runner or pass objects to execute them.

Every pass should have its own snapshot surface.

The minimum expectation is:

- pre-pass snapshot
- post-pass snapshot
- final stage snapshot

Good future pass candidates borrowed from `Lambda`/`Cmm` pressure:

- constant lifting
- closure layout materialization
- explicit data layout lowering
- direct-call classification
- unit-entry normalization

### `MIR`

`MIR` should play the role Mach plays structurally.

It should own:

- selected pseudo-instructions
- explicit control-flow blocks
- direct and indirect call forms
- polling points
- target legality constraints that are still above final emission

Existing pass slots:

- `Native.Mir.Passes.Canonicalize`
- `Native.Mir.Passes.Insert_polls`
- `Native.Mir.Passes.Copy_propagate`
- `Native.Mir.Passes.Dead_code`

These should stay plain function calls in the stage pipeline.
Do not introduce a generic pass runner or pass objects to execute them.

Every pass should have its own snapshot surface.

Good future pass candidates borrowed from `asmcomp`:

- `Comballoc`
- `Cse`
- `Deadcode`
- `Liveness`
- `Spill`
- `Split`
- `Regalloc`
- `Reload`

Do not add all of these at once.
Add them in response to concrete corpus failures or obvious invariants.

### `LIR`

`LIR` should be the flat, restartable, pre-emission boundary.

It should own:

- labels and flat control transfer
- final instruction ordering
- frame/layout metadata required by emission
- branch-distance repair if a target needs it

The current `aarch64-apple-darwin` slice now makes that explicit:
`Native.Lir.Passes.Layout_frames` computes `LIR.Procedure.frame`, and native
emitters are expected to consume that metadata instead of rebuilding stack-slot
layouts themselves.

Existing pass slots:

- `Native.Lir.Passes.Layout_frames`
- `Native.Lir.Passes.Simplify`
- `Native.Lir.Passes.Schedule`

These should stay plain function calls in the stage pipeline.
Do not introduce a generic pass runner or pass objects to execute them.

Every pass should have its own snapshot surface.

Good future pass candidates borrowed from `asmcomp`:

- `Branch_relaxation`
- `Finalize_prologue_epilogue`
- `Record_frame_descriptors`

### `Emitter` and `Linker`

Keep these explicit and late.

`Emitter` should:

- render target-specific assembly using `compiler/asm`
- own target text conventions, symbol spelling, and sections
- never compensate for missing semantic lowering
- never recompute frame layouts that `LIR` already attached to procedures

`Linker` should:

- plan assembler/object/executable commands
- own host-toolchain interaction
- remain separate from text emission

## Snapshot Strategy

Native work must snapshot more than the final result.

For a source-driven native slice, keep or add snapshots for:

- `*.nir.expected`
- `*.mir.expected`
- `*.lir.expected`
- `*.native.expected`
- `*.link.expected`

The native lowering stage JSON should also keep:

- `initial` for the pre-pass program
- `passes` keyed by pass name
- `program` for the final stage result

For every named pass, also keep pass-local snapshots.

Examples:

- `*.nir.normalize.expected`
- `*.nir.simplify.expected`
- `*.mir.canonicalize.expected`
- `*.mir.insert_polls.expected`
- `*.lir.schedule.expected`
- `*.lir.dead_code.expected`
- `*.lir.layout_frames.expected`
- `*.lir.allocate_homes.expected`
- `*.lir.assign_homes.expected`
- `*.lir.legalize.expected`
- `*.lir.calling_convention.expected`

If a pass does not have a snapshot, it is too easy to break it without noticing.

If the source corpus grows faster than the backend, blocked programs should
still snapshot cleanly as structured errors rather than disappearing from the
suite.

When migrating from synthetic fixture inputs to corpus-driven fixtures, keep the
old snapshot family until the new corpus-backed family covers the same contract.
Renaming or moving snapshots is fine. Silent coverage loss is not.

Pending `*.expected.new` files do not count as approved coverage.
Only promoted `*.expected` files count for deciding whether a program belongs in
the active native fixture filter.

## Corpus-Driven Definition Of Done

A corpus program is done for the current native target when all of these are
true:

1. It parses and typechecks through the normal `Raml.compile` path.
2. It lowers through `Core_ir`.
3. It lowers through `NIR`, `MIR`, and `LIR`.
4. Every changed pass has a before/after snapshot.
5. It emits native assembly for the selected target.
6. It produces a valid link plan for the selected target.
7. Its native snapshots are updated and readable.
8. Earlier corpus programs still pass.
9. No active native fixture depends on unpromoted `*.expected.new` snapshots.

Do not declare a program done because the emitter prints something plausible.

## Target Rules

Keep cross-compilation explicit from the start.

- always thread `~host` and `~target`
- never infer backend family from the host machine
- keep `NIR`, `MIR`, and `LIR` as target-generic as possible within the native
  family
- move ISA or object-format details into target modules and `compiler/asm`
- lock one native target first, but do not hardcode the architecture into the
  public compiler contract

The first serious target remains:

- `aarch64-apple-darwin`

But every new native API should be shaped as if
`x86_64-unknown-linux-gnu` and `x86_64-pc-windows-msvc` will eventually exist.

## Verification

When only `compiler/raml` changed:

```sh
riot fix ./compiler/raml
riot fmt ./compiler/raml
riot build raml
riot test -p raml
git diff --check -- compiler/raml
```

When native emission or target profiles touched `compiler/asm` too:

```sh
riot fix ./compiler/asm ./compiler/raml
riot fmt ./compiler/asm ./compiler/raml
riot build asm raml
riot test -p raml
git diff --check -- compiler/asm compiler/raml
```

If native tests stop reading the shared corpus, stop snapshotting any late
native stage, or stop snapshotting named passes, the harness regressed.

## Immediate Native Backlog

Work these in order.

1. [x] Widen the corpus-backed native suite beyond `0001_hello_world.ml`.
   The most recently verified native fixture suite reaches
   `0002_integer_arithmetic.ml`,
   `0002_exported_constants.ml`,
   `0003_float_arithmetic.ml`,
   `0003_top_level_function_direct_call.ml`,
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
   `0057_phantom_length_vector.ml`,
   `0101_tail_conditional_direct_call.ml`,
   `0102_grouped_initialization_order.ml`,
   `0103_local_function_capture.ml`,
   `0104_sequence_before_conditional.ml`,
   `0105_indirect_call_via_returned_closure.ml`,
   `0106_prelude_option_match.ml`,
   `0107_open_std_hello_world.ml`,
   `0108_less_than_comparison.ml`,
   `0109_greater_than_comparison.ml`,
   `0110_less_or_equal_comparison.ml`,
   `0111_greater_or_equal_comparison.ml`,
   `0112_effect_position_local_let.ml`,
   `0113_initializer_shadowing.ml`,
   `0114_top_level_mutual_recursion.ml`,
   `0115_external_print_endline.ml`,
   `0116_prelude_result_match.ml`,
   `0117_dead_local_bindings.ml`,
   `0118_printf_and_print_endline.ml`,
   `0119_string_concat.ml`,
   `0120_string_of_int.ml`,
   `0121_print_newline.ml`,
   `0122_int_of_string.ml`,
   `0123_module_identity.ml`,
   `0124_print_int.ml`,
   `0125_print_string.ml`,
   `0126_string_of_float.ml`, and
   `0128_print_char.ml`, with pass-local snapshots
   plus final `NIR`, `MIR`, `LIR`, native emission, and linker-plan snapshots
   on `aarch64-apple-darwin`.
   The active native fixture filter now also includes
   `0022_local_functions_and_closures.ml`,
   `0023_partial_application.ml`, and
   `0025_custom_infix_operators.ml`, and
   `0026_sequence_and_ignore.ml`.
   `0022_local_functions_and_closures.ml`,
   `0023_partial_application.ml`, and
   `0025_custom_infix_operators.ml`, and
   `0026_sequence_and_ignore.ml` all have approved pass-local plus final
   native snapshots on `aarch64-apple-darwin`.
2. [x] Preserve coverage while migrating. Do not delete `constants`,
   `functions_and_calls`, or `module_identity` native coverage until equivalent
   corpus-backed cases exist and pass.
   The old `constants` and `functions_and_calls` contracts are now covered by
   `0002_exported_constants.ml` and `0003_top_level_function_direct_call.ml`
   with pass-local plus final native snapshots; the old `module_identity`
   contract is now covered by `0123_module_identity.ml`, which keeps the
   compile-time-only module empty through pass-local plus final native
   snapshots on `aarch64-apple-darwin`.
3. [x] Make `0001_hello_world.ml` compile through the native path with real
   entry side effects and a clean linker plan.
   The first corpus-backed native slice now reaches `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`.
4. [x] Keep pass-local snapshots aligned with every existing native pass as
   new corpus programs and new passes land.
   The active native corpus fixtures now snapshot `normalize`, `simplify`,
   `canonicalize`, `insert_polls`, `dead_code`, `schedule`, `layout_frames`,
   `allocate_homes`, `assign_homes`, `legalize`, and `calling_convention`
   alongside the final stage snapshots.
5. [x] Tighten `NIR` around runtime imports, top-level init ordering, direct
   call materialization, and newly introduced shared `Core_ir` forms.
   `Native.Nir.Lowering` now lowers `Core.Expr.Tuple` and
   `Core.Expr.Tuple_get` into explicit runtime helper calls, and the resulting
   `NIR` snapshots record the imported tuple helper surface alongside the
   existing direct-call and entry sequencing shape.
6. [x] Teach `MIR` enough instruction selection to cover constants, calls,
   returns, and simple control flow.
   `MIR` now carries a structured conditional form for expression-valued
   branches, keeping simple control flow above the flat `LIR` boundary.
7. [x] Make `LIR` a real flat control-flow boundary instead of a thin relay
   layer.
   `LIR` now linearizes structured conditionals into labels, zero-branches, and
   explicit jumps before AArch64 emission.
8. [x] Replace pseudo-assembly in `Native.Emitter` with `compiler/asm`-backed
   emission for `aarch64-apple-darwin`.
   The current AArch64 Darwin emitter now renders typed `compiler/asm`
   instructions/directives, including labels and flat branch instructions for
   the `LIR` control-flow boundary.
9. Add the next simplest corpus programs in order:
   [x] `0002_integer_arithmetic.ml`,
   [x] `0003_float_arithmetic.ml`,
   [x] `0004_boolean_logic.ml`,
   [x] `0005_if_then_else.ml`,
   [x] `0006_let_shadowing.ml`,
   [x] `0007_tuples_and_patterns.ml`,
   [x] `0008_records_and_updates.ml`.
   The active native corpus suite now snapshots simple source conditionals,
   local let shadowing, tuple construction, tuple-pattern destructuring, and
   immutable record construction/update lowered through the shared tuple path
   via `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`.
10. Add the next simplest corpus program in order:
   [x] `0009_variants_and_match.ml`.
   The active native corpus suite now snapshots closed ordinary variants plus
   one exhaustive match lowered through shared tagged tuples and tag checks via
   `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`.
11. Add the next simplest corpus program in order:
   [x] `0010_option_pipeline.ml`.
   The active native corpus suite now snapshots nested stdlib `option`
   control flow through the shared tagged-tuple encoding and source-level `=`
   through the existing equality helper via `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`.
12. Add the next simplest corpus program in order:
   [x] `0101_tail_conditional_direct_call.ml`.
   The active native corpus suite now snapshots a top-level function body with
   a tail conditional and a later direct call through `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`.
13. Add the next simplest corpus program in order:
   [x] `0102_grouped_initialization_order.ml`.
   The active native corpus suite now snapshots an ordered top-level binding,
   eval item, and later binding initializer through `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`.
14. Add the next simplest corpus program in order:
   [x] `0103_local_function_capture.ml`.
   The active native corpus suite now snapshots a local function binding that
   captures an outer parameter, lowered through `NIR` as a non-escaping lifted
   helper plus rewritten direct call, then through `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`.
15. Add the next simplest corpus program in order:
   [x] `0104_sequence_before_conditional.ml`.
   The active native corpus suite now snapshots a local sequence expression
   before a conditional result, lowered through `NIR` as explicit ordered
   let-bound evaluation, then through `MIR`, `LIR`, native emission, and
   linker planning on `aarch64-apple-darwin`.
16. Add the next simplest corpus program in order:
   [x] `0105_indirect_call_via_returned_closure.ml`.
   The active native corpus suite now snapshots a local function value that
   escapes its defining body, lowered through `NIR` as a closure tuple with a
   symbol-addressed closure entrypoint plus captured values, then through
   `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`.
17. Add the next simplest corpus program in order:
   [x] `0106_prelude_option_match.ml`.
   The active native corpus suite now snapshots prelude `option`
   constructors plus one exhaustive `match`, lowered through the shared tagged
   tuple encoding and tag checks via `NIR`, `MIR`, `LIR`, native emission, and
   linker planning on `aarch64-apple-darwin`.
18. Add the next simplest corpus program in order:
   [x] `0107_open_std_hello_world.ml`.
   The active native corpus suite now snapshots an explicit top-level
   `open Std` that disappears before `Core_ir`, then reaches `NIR`, `MIR`,
   `LIR`, native emission, and linker planning on `aarch64-apple-darwin`
   without introducing any open-specific native pass or runtime helper.
19. Add the next simplest corpus program in order:
   [x] `0108_less_than_comparison.ml`.
   The active native corpus suite now snapshots a source-level direct `<`
   call through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`, with the AArch64 emitter mangling the punctuation-
   bearing callee symbol before assembly emission.
20. Add the next simplest corpus program in order:
   [x] `0109_greater_than_comparison.ml`.
   The active native corpus suite now snapshots a source-level direct `>`
   call through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`, with the AArch64 emitter mangling the punctuation-
   bearing callee symbol before assembly emission.
21. Add the next simplest corpus program in order:
   [x] `0110_less_or_equal_comparison.ml`.
   The active native corpus suite now snapshots a source-level direct `<=`
   call through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`, with the AArch64 emitter mangling the punctuation-
   bearing callee symbol before assembly emission.
22. Add the next simplest corpus program in order:
   [x] `0111_greater_or_equal_comparison.ml`.
   The active native corpus suite now snapshots a source-level direct `>=`
   call through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`, with the AArch64 emitter mangling the punctuation-
   bearing callee symbol before assembly emission.
23. Add the next simplest corpus program in order:
   [x] `0112_effect_position_local_let.ml`.
   The active native corpus suite now snapshots an eval-position local `let`
   through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or pass;
   the existing local-`let` lowering already handled effect position.
24. Add the next simplest corpus program in order:
   [x] `0113_initializer_shadowing.ml`.
   The active native corpus suite now snapshots a declaration initializer that
   shadows its own exported binding name through `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; the existing local-`let` lowering
   already preserved the outer exported binding and only needed approved
   native snapshot coverage.
25. Add the next simplest corpus program in order:
   [x] `0114_top_level_mutual_recursion.ml`.
   The active native corpus suite now snapshots a top-level function-only
   `let rec ... and ...` group through `NIR`, `MIR`, `LIR`, native emission,
   and linker planning on `aarch64-apple-darwin`. This slice required no new
   native IR form or pass; the existing recursive-group lowering already kept
   the mutually recursive functions as one shared recursive init group and the
   later native stages handled the resulting direct cross-calls unchanged.
26. Add the next shared corpus program already supported above `NIR`:
   [x] `0013_tail_recursive_factorial.ml`.
   The active native corpus suite now snapshots a function-local tail-
   recursive `let rec` helper through `NIR`, `MIR`, `LIR`, native emission,
   and linker planning on `aarch64-apple-darwin`. This slice required no new
   native pass; `Native.Nir.Lowering` now accepts function-only recursive
   local `let` groups and lowers them through the existing lifted
   local-function path.
27. Add the next shared corpus program already supported above `NIR`:
   [x] `0014_mutual_recursion_even_odd.ml`.
   The active native corpus suite now snapshots mutually recursive top-level
   functions plus source-level `<>` through `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`.
28. Add the next shared corpus program already supported above `NIR`:
   [x] `0115_external_print_endline.ml`.
   The active native corpus suite now snapshots a source-level external
   declaration call through `NIR`, `MIR`, `LIR`, native emission, and linker
   planning on `aarch64-apple-darwin`. This slice required no new native IR
   form or pass; the existing direct-call path already preserved the external
   symbol call through the native pipeline.
29. Add the next shared corpus program already supported above `NIR`:
   [x] `0116_prelude_result_match.ml`.
   The active native corpus suite now snapshots stdlib `result`
   constructors plus one exhaustive `match` through `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; the existing shared tagged-tuple
   lowering for ordinary variants and prelude `option` already handled the
   `result` constructors and match tag checks unchanged.
30. Add the next shared corpus program already supported above `NIR`:
   [x] `0012_list_recursion_sum.ml`.
   The active native corpus suite now snapshots stdlib `list` constructors
   plus one exhaustive recursive `match` through `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; the existing shared tagged-tuple
   lowering for prelude list constructors, including packing the two `::`
   arguments into one shared tuple payload, already reached the current native
   stages unchanged.
31. Add the next shared corpus program already supported above `NIR`:
   [x] `0117_dead_local_bindings.ml`.
   The active native corpus suite now snapshots a dead local value binding
   plus one unused non-escaping lifted local helper through `NIR`, `MIR`,
   `LIR`, native emission, and linker planning on `aarch64-apple-darwin`.
   This slice required no new native IR form or pass; the existing local-`let`
   lowering and later native stages already preserved the dead helper honestly
   in snapshots.
32. Add the next shared corpus program already supported above `NIR`:
   [x] `0022_local_functions_and_closures.ml`.
   The active native corpus suite now snapshots a top-level binding that
   stores one escaped local function value returned from its defining
   function, then later calls that closure through `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; the existing closure-tuple
   lowering already handled the same escaped-helper shape used by
   `0105_indirect_call_via_returned_closure.ml`.
33. Add the next shared corpus program that forces one honest native currying
   slice:
   [x] `0023_partial_application.ml`.
   The active native corpus suite now snapshots an under-applied top-level
   direct call that later flows through two indirect calls via `NIR`, `MIR`,
   `LIR`, native emission, and linker planning on `aarch64-apple-darwin`.
   This slice required one new `NIR` lowering rule: under-applied direct calls
   to known native functions now materialize closure tuples plus generated
   wrapper functions so later indirect calls remain source-correct instead of
   snapshotting a dishonest raw direct call result as if it were already a
   closure.
34. Introduce one new optimization or legality pass only when those examples
   force it.
35. Add the next shared corpus program already supported above `NIR`:
   [x] `0118_printf_and_print_endline.ml`.
   The active native corpus suite now snapshots one top-level
   `Printf.printf` eval item followed by one top-level `print_endline` eval
   item through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or pass;
   the existing direct-call lowering already preserved both ambient calls as
   direct native symbols through the current native pipeline.
36. Add the next shared corpus program already supported above `NIR`:
   [x] `0049_function_composition_pipeline.ml`.
   The active native corpus suite now snapshots one higher-order composition
   binding built from three anonymous `fun` expressions plus one under-applied
   direct call through `NIR`, `MIR`, `LIR`, native emission, and linker
   planning on `aarch64-apple-darwin`. This slice required one new `NIR`
   lowering rule: expression-position anonymous lambdas now materialize as
   closure tuples plus lifted wrapper entrypoints that reuse the existing
   native closure ABI instead of blocking the program above `MIR`.
37. Add the next shared corpus program already supported above `NIR`:
   [x] `0119_string_concat.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `^` direct call through `NIR`, `MIR`, `LIR`, native emission, and linker
   planning on `aarch64-apple-darwin`. This slice required no new native IR
   form or pass; the existing direct-call lowering and AArch64 emitter
   already preserved the punctuation-bearing callee symbol honestly through
   the current native pipeline.
38. Add the next shared corpus program already supported above `NIR`:
   [x] `0057_phantom_length_vector.ml`.
   The active native corpus suite now snapshots one phantom-indexed vector
   recursive sum through `NIR`, `MIR`, `LIR`, native emission, and linker
   planning on `aarch64-apple-darwin`. This slice required no new native IR
   form or pass; the existing shared tagged-tuple lowering for `VNil` /
   `VCons` plus the current recursive top-level function path already reached
   the native pipeline unchanged.
39. Add the next shared corpus program already supported above `NIR`:
   [x] `0120_string_of_int.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `string_of_int` direct call nested inside `print_endline` through `NIR`,
   `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or pass;
   the existing direct-call lowering and AArch64 Darwin emitter already
   preserved the alphabetic callee symbol honestly through the current native
   pipeline.
40. Add the next shared corpus program already supported above `NIR`:
   [x] `0121_print_newline.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `print_newline` direct call through `NIR`, `MIR`, `LIR`, native emission,
   and linker planning on `aarch64-apple-darwin`. This slice required no new
   native IR form or pass; the existing direct-call lowering and AArch64
   Darwin emitter already preserved the alphabetic callee symbol honestly
   through the current native pipeline.
41. Add the next shared corpus program already supported above `NIR`:
   [x] `0122_int_of_string.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `int_of_string` direct call nested inside `Printf.printf` through `NIR`,
   `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or pass;
   the existing direct-call lowering and AArch64 Darwin emitter already
   preserved the alphabetic callee symbol honestly through the current native
   pipeline.
42. Add the next shared corpus program already supported above `NIR`:
   [x] `0124_print_int.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `print_int` direct call through `NIR`, `MIR`, `LIR`, native emission, and
   linker planning on `aarch64-apple-darwin`. This slice required no new
   native IR form or pass; the existing direct-call lowering and AArch64
   Darwin emitter already preserved the alphabetic callee symbol honestly
   through the current native pipeline.
43. Add the next shared corpus program already supported above `NIR`:
   [x] `0125_print_string.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `print_string` direct call through `NIR`, `MIR`, `LIR`, native emission,
   and linker planning on `aarch64-apple-darwin`. This slice required no new
   native IR form or pass; the existing direct-call lowering and AArch64
   Darwin emitter already preserved the alphabetic callee symbol honestly
   through the current native pipeline.
44. Add the next shared corpus program already supported above `NIR`:
   [x] `0126_string_of_float.ml`.
   The active native corpus suite now snapshots one top-level source-level
   finite-input `string_of_float` direct call nested inside `print_endline`
   through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or pass;
   the existing direct-call lowering and AArch64 Darwin emitter already
   preserved the alphabetic callee symbol honestly through the current native
   pipeline.
45. Add the next shared corpus program already supported above `NIR`:
   [x] `0127_float_of_string.ml`.
   The active native corpus suite now snapshots one top-level source-level
   finite-input `float_of_string` direct call nested inside
   `string_of_float` and `print_endline` through `NIR`, `MIR`, `LIR`,
   native emission, and linker planning on `aarch64-apple-darwin`. This
   slice required no new native IR form or pass; the existing direct-call
   lowering and AArch64 Darwin emitter already preserved the alphabetic
   callee symbol honestly through the current native pipeline.
46. Add the next shared corpus program already supported above `NIR`:
   [x] `0025_custom_infix_operators.ml`.
   The active native corpus suite now snapshots one top-level source-level
   custom infix-operator direct call through `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; `Raml.Example_pipeline` now seeds
   the minimal polymorphic ambient surface for `@` and `List.iter`, and the
   existing direct-call lowering plus AArch64 Darwin emitter preserve the
   punctuation-bearing custom callee symbols honestly through the current
   native pipeline.
47. Add the next shared corpus program already supported above `NIR`:
   [x] `0128_print_char.ml`.
   The active native corpus suite now snapshots one top-level source-level
   `print_char` direct call with a shared `Core_ir.Constant.Char` lowered as a
   one-character string literal through `NIR`, `MIR`, `LIR`, native
   emission, and linker planning on `aarch64-apple-darwin`. This slice
   required no new native IR form or pass; the existing direct-call lowering
   and AArch64 Darwin emitter already preserved the alphabetic callee symbol
   honestly through the current native pipeline while reusing the current
   string-literal materialization path for the shared char payload.
48. Add the next shared corpus program already supported above `NIR`:
   [x] `0026_sequence_and_ignore.ml`.
   The active native corpus suite now snapshots one top-level ignored call
   through the shared `Core_ir.Sequence` plus unit lowering path, then
   through `NIR`, `MIR`, `LIR`, native emission, and linker planning on
   `aarch64-apple-darwin`. This slice required no new native IR form or
   pass; the existing shared `ignore` lowering and sequence-preserving native
   path already handled the program honestly through the current pipeline.

## One-Slice Rule

One change should usually do one of these:

- move one corpus program one stage further
- make one native pass real
- add one pass-local snapshot family
- make one target surface real
- remove one dishonest shortcut

If a change needs more than that, split it.
