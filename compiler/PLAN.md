# Riot Stage0 Vertical Roadmap

This is the implementation ledger for taking `compiler/stage0` and
`compiler/rt` from an advanced bootstrap proof-of-concept to a compiler/runtime
that can support real Riot programs and, eventually, the self-hosted compiler.

The core working style is vertical: every useful language feature should travel
through the whole compiler shape in one small, reviewable commit:

source syntax -> AST -> typed HIR/.rsig -> lambda IR -> Actor IR where
relevant -> LLVM/codegen -> runtime ABI where relevant -> fixtures/snapshots.

Commit every value-adding slice with a conventional commit. If a slice exposes
a better name while implementing it, keep the message conventional and keep the
scope narrow.

## Stage0 Reference-Compiler Hardening Target

Before self-hosting, stage0 should become a reliable reference compiler for Riot
ML. The near-term goal is not to bootstrap `riotc`; it is to make stage0 correct,
fast enough to iterate on, and pleasant to extend. Each slice should improve a
clear compiler boundary and prove the improvement with visible fixtures,
snapshots, examples, or runtime tests.

Hardening priorities:

1. Parser quality: clearer grammar boundaries, better recovery diagnostics, and
   examples for every accepted surface form.
2. Type checker quality: fewer `Unknown` fallbacks, better source spans, richer
   inference constraints, and explicit diagnostics for unsupported cases.
3. Lowering quality: stable typed HIR/lambda/AIR snapshots, fewer ad hoc codegen
   decisions, and small simplification passes with tests.
4. Runtime/codegen reliability: root/value lifetime tests, actor scheduling
   semantics, and deterministic behavior under GC pressure.
5. Performance hygiene: keep hot lookup tables named and deterministic, avoid
   repeated stringly resolution in later passes, and add focused benchmarks only
   after behavior is covered.

Useful fixtures should look like small real programs: compiler-shaped token
classification, recursive list/tree walks, closure-heavy helper functions,
variant/record transformations, multi-module interface checks, actor protocols,
and diagnostics that demonstrate the exact failure mode.

Resolved hardening gap: pre-inference validation used to check match
exhaustiveness before lambda parameter types had been inferred. Stage0 now reruns
validation with inferred expression types and refreshes match scrutinee type
facts after pattern constraints.

Resolved hardening gap: runtime variant values constructed through imported
constructors used to disagree with provider-local matches because codegen used
qualified consumer type names for one side and local provider type names for the
other. LLVM lowering now canonicalizes runtime variant tags to the base type name
while keeping `.rsig` and type-checker names qualified.

Resolved hardening gap: imported record patterns now follow the same split
identity model. Checker validation and typed-HIR lowering qualify provider-local
record field types through the imported module (`token` becomes `Syntax.token`),
while LLVM record values/tests canonicalize runtime record tags to the base
record name so provider-local records match consumer-side `Module.record`
patterns.

Resolved hardening gap: transitive `.rsig` type references now keep their original
module qualifier when another module imports and re-exports/calls through them.
For example, an `Analyze` interface that accepts `Syntax.token` is imported by
consumers as `Syntax.token`, not incorrectly rewritten to `Analyze.Syntax.token`.

Resolved hardening gap: transitive `.rsig` dependency object resolution now
checks dependency fingerprints even when a module was already seen as a direct
import. Direct imports and transitive dependency edges must agree on the same
signature fingerprint instead of letting traversal order hide stale interfaces.

Resolved hardening gap: list spread syntax (`..tail`) used to be pattern-only.
Stage0 now supports list cons expressions such as `[head, ..tail]`, lowered
through the ordinary `list_cons` std/runtime ABI.

Resolved hardening gap: annotated mutually recursive top-level functions are now
predeclared before inference. Unannotated forward recursion still receives a
source-backed diagnostic so the unsupported inference case is explicit.

Known hardening gaps discovered by compiler-shaped fixtures:

- Mutually recursive helpers now have an inferred group path when the cycle is
  fully unannotated or partially annotated and annotations/body facts provide
  concrete constraints. Underconstrained no-facts cycles remain source-backed
  diagnostic boundaries.
Resolved hardening gap: LLVM lambda-apply helper symbols are now module-scoped.
Imported providers and consumers can both lower lambda applications without
emitting duplicate `_riot_lambda_apply_0` symbols at link time.

Resolved hardening gap: imported and local generic constructor inference now
preserves result type arguments instead of erasing applications to their base
variant names. Nested pattern flows such as `Options.Some(Boxes.box { value })`
can infer `value:i64` without a scrutinee annotation.

Resolved hardening gap: tuple patterns now test nested item patterns during LLVM
lowering instead of only checking tuple arity, so tuple scrutinees containing
variant payload patterns select the correct arm.

Actor metadata note: imported generic record and variant containers can preserve
`actor_id<'msg>` payload facts through `.rsig` boundaries, including concrete
payload diagnostics. A single `actor_id<_>` parameter or generic variant-pattern
binder still behaves like a normal type variable within one scope/arm, so sending
two incompatible message shapes through the same binder currently needs either
separate projections/matches or a future existential-unknown actor-id treatment.
A local `let worker: actor_id<_> = spawn { ... }` binding to an already
heterogeneous actor remains a concrete runtime actor id with unknown message
shape and can still send multiple shapes; the scoped conflict applies when one
unknown actor-id binder is refined by sends in that scope.

`compiler/riotc` may remain as an eventual consumer, but it should not drive the
loop until stage0 is sturdy enough to serve as the reference implementation.

## Current Baseline

Stage0 currently has:

- A handwritten lexer/parser for a small Riot ML surface.
- `use Module` resolution through binary `.rsig` files.
- A typed HIR boundary, lambda IR boundary, Actor IR boundary, LLVM text/object
  emission, and native linking.
- A runtime value ABI (`RtValue`) for scalars, actor ids, strings, tuples,
  lists, and records.
- Native actor state-machine lowering with heap-owned actor frames.
- A Rust-shaped runtime split into ABI, actor, actor-id, frame, I/O, scheduler,
  test, and value modules.
- Opaque `ActorId` handles that route sends directly to actor slots/mailboxes,
  with unsafe raw reconstruction kept at the runtime boundary.
- Thread-local scheduler state, FIFO mailboxes, monitor/link stubs, and a
  mark/sweep GC scaffold. Stage0 still executes actors on one scheduler worker;
  true cross-core work stealing/migration is not implemented yet.
- Snapshot-driven fixture coverage for parser/typed/lambda IR/Actor IR/LLVM/object
  and runtime output.

The most important missing capability is no longer one isolated feature. It is
the ability to add a source feature and prove it end-to-end through frontend,
interfaces, backend, runtime, and tests while preserving runtime value lifetime
and actor-frame safety.

## Reevaluation Checkpoint: ActorId Runtime Split

This checkpoint incorporates the first runtime hardening pass after the initial
roadmap was written.

Completed since the initial roadmap:

- `fix(rt): fail closed on runtime value access`
  - Runtime value access now traps loudly on invalid type/stale-value paths
    instead of returning null pointers, zero lengths, or default values.
  - Stage0 rejects the corresponding invalid list/string operations earlier
    where it has enough type information.
- `refactor(rt): split runtime around actor ids`
  - `compiler/rt/src/lib.rs` is now a module root, not the whole runtime.
  - `ActorId::from_raw` is unsafe, making the raw-handle trust boundary
    explicit.
  - Runtime and compiler internals use `ActorId`/`actor_id` terminology; the
    legacy actor-handle terminology is intentionally gone from `compiler/`.
  - Actor sends resolve through the opaque actor handle to the actor mailbox
    instead of using a global lookup table.
  - The C ABI layer is concentrated in `rt/src/abi.rs`; Rust runtime internals
    remain ordinary Rust modules.

Current architectural read:

- The next feature work should stay vertical. GC/rooting is no longer the
  immediate blocker for small examples: generated code now roots live runtime
  values across runtime calls, actor frames expose boxed-value root slots, and
  allocation-pressure GC is active enough to catch lifetime mistakes.
- The runtime is multicore-ready in shape, not multicore-complete. Mailboxes are
  already independently synchronized, scheduler state is thread-local, and
  stale runtime-created `ActorId`s are deterministic tombstones. A stable
  `ActorId` should be enough to push directly into an actor mailbox from any
  runtime thread; cross-scheduler work should add wakeup/ready notification, not
  reroute message ownership through scheduler queues.
- `ActorId` as an opaque `u64` capability is fine for generated code. The Rust
  runtime should keep the internals idiomatic and make the extern layer do the
  raw pointer/length/handle translation.

Near-term order after this checkpoint:

1. Build the core inferred typed lambda language: function types, lexical
   rebinding, lambdas, application, and eventually closures.
2. Compile that lambda core vertically through RIR, LLVM, and a runtime closure
   ABI.
3. Move into records, variants, match, and richer actor messages once
   higher-order helper code is usable.
4. Return to multicore scheduling once the direct mailbox-send invariant has a
   wakeup/ready-queue design beside it.

## Lambda Core Checkpoint

The fastest path to a small usable language is an inference-first typed lambda
core. Stage0 should not require annotations for ordinary lambdas or helper
functions. Type annotations remain useful as constraints and interface
documentation, but inference should do the default work.

## Pipeline Ownership Checkpoint

Stage0 now keeps one lambda middle IR rather than separate LIR/RIR models. The
source pass names can still say `emit ir` for user-facing continuity, but the
Rust ownership is:

- `checker` owns typed HIR construction and `.rsig` projection.
- `lambda` owns the single lambda IR model, typed-tree lowering, simplification,
  and closure conversion.
- `actor` owns AIR, actor discovery, frame layout, and actor slot typing.
- `backend::llvm` consumes lambda IR plus AIR and emits LLVM/object artifacts.

This means future cleanup should avoid creating another intermediate lambda IR
unless it has a distinct invariant. If a pass is just simplifying typed HIR into
callable, closure-aware lambda code, it belongs in `lambda`.

Agreed source syntax:

```riot
let add = fn(n, x) { x + n };
let make_adder = fn(n) { fn(x) { x + n } };
```

The old candidate syntax `fn (x: T) -> U { ... }` is not the source shape for
lambdas. Top-level functions still use `fn name(...) { ... }`.

Near-term lambda-core slices:

1. `docs(stage0): define inferred lambda-core subset`
   - Document optional annotations, lexical rebinding, function values,
     application, captures, closure runtime needs, and the no-polymorphism-yet
     boundary.
2. `feat(stage0): introduce type variables and substitutions`
   - Add type variables, substitutions, occurs check, and unification as a
     tested internal module.
3. `feat(stage0): infer expression types with unification`
   - Move literals, paths, arithmetic, bool ops, tuples/lists/records, `if`,
     and field/projection typing onto unification.
4. `feat(stage0): infer lexical let rebinding`
   - Every `let` creates a new lexical binding, even in the same block. Later
     references resolve to the nearest earlier binding.
5. `feat(stage0): infer function signatures`
   - Infer unannotated params/results from bodies and call sites where
     constraints are concrete, and emit concrete `.rsig` where possible.
6. `feat(stage0): parse lambda expressions`
   - Parse `fn(...) { ... }` in expression position. Lambda params may have
     optional annotations but do not require them.
7. `feat(stage0): infer lambda and apply types`
   - Infer lambda parameter/result types and ordinary application of callable
     expressions. Reject non-callable application in type checking.
8. `feat(stage0): parse function type annotations`
   - Support function types in annotations and `.rsig`, using annotations as
     constraints on inference.
9. `feat(stage0): lower typed callable values to rir`
   - Add typed `Lambda`, `Apply`, `FunctionRef`, and capture metadata while
     preserving direct calls when the callee is statically known.
10. `feat(rt): add closure value abi`
    - Represent closures as runtime values containing a code pointer and
      captured environment. GC must trace closure captures.

Acceptance target:

```riot
fn main() {
  let make_adder = fn(n) {
    fn(x) { x + n }
  };
  let add2 = make_adder(2);
  dbg(add2(40))
}
```

This should print `42` without annotations.

## Validation Loop

Use the focused command while implementing a slice, then the full loop before
committing unless the slice is documentation-only.

```sh
env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm cargo check --manifest-path compiler/stage0/Cargo.toml
cargo test --manifest-path compiler/rt/Cargo.toml
env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm cargo test --manifest-path compiler/stage0/Cargo.toml
```

Snapshot updates are intentional only:

```sh
env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm INSTA_UPDATE=always cargo test --manifest-path compiler/stage0/Cargo.toml --test fixtures
```

## Roadmap Rules

- Keep `.rsig` binary. Add human-readable output only as an inspection aid.
- Keep source-level runtime facilities as declarations or prelude declarations,
  not compiler primitive enum variants.
- Keep `spawn` and `receive` as source/compiler constructs that lower through
  RIR and Actor IR.
- Keep `use`, not `import`.
- Use `actor_id<'msg>` and `ActorId` for actor handles. Do not reintroduce the
  old actor-handle name in compiler/runtime internals or fixtures.
- Preserve the file-to-module rule: `hello.ml -> Hello`,
  `hello_world.ml -> HelloWorld`, and mixed-case source file stems are invalid.
- Every implementation commit should add a positive fixture, a diagnostic
  fixture, or a runtime unit test.
- Do not broaden a slice because neighboring code looks tempting. Add the next
  slice as the next commit.

## Entry Template

Each entry below names the intended commit and the exact vertical acceptance
shape.

- **Commit:** conventional commit subject.
- **Intent:** why this matters for self-hosting or actor-backed programs.
- **Frontend:** lexer/parser/AST/checker/typed HIR work.
- **Lowering/backend/runtime:** RIR/Actor IR/codegen/runtime ABI work.
- **Fixtures/tests:** expected test additions.
- **Validation:** focused acceptance criteria and commands for this slice.
- **Done when:** acceptance criteria.

## Wave 1: Runtime Value + GC Foundation

### 1. Allow Sequenced Main Actions

- **Commit:** `feat(stage0): allow sequenced main actions`
- **Intent:** Real programs need to perform more than one effect. The current
  one-output-expression main rule forces fixture-shaped programs instead of
  ordinary program-shaped code.
- **Frontend:** Relax `main` validation so a main block may contain any number
  of statements and a tail expression. Keep the requirement that executable
  `main` must either produce output, perform actor actions, or eventually return
  `unit`.
- **Lowering/backend/runtime:** Preserve existing block statement order in RIR
  and LLVM. No runtime ABI change should be required.
- **Fixtures/tests:** Add a basic fixture with several `dbg`/`println` calls and
  a final unit tail. Update diagnostics that mention the old one-output
  restriction.
- **Done when:** The fixture binary prints all lines in source order, existing
  actor fixtures still run, and the old diagnostic wording is gone.
- **Validation:** Add `programs/basic/sequenced_main.ml` with matching
  `.stdout`; run the generated fixture test and the full stage0 suite without
  `INSTA_UPDATE`.

<!-- autoresearch:step-1:done -->

### 2. Add Structural RtValue Equality

- **Commit:** `feat(rt): add structural RtValue equality`
- **Intent:** Pattern matching, maps, sets, compiler data structures, and test
  assertions all need equality that is not based on rendered strings.
- **Frontend:** Keep `==` syntax unchanged. In typed HIR, allow equality between
  matching scalar types and matching boxed value types. Reject obviously mixed
  types such as `string == i64` unless both sides are unknown.
- **Lowering/backend/runtime:** Add `riot_rt_value_eq(lhs, rhs) -> bool`.
  Implement structural equality for `unit`, bools, i64s, actor ids, strings, tuples,
  lists, records with same path/field order, and future-proof unsupported heap
  tags as false. Lower boxed equality through this ABI; keep scalar LLVM equality
  for unboxed scalars.
- **Fixtures/tests:** Add fixtures for string equality, tuple equality, list
  equality, record equality, and mixed-type diagnostics. Add direct runtime unit
  tests for nested structures.
- **Done when:** No equality path uses `to_print_string` or rendered output as
  semantic equality.
- **Validation:** Runtime unit tests cover nested equal/not-equal values; stage0
  fixtures prove boxed equality emits calls to `riot_rt_value_eq` in LLVM and
  produces expected stdout.

<!-- autoresearch:step-2:done -->

### 3. Add RtValue Ordering for I64 and Strings

- **Commit:** `feat(rt): add RtValue ordering for i64 and strings`
- **Intent:** Compiler code needs comparisons for indexes, lengths, names, and
  simple ordering decisions.
- **Frontend:** Keep `<` syntax. Permit `<` for i64 and string operands. Reject
  boxed tuples/lists/records with a source-backed diagnostic.
- **Lowering/backend/runtime:** Add `riot_rt_value_lt(lhs, rhs) -> bool` for
  boxed i64 and string values. Lower boxed `<` through the runtime; keep direct
  LLVM comparison for unboxed i64.
- **Fixtures/tests:** Add positive string/i64 comparison fixtures and negative
  tuple/list/record comparison diagnostics.
- **Done when:** Boxed string comparison works at runtime and unsupported boxed
  ordering fails in checking, not during codegen.
- **Validation:** Positive fixtures for i64/string `<` pass; diagnostics for
  tuple/list/record ordering fail during checking; LLVM snapshots show boxed
  comparisons call `riot_rt_value_lt`.

<!-- autoresearch:step-3:done -->

### 4. Lower Record Field Access for Runtime Values

- **Commit:** `feat(stage0): lower record field access for runtime values`
- **Intent:** Records are essential for compiler ASTs and runtime messages. They
  must be usable after construction, not just printable.
- **Frontend:** Parse postfix field access as a real expression node rather than
  overloading dotted paths. Resolve `Module.value` separately from
  `record.field`.
- **Lowering/backend/runtime:** Add RIR field access and lower runtime record
  access through `riot_rt_value_record_get(record, name_ptr, name_len) ->
  RtValue`. Add a checked failure path for missing runtime fields.
- **Fixtures/tests:** Add fixtures for `Point { x: 1, y: 2 }.x`, binding a
  record then reading fields, and a diagnostic for unknown declared fields once
  declared records exist.
- **Done when:** Runtime record projection works without static evaluation.
- **Validation:** A fixture binds a record returned from a function and prints a
  projected field; `emit ir` contains field access rather than a precomputed
  value.

<!-- autoresearch:step-4:done -->

### 5. Add Tuple Projection Support

- **Commit:** `feat(stage0): add tuple projection support`
- **Intent:** The compiler will use tuples for small intermediate structures
  long before full ADTs are ergonomic.
- **Frontend:** Choose explicit syntax for stage0, for example `tuple.0`, and
  parse it as tuple projection only when the suffix is numeric.
- **Lowering/backend/runtime:** Add `riot_rt_value_tuple_get(tuple, index) ->
  RtValue` and optionally `riot_rt_value_tuple_len(tuple) -> usize`. Lower
  projection through runtime for boxed tuples.
- **Fixtures/tests:** Add tuple projection fixtures for literals, local
  bindings, function returns, and nested tuples. Add an out-of-bounds diagnostic
  when the tuple length is statically known.
- **Done when:** A function can return a tuple and callers can project fields
  from the returned runtime value.
- **Validation:** Positive fixtures project tuple fields from literals and
  function returns; out-of-bounds projection diagnostics point at the projection
  suffix.

<!-- autoresearch:step-5:done -->

### 6. Add List Length and Nth Operations

- **Commit:** `feat(rt): add list length and nth operations`
- **Intent:** Compiler passes need basic sequence operations before a richer
  standard library exists.
- **Frontend:** Expose operations as source-level declarations or injected
  prelude externals, for example `list_len(xs)` and `list_get(xs, i)`, not as
  bespoke compiler primitives.
- **Lowering/backend/runtime:** Add runtime ABI for list length and index. Return
  an `RtValue` from `list_get`, and decide out-of-bounds behavior as a runtime
  trap/diagnostic for now.
- **Fixtures/tests:** Add fixtures for empty length, non-empty length, indexing
  scalar elements, and indexing boxed elements.
- **Done when:** Generated code can index a list built at runtime and print the
  selected value.
- **Validation:** Runtime unit tests cover empty/non-empty list length and nth;
  stage0 fixtures print `list_len([1, 2])` and `list_get([10, 20], 1)`.

<!-- autoresearch:step-6:done -->

### 7. Add String Length and Concat Operations

- **Commit:** `feat(rt): add string length and concat operations`
- **Intent:** Self-hosted compiler diagnostics and interface rendering need
  basic string manipulation.
- **Frontend:** Expose `string_len(s)` and `string_concat(a, b)` via prelude
  declarations or fixture-level extern declarations. Type them as
  `string -> i64` and `string -> string -> string`.
- **Lowering/backend/runtime:** Add runtime ABI for string length and concat.
  Concat allocates a new runtime string and returns `RtValue`.
- **Fixtures/tests:** Add literal concat, variable concat, nested concat, and
  string length fixtures.
- **Done when:** A runtime-computed string can be concatenated and printed
  without going through static evaluation.
- **Validation:** A fixture concatenates function-returned strings and prints
  the result; LLVM text calls the runtime string concat ABI instead of embedding
  the final string.

<!-- autoresearch:step-7:done -->

### 8. Root Live RtValues Across Runtime Calls

- **Commit:** `feat(stage0): root live RtValues across runtime calls`
- **Intent:** GC cannot become reliable until generated code protects live heap
  values across allocations and runtime calls.
- **Frontend:** No syntax change.
- **Lowering/backend/runtime:** Teach codegen to push runtime roots for live
  `RtValue` temporaries around calls that can allocate or collect. Keep the
  initial implementation conservative; extra root pushes are acceptable.
- **Fixtures/tests:** Add a fixture that builds nested values across many
  allocations and prints an early value after later allocations. Add runtime GC
  unit coverage for root push/pop around nested children.
- **Done when:** Enabling allocation-pressure GC does not invalidate live
  generated-code values.
- **Validation:** Add a GC stress fixture that allocates after saving an early
  value and prints the early value correctly; runtime tests assert roots protect
  nested children.

<!-- autoresearch:step-8:done -->

### 9. Store RtValues in Actor Frames

- **Commit:** `feat(stage0): store RtValues in actor frames`
- **Intent:** Actor-based compiler services will need to preserve boxed state
  across `receive` suspension.
- **Frontend:** Permit boxed values in actor local bindings and captures where
  validation currently permits only scalar slot types.
- **Lowering/backend/runtime:** Add `Value` to Actor IR slot types, frame layout,
  load/store logic, and root scanning metadata. Ensure actor frame `RtValue`
  slots are treated as GC roots while the actor is alive.
- **Fixtures/tests:** Add an actor fixture that captures a tuple/list/record,
  receives a message, and prints the captured value after suspension.
- **Done when:** Actor frames can safely hold boxed values across receive without
  freeing them during GC.
- **Validation:** Actor fixture captures a boxed tuple/list/record, receives a
  message, triggers allocation/GC, then prints the captured value after resume.

<!-- autoresearch:step-9:done -->

### 10. Trigger GC From Heap Allocation Pressure

- **Commit:** `feat(rt): trigger GC from heap allocation pressure`
- **Intent:** The GC scaffold should begin behaving like a runtime service
  instead of only a test hook.
- **Frontend:** No syntax change.
- **Lowering/backend/runtime:** Add an allocation counter/threshold. Before
  growing the heap beyond the threshold, collect roots from explicit root stack,
  mailboxes, and actor frames. Keep the threshold deterministic for tests.
- **Fixtures/tests:** Add runtime unit tests for threshold-triggered collection
  and a stage0 fixture that allocates enough values to trigger collection.
- **Done when:** Runtime collection occurs automatically under allocation
  pressure without breaking existing fixtures.
- **Validation:** Runtime unit tests force threshold collection and assert freed
  counts; full stage0 fixtures pass with a low deterministic test threshold.

<!-- autoresearch:step-10:done -->

## Runtime Hardening Interlude

These slices are prerequisites for real multicore scheduling. They should land
after the first GC/rooting pass and before actor migration, work stealing, or
cross-core scheduler queues.

### 10A. Define the ActorId Lifetime Contract

- **Commit:** `fix(rt): define actor id lifetime safety`
- **Intent:** `ActorId` is intentionally an opaque capability, but the runtime
  must make clear which raw handles are safe to dereference and when actor-slot
  memory may be reclaimed.
- **Frontend:** Keep `actor_id<'msg>` source types unchanged.
- **Lowering/backend/runtime:** Document and enforce the initial contract:
  `ActorId`s are created only by the runtime, `ActorId::from_raw` remains unsafe,
  actor slots are not deallocated while generated code can still hold a handle,
  and sends to terminated actors become no-ops. If slot reclamation is added,
  add generation checks or an indirection header before freeing slots.
- **Fixtures/tests:** Add direct runtime tests for sending to terminated actors,
  monitor/link after termination, and shutdown cleanup. Avoid tests that forge
  arbitrary aligned handles; that is intentionally outside the safe contract.
- **Done when:** Runtime behavior for stale-but-runtime-created actor handles is
  deterministic and documented.
- **Validation:** Runtime tests prove sends/monitor/link against a terminated
  runtime-created actor handle do not crash, resurrect the actor, or enqueue
  messages.

<!-- autoresearch:step-10A:done -->

### 10B. Preserve Direct ActorId Mailbox Sends

- **Commit:** `test(rt): assert actor ids send directly to mailboxes`
- **Intent:** Sending by `ActorId` should remain cheap when actors eventually
  live on different scheduler workers. A stable actor id should point to the
  actor slot/mailbox strongly enough that send can push directly from any
  runtime thread.
- **Frontend:** No syntax change.
- **Lowering/backend/runtime:** Keep the direct `ActorId -> ActorSlot -> mailbox`
  delivery path. Future multicore scheduling should add wakeup/ready
  notification beside mailbox push, not a scheduler-owned message forwarding
  queue.
- **Fixtures/tests:** Add runtime unit tests that exercise local-send and
  simulated foreign-owned actor send paths without starting real worker threads.
- **Done when:** The mailbox API does not require sender and receiver to live on
  the same scheduler local.
- **Validation:** Runtime tests prove a foreign-owned actor slot still receives
  a message through direct `ActorId` mailbox push; existing actor fixtures still
  pass without changing generated code.

<!-- autoresearch:step-10B:done -->

### 10C. Separate Scheduler Ownership From Runtime ABI

- **Commit:** `refactor(rt): separate scheduler ownership from abi`
- **Intent:** The extern layer should not know about scheduler-local internals
  once multiple scheduler workers exist.
- **Frontend:** No syntax change.
- **Lowering/backend/runtime:** Introduce a small Rust runtime facade that owns
  scheduler selection, actor spawning, sending, monitor/link operations, and GC
  entrypoints. Keep `abi.rs` as pointer validation plus facade calls.
- **Fixtures/tests:** Add unit tests against the Rust facade where possible, and
  keep ABI tests focused on raw pointer/length behavior.
- **Done when:** `abi.rs` has no direct heap/mailbox/scheduler field access.
- **Validation:** Runtime tests cover facade behavior; `abi.rs` remains a thin
  translation layer and full stage0 fixtures pass.

## Wave 2: Blocks, Locals, and Name Resolution

### 11. Make Block Expressions First-Class

- **Commit:** `feat(stage0): make block expressions first-class`
- **Intent:** Compiler code needs scoped helper bindings inside expressions,
  especially in branches and future match arms.
- **Frontend:** Parse `{ ... }` as an expression in expression positions. Type a
  block by its tail expression or `unit` when no tail exists.
- **Lowering/backend/runtime:** Add HIR/RIR block expression nodes or lower them
  into scoped statement sequences. Codegen must preserve evaluation order and
  local scope.
- **Fixtures/tests:** Add nested block fixtures in `let`, `if`, function return,
  and output positions.
- **Done when:** A function can return a value computed in a nested block with
  local bindings.
- **Validation:** Add fixtures for nested blocks in `let`, `if`, return, and
  output positions; `emit typed` shows scoped block types and runtime stdout
  matches source order.

### 12. Support Lexical Let Rebinding

- **Commit:** `feat(stage0): support lexical let rebinding`
- **Intent:** Riot treats repeated `let` names as new lexical bindings, not as
  assignment. Resolver and lowering must preserve binding identity so later
  references use the nearest earlier binding without changing earlier captures.
- **Frontend:** Accept same-block rebinding such as `let a = 0; let a = true;`
  and keep nested block shadowing valid. Apply the same policy to actor blocks.
- **Lowering/backend/runtime:** Ensure HIR/RIR scopes distinguish shadowed names
  with binding identities, and ensure closure/actor captures use the binding
  identity they resolved to rather than a raw source name.
- **Fixtures/tests:** Keep positive fixtures for ordinary rebinding, actor
  rebinding, and closure capture of a shadowed binding.
- **Done when:** Normal blocks, actor blocks, and closures resolve rebindings
  deterministically through HIR/RIR binding identities.
- **Validation:** Positive fixtures print the inner/latest binding where used,
  and existing HIR/RIR unit tests prove shadowed captures keep the intended
  binding identity.

<!-- autoresearch:step-12:done -->

### 13. Implement Resolver Symbol Tables

- **Commit:** `feat(stage0): implement resolver symbol tables`
- **Intent:** Self-hosting needs predictable name resolution independent of
  backend fallback behavior.
- **Frontend:** Add a resolver pass before typing. Build symbol tables for
  locals, params, functions, externals, and imported module exports. Resolve
  paths into explicit HIR references.
- **Lowering/backend/runtime:** RIR and codegen should consume resolved names or
  symbols rather than re-deciding path meaning.
- **Fixtures/tests:** Add unknown local, unknown function, unknown module member,
  and local-shadowing-module diagnostics.
- **Done when:** Unknown identifiers cannot reach codegen.
- **Validation:** Unknown local/function/module-member fixtures fail during
  resolver/checking; no new codegen error snapshot is required for unknown
  values.

### 14. Check Function Call Arity in Resolver

- **Commit:** `feat(stage0): check function call arity in resolver`
- **Intent:** Bad calls should fail at the semantic boundary with source spans.
- **Frontend:** Validate argument counts for local functions, externals, prelude
  declarations, and imported `.rsig` exports.
- **Lowering/backend/runtime:** Remove backend arity checks that are now
  unreachable or keep them as defensive errors only.
- **Fixtures/tests:** Add wrong-arity diagnostics for local, external, imported,
  and prelude calls.
- **Done when:** Wrong arity fails before RIR/codegen.
- **Validation:** Add diagnostics for local, external, imported, and prelude
  wrong-arity calls; stderr labels the callee and expected/actual arity.

### 15. Check Function Argument Types

- **Commit:** `feat(stage0): check function argument types`
- **Intent:** The typed interface model only becomes useful once calls are
  checked against it.
- **Frontend:** Validate call arguments against local annotations, inferred
  function types, extern declarations, and imported `.rsig` types.
- **Lowering/backend/runtime:** Preserve checked argument types into RIR so
  codegen does not infer ABI from expression shape alone.
- **Fixtures/tests:** Add diagnostics for scalar mismatch, boxed mismatch,
  imported mismatch, and external mismatch.
- **Done when:** Calling `fn inc(x: i64)` with a string fails in type checking.
- **Validation:** Diagnostics cover scalar mismatch, boxed mismatch, imported
  mismatch, and external mismatch; positive typed calls still compile and run.

### 16. Infer Unannotated Local Binding Types

- **Commit:** `feat(stage0): infer unannotated local binding types`
- **Intent:** Compiler code should not need annotations on every local binding.
- **Frontend:** Infer local `let` binding types for literals, calls, tuples,
  lists, records, `if`, `spawn`, and `receive` where currently supported.
- **Lowering/backend/runtime:** Store inferred types in HIR and carry them into
  RIR locals for ABI decisions.
- **Fixtures/tests:** Add typed snapshot fixtures showing inferred local types.
- **Done when:** `emit typed` displays concrete local types for ordinary
  unannotated bindings.
- **Validation:** Typed snapshots show inferred local types for scalar, tuple,
  list, record, `if`, `spawn`, and receive-supported bindings.

### 17. Infer Unannotated Function Result Types

- **Commit:** `feat(stage0): infer unannotated function result types`
- **Intent:** Small compiler helpers should not all require return annotations.
- **Frontend:** Infer function result types from typed bodies when all returns
  are concrete. Keep explicit annotations authoritative and checked.
- **Lowering/backend/runtime:** Emit inferred results into `.rsig` for exported
  functions.
- **Fixtures/tests:** Add `.rsig` snapshots for unannotated scalar and boxed
  function results.
- **Done when:** `fn answer() { 42 }` exports `i64`, not `_`.
- **Validation:** `.rsig` snapshots for unannotated scalar and boxed functions
  show concrete result types; imported callers type-check against those inferred
  results.

### 18. Reject Unsupported Mixed Branch Types

- **Commit:** `feat(stage0): reject unsupported mixed branch types`
- **Intent:** Unknown ABI fallback hides real type errors and weakens generated
  interfaces.
- **Frontend:** Type `if` by unifying branch types. Reject incompatible concrete
  branch types with a source-backed diagnostic.
- **Lowering/backend/runtime:** Remove codegen fallback paths that static-eval
  mixed branches to avoid typing.
- **Fixtures/tests:** Add diagnostics for `if cond { 1 } else { "x" }` and
  positives for matching boxed branch types.
- **Done when:** Mixed concrete branches fail before codegen.
- **Validation:** Mixed branch diagnostics point at the incompatible branch;
  positive fixtures for matching boxed branches compile and produce expected
  stdout.

### 19. Snapshot Resolved Typed HIR

- **Commit:** `feat(stage0): snapshot resolved typed HIR`
- **Intent:** Typed HIR should be a reliable compiler boundary, not a debug dump.
- **Frontend:** Make `emit typed` show resolved local/function/external/import
  references, types, and symbols in a stable format.
- **Lowering/backend/runtime:** No runtime change. Keep RIR lowering consuming
  the same HIR values.
- **Fixtures/tests:** Update focused typed snapshots and avoid noisy formatting
  churn.
- **Done when:** A reviewer can inspect `emit typed` and understand what each
  name resolved to.
- **Validation:** Focused typed snapshots include local, function, external, and
  imported references with stable resolved identifiers and no nondeterministic
  ordering.

### 20. Preserve Spans Through Typed HIR

- **Commit:** `feat(stage0): preserve spans through typed HIR`
- **Intent:** Later errors from match lowering, type checking, or codegen should
  still point at source expressions.
- **Frontend:** Add spans to typed expressions, statements, params, and relevant
  type nodes.
- **Lowering/backend/runtime:** Carry spans into RIR where diagnostics can still
  happen after lowering.
- **Fixtures/tests:** Add a diagnostic that is detected after HIR construction
  and verifies the reported span.
- **Done when:** Semantic errors from post-parse passes no longer need to use
  broad block spans.
- **Validation:** Add at least one post-HIR diagnostic whose label points at the
  offending expression, not the whole function or block.

## Wave 3: Data Types and Pattern Matching

### 21. Parse Record Type Declarations

- **Commit:** `feat(stage0): parse record type declarations`
- **Intent:** Compiler ASTs and IRs need named records with known fields.
- **Frontend:** Add top-level record type declarations such as
  `type point = { x: i64, y: i64 }`. Store declared type names and fields in
  AST/HIR.
- **Lowering/backend/runtime:** Add record type entries to `.rsig`; no codegen
  change is required until construction checking uses declarations.
- **Fixtures/tests:** Add CST, typed, and `.rsig` snapshots for exported record
  types.
- **Done when:** `emit rsig` includes declared record shape information.
- **Validation:** CST/typed/rsig snapshots for a record type declaration are
  stable; `.rsig` binary roundtrip preserves field names and types.

### 22. Construct Declared Record Values

- **Commit:** `feat(stage0): construct declared record values`
- **Intent:** Named records should be checked data, not just arbitrary runtime
  maps.
- **Frontend:** Resolve record literal paths to declared record types where
  possible. Assign the declared record type to the expression.
- **Lowering/backend/runtime:** Continue lowering to RtValue records, but use
  declared field order for stable layout/rendering.
- **Fixtures/tests:** Add fixtures constructing declared records locally and
  across module boundaries.
- **Done when:** Declared record construction type-checks and exports/imports
  through `.rsig`.
- **Validation:** A two-module fixture exports a record type, constructs it in a
  downstream module, and prints the runtime record successfully.

### 23. Validate Record Field Shapes

- **Commit:** `feat(stage0): validate record field shapes`
- **Intent:** Record mistakes should be caught before runtime.
- **Frontend:** Diagnose missing fields, duplicate fields, unknown fields, and
  field type mismatches for declared records.
- **Lowering/backend/runtime:** No runtime change unless field order metadata is
  needed for stable lowering.
- **Fixtures/tests:** Add diagnostics for each invalid record shape.
- **Done when:** Bad declared record literals fail in checking with field spans.
- **Validation:** Diagnostics cover missing, duplicate, unknown, and mismatched
  fields, each with a label on the specific field or record literal.

### 24. Parse Variant Type Declarations

- **Commit:** `feat(stage0): parse variant type declarations`
- **Intent:** Variants are required for ASTs, options/results, compiler errors,
  and actor messages.
- **Frontend:** Add top-level variant declarations with nullary constructors,
  for example `type color = Red | Green | Blue`.
- **Lowering/backend/runtime:** Add variant type and constructor metadata to
  `.rsig`.
- **Fixtures/tests:** Add CST, typed, and `.rsig` snapshots for simple variants.
- **Done when:** Imported modules can expose variant constructor names through
  `.rsig`.
- **Validation:** CST/typed/rsig snapshots show a simple variant type and its
  nullary constructors; `.rsig` roundtrip preserves constructor order.

### 25. Represent Variants as RtValues

- **Commit:** `feat(rt): represent variants as RtValues`
- **Intent:** Variants need a runtime representation before match and messages
  can use them.
- **Frontend:** Type nullary constructors as values of their declared variant
  type.
- **Lowering/backend/runtime:** Add runtime variant heap objects with type path,
  constructor tag/name, and optional payload placeholder. Lower nullary
  constructors through a new runtime ABI.
- **Fixtures/tests:** Add fixtures constructing and printing nullary variants.
- **Done when:** A nullary variant can be returned from a function and printed.
- **Validation:** Runtime unit tests allocate/render nullary variants; stage0
  fixture returns a variant from a function and stdout matches the constructor
  name.

### 26. Add Variant Constructors With Payloads

- **Commit:** `feat(stage0): add variant constructors with payloads`
- **Intent:** Real compiler data needs payload-bearing variants.
- **Frontend:** Parse constructors with single or tuple payload types. Type
  constructor calls against the declared payload.
- **Lowering/backend/runtime:** Extend runtime variant allocation to store a
  payload `RtValue`, using `unit` for nullary constructors.
- **Fixtures/tests:** Add fixtures for `Some(1)`, error-like variants, nested
  payloads, and payload type mismatch diagnostics.
- **Done when:** Payload variants flow through functions and `.rsig`.
- **Validation:** Fixtures cover scalar, tuple, and boxed payload constructors;
  diagnostics reject wrong payload arity/type before RIR.

### 27. Parse Match Expressions

- **Commit:** `feat(stage0): parse match expressions`
- **Intent:** Match is the central control-flow feature for compiler code.
- **Frontend:** Add `match expr { pattern -> expr, ... }` syntax, AST patterns,
  typed HIR match nodes, and basic branch result typing.
- **Lowering/backend/runtime:** Add RIR match nodes or lower simple matches to
  nested conditionals in RIR.
- **Fixtures/tests:** Add CST/typed/RIR snapshots for match on ints, bools, and
  variants.
- **Done when:** `emit typed` and `emit ir` preserve match structure clearly.
- **Validation:** CST, typed, and RIR snapshots for int, bool, and variant match
  expressions are stable and include all arms in source order.

### 28. Lower Literal Match Patterns

- **Commit:** `feat(stage0): lower literal match patterns`
- **Intent:** Literal matching is the smallest executable match slice.
- **Frontend:** Type-check literal patterns against the scrutinee type and bind
  no names.
- **Lowering/backend/runtime:** Lower scalar literals to LLVM comparisons and
  string literals to runtime equality.
- **Fixtures/tests:** Add runtime fixtures for int, bool, string, and unit
  matches, plus a no-arm-matched diagnostic or runtime trap policy.
- **Done when:** Literal `match` executes in native generated code.
- **Validation:** Runtime fixtures for int, bool, string, and unit matches print
  the selected arm result; unmatched policy is covered by either a diagnostic or
  deterministic runtime failure fixture.

### 29. Lower Tuple and Record Patterns

- **Commit:** `feat(stage0): lower tuple and record patterns`
- **Intent:** Destructuring is needed to make tuples/records practical in
  compiler passes.
- **Frontend:** Add tuple and record patterns with binder names. Check tuple
  arity and record field names/types.
- **Lowering/backend/runtime:** Lower destructuring through runtime tuple/record
  projection and bind extracted values in the arm scope.
- **Fixtures/tests:** Add fixtures destructuring function returns and nested
  values.
- **Done when:** A match arm can bind fields and use them in its body.
- **Validation:** Fixtures destructure tuple and record values returned from
  functions, print bound variables, and reject arity/field mismatches.

### 30. Lower Variant Constructor Patterns

- **Commit:** `feat(stage0): lower variant constructor patterns`
- **Intent:** Variants become useful only when matches can inspect them.
- **Frontend:** Resolve constructor patterns to declared constructors. Check
  payload pattern shape.
- **Lowering/backend/runtime:** Add runtime ABI for variant tag test and payload
  extraction. Lower constructor patterns through those operations.
- **Fixtures/tests:** Add fixtures for `Some(x)`, error/result-style matches,
  and wrong-constructor diagnostics.
- **Done when:** Constructor matches bind payloads and execute in native code.
- **Validation:** Fixtures match `Some(x)`/result-style constructors and print
  payload-derived values; diagnostics reject constructors from the wrong variant
  type.

## Wave 4: Actor Runtime Semantics

### 31. Support Multi-Arm Receive

- **Commit:** `feat(stage0): support multi-arm receive`
- **Intent:** Actors need to react to more than one message shape.
- **Frontend:** Parse `receive { p1 -> e1, p2 -> e2 }`. Represent receive arms
  with patterns in AST/HIR/RIR.
- **Lowering/backend/runtime:** Extend Actor IR receive states to include ordered
  arm tests. Keep the first implementation matching only against the current
  mailbox candidate.
- **Fixtures/tests:** Add actor fixtures for two literal arms and typed snapshots
  for receive arm structure.
- **Done when:** An actor can receive different messages and choose different
  branches.
- **Validation:** Actor fixture sends at least two message shapes to one actor
  and verifies different branch output; `emit actor-ir` shows ordered receive
  arm tests.

### 32. Add Mailbox Cursor Receive Scanning

- **Commit:** `feat(rt): add mailbox cursor receive scanning`
- **Intent:** Selective receive must not drop or block forever on unmatched
  earlier messages.
- **Frontend:** No syntax change beyond multi-arm receive.
- **Lowering/backend/runtime:** Change runtime mailbox handling to let generated
  code accept or skip a candidate. Preserve unmatched messages and consume only
  the selected one.
- **Fixtures/tests:** Add actor fixture sending unmatched then matched messages
  and verifying the unmatched message remains available.
- **Done when:** Selective receive semantics are observable and deterministic.
- **Validation:** Actor fixture sends unmatched then matched messages, consumes
  only the matched one, and later receives the previously unmatched message.

### 33. Lower Receive Literal Patterns

- **Commit:** `feat(stage0): lower receive literal patterns`
- **Intent:** Actors need typed message dispatch for simple protocols.
- **Frontend:** Type receive literal patterns against the actor message value.
- **Lowering/backend/runtime:** Lower literal tests using scalar comparison or
  runtime value equality against the candidate message.
- **Fixtures/tests:** Add receive fixtures for int, bool, string, and unit
  messages.
- **Done when:** Literal receive arms execute through Actor IR, not checker
  simulation.
- **Validation:** Actor fixtures for int, bool, string, and unit receive arms
  pass; LLVM snapshots include runtime/scalar tests inside resume functions.

### 34. Lower Receive Tuple and Variant Patterns

- **Commit:** `feat(stage0): lower receive tuple and variant patterns`
- **Intent:** Compiler actors will pass structured work items, not only strings.
- **Frontend:** Allow tuple and variant patterns in receive arms. Bind payload
  names in arm bodies.
- **Lowering/backend/runtime:** Reuse runtime tuple/variant projection and tag
  tests inside actor resume functions.
- **Fixtures/tests:** Add actor fixtures for tuple work items and result/error
  variants.
- **Done when:** Actors can destructure structured messages after suspension.
- **Validation:** Actor fixtures receive tuple and variant messages, bind
  payloads, and print payload-derived output after a suspension/resume cycle.

### 35. Send Structured Down Messages for Monitors

- **Commit:** `feat(rt): send structured down messages for monitors`
- **Intent:** Monitors must be actor semantics, not stdout side effects.
- **Frontend:** Keep `monitor(actor_id)` surface unchanged for now.
- **Lowering/backend/runtime:** Track watcher actor-id relationships. On target
  termination, enqueue a structured down message to watchers instead of printing
  `down {actor_id}`.
- **Fixtures/tests:** Update monitor fixtures so watcher actors receive and print
  down messages explicitly.
- **Done when:** Runtime shutdown no longer prints monitor notifications itself.
- **Validation:** Monitor fixtures pass only when watcher actors explicitly
  receive and print structured down messages; direct runtime tests assert no
  monitor stdout is emitted by shutdown.

### 36. Implement Link Failure Propagation

- **Commit:** `feat(rt): implement link failure propagation`
- **Intent:** Linked actors need predictable failure behavior before supervisors
  can be written.
- **Frontend:** Keep `link(actor_id)` surface unchanged.
- **Lowering/backend/runtime:** Track bidirectional links. Define stage0 link
  semantics as linked termination propagation using structured exit messages or
  direct termination, and document the choice in `compiler/rt/AGENTS.md` if it
  becomes runtime contract.
- **Fixtures/tests:** Add linked actor termination fixture and direct runtime
  unit tests.
- **Done when:** Link behavior is deterministic and no longer a boolean stub.
- **Validation:** Runtime unit tests and actor fixtures cover linked termination;
  repeated runs produce the same output and actor termination state.

### 37. Add Scheduler Run Budget

- **Commit:** `feat(rt): add scheduler run budget`
- **Intent:** The compiler actor runtime should not let one actor monopolize the
  scheduler.
- **Frontend:** No syntax change.
- **Lowering/backend/runtime:** Add a per-schedule-pass budget or per-actor poll
  budget. Treat `POLL_YIELD` as progress that rotates scheduling.
- **Fixtures/tests:** Add deterministic actor fixture showing interleaving under
  repeated sends or local progress states.
- **Done when:** Round-robin behavior is enforced by runtime policy, not only by
  fixture shape.
- **Validation:** A scheduler fixture with one busy actor and one ready actor
  shows deterministic interleaving under the configured budget.

### 38. Insert Loop Safepoints

- **Commit:** `feat(stage0): insert loop safepoints`
- **Intent:** Once loops exist, actors and long-running functions need compiler
  safepoints to keep scheduling honest.
- **Frontend:** Depends on loop syntax. No standalone syntax change.
- **Lowering/backend/runtime:** Emit runtime poll calls or Actor IR yield states
  on loop backedges. In non-actor functions, make the safepoint cheap and
  deterministic.
- **Fixtures/tests:** Add a long-loop actor fixture that still lets another
  actor run.
- **Done when:** Long loops cannot starve the single-threaded scheduler.
- **Validation:** A long-loop actor fixture still allows another actor to print
  before the loop completes; LLVM/Actor IR snapshots show safepoint insertion.

### 39. Add Timer Messages for Receive Timeouts

- **Commit:** `feat(rt): add timer messages for receive timeouts`
- **Intent:** Compiler services often need request timeouts and watchdogs.
- **Frontend:** Add minimal receive timeout syntax only if the grammar decision
  is already clear; otherwise expose a runtime `send_after(actor_id, ms, msg)` helper
  first.
- **Lowering/backend/runtime:** Add timer queue support to the scheduler and
  enqueue timeout messages deterministically.
- **Fixtures/tests:** Add an actor fixture that receives a timer message after no
  normal message arrives.
- **Done when:** Timeout behavior is tested without relying on flaky wall-clock
  sleeps.
- **Validation:** Timer tests use a deterministic runtime clock or tick API;
  actor fixture receives a timeout message only after advancing the scheduler
  clock.

### 40. Expose Actor Message Types in Rsig

- **Commit:** `feat(stage0): expose actor message types in rsig`
- **Intent:** Imported actor APIs need typed message contracts.
- **Frontend:** Track actor-returning functions and message payload types where
  annotations make them knowable.
- **Lowering/backend/runtime:** Extend `.rsig` with actor/message summaries and
  bump the binary format version.
- **Fixtures/tests:** Add `.rsig` roundtrip tests and import diagnostics for
  sending the wrong message type to an imported actor id.
- **Done when:** A downstream module can type-check sends to imported actors.
- **Validation:** `.rsig` roundtrip preserves actor message summaries; imported
  send fixtures include one accepted message and one rejected wrong-message
  diagnostic.

## Wave 5: Control Flow and Self-Hosting Structure

### 41. Parse While Loops

- **Commit:** `feat(stage0): parse while loops`
- **Intent:** Self-hosted compiler code will need straightforward iterative
  loops before all recursion and higher-order helpers are mature.
- **Frontend:** Add `while condition { ... }` as an expression or statement with
  `unit` type. Check condition is bool.
- **Lowering/backend/runtime:** Lower to RIR loop blocks and LLVM branches. Add
  safepoints on backedges when the safepoint ABI exists.
Resolved hardening gap: `while` is now parsed as control-flow syntax rather
than as an ordinary value path. The expression has `unit` type, requires a Bool
condition, recursively validates/checks the body, lowers through typed HIR and
Lambda IR, and emits LLVM loop condition/body/continuation blocks with a
backedge. Source fixtures pin terminating false-condition loops, nested loop
lowering, and unit-valued sequencing before later output.

Remaining boundary: loop-carried mutation/accumulators and actor-loop fairness
fixtures need local mutable assignment or an equivalent state-update feature.
Until then, while coverage intentionally focuses on false-condition execution,
condition/body diagnostics, lowering shape, and label stability.

- **Fixtures/tests:** Add loop accumulator and actor loop fairness fixtures once
  loop-carried state/mutation or an equivalent iteration pattern exists.
- **Validation:** `compiler_like_control_flow_boundaries` now records `while` as
  supported control-flow syntax alongside recursion, match, and receive.
  `compiler_like_while_lowering_plan` models the checker/lowering
  boundary: bool conditions produce loop blocks, backedges, and safepoints,
  while non-bool or unknown conditions stay diagnostic-only. Lower-layer while
  tests pin lexer reservation, parser construction, inference
  Bool-condition/unit-result behavior, and typed-HIR unit lowering. LLVM coverage
  now includes both lower-layer loop construction and source-level `emit llvm`
  assertions for the generated `while.cond`/`while.body`/`while.cont` branch
  structure, including uniqued nested-loop labels. `emit all` coverage pins
  that parsed while loops remain visible
  across CST, typed HIR, Lambda IR, and LLVM output. Source fixtures cover
  `while false { ... }` runtime behavior, nested false-condition loops, `while`
  as a sequenced unit-valued let initializer, non-Bool while-condition
  diagnostics, and recursive
  validation of while bodies even when the first source runtime fixture uses a
  false condition.

### 42. Recursive Function Boundaries

Resolved hardening gap: direct self-recursive functions compile and run through
normal inference/lowering/backend paths, with `recursive_factorial` and LLVM
self-call coverage pinning the behavior. Annotated mutually recursive top-level
functions are also supported when every function in the cycle has parameter and
return annotations.

Resolved hardening gap: mutual-recursion groups with incomplete annotations can
be seeded with monomorphic placeholders and solved when annotations or body facts
provide concrete parameter/result constraints. Fully unannotated groups and
partially annotated groups now both use the grouped path when enough facts are
available. Underconstrained cycles still receive a dedicated source-backed missing-facts
diagnostic until a richer group solver exists.

- **Validation:** `recursive_factorial`, `mutual_recursion_annotated`,
  `mutual_recursion_unannotated`, `mutual_recursion_partial_annotation`,
  `mutual_recursion_param_annotation`,
  `compile_lib_exports_annotated_mutual_recursion`,
  `compile_lib_exports_partial_mutual_recursion`,
  `mutual_recursion_unannotated_missing_facts`,
  `mutual_recursion_unannotated_missing_param_facts`,
  `mutual_recursion_partial_missing_param_facts`, and
  `mutual_recursion_unannotated_mismatched_returns`.

### 43. Support Unannotated Mutual Recursion Groups

- **Commit:** `feat(stage0): support unannotated mutual recursion groups`
- **Intent:** Real compiler modules often use mutually recursive helpers where
  annotating every helper is noisy.
- **Frontend:** Mutual-recursion cycles are now detected before body inference
  and incomplete signatures are seeded with shared monomorphic placeholders.
  Present annotations are preserved as constraints and missing annotations are
  inferred from body facts when possible. Future work can broaden this into a
  richer grouped solver for cycles that lack concrete parameter/result facts,
  without weakening source-backed diagnostics.
- **Lowering/backend/runtime:** Preserve the existing local LLVM declaration
  ordering and object export behavior already exercised by annotated recursion.
- **Fixtures/tests:** Add compiler-like mutually recursive helpers plus arity and
  type diagnostics across the group.
- **Done when:** Mutually recursive top-level functions with enough inferred
  constraints compile and run without requiring every function in the cycle to be
  annotated.
- **Validation:** `mutual_recursion_unannotated`,
  `mutual_recursion_partial_annotation`, `mutual_recursion_param_annotation`,
  `compile_lib_exports_partial_mutual_recursion`, and lower-layer inference
  tests prove fully unannotated and partially annotated group paths, including
  exported `.rsig` signatures. Diagnostics now cover no-facts groups, unannotated and partial groups
  with concrete returns but unconstrained parameters, and mismatched return
  constraints. `compiler_like_mutual_recursion_unannotated` exercises a
  compiler-shaped parser helper pair with unannotated mutually recursive
  functions over token variants and lists. `compiler_like_mutual_recursion_group_plan`
  models the remaining group-inference boundary: seeded placeholder groups may
  solve from complete annotations or concrete constraints, while missing
  parameter facts and mismatched return constraints remain diagnostics.

### 44. Lambda Expression and Closure Boundaries

Resolved hardening gap: expression-position lambdas now use the agreed
`fn(...) { ... }` syntax, infer parameter/result types from body and application
constraints, lower through Lambda IR, and apply as callable runtime values.
Captured closures are also supported: closure conversion computes capture sets,
LLVM helper symbols are module-scoped, and runtime/GC behavior is covered by
fixtures that capture both scalars and boxed values.

Remaining boundary: lambda and apply metadata stays conservative when typed HIR
is built without expression-type facts. This is intentional and is covered by the
no-facts metadata tests; normal source compilation runs inference first and gets
concrete metadata whenever constraints are available.

- **Validation:** `lambda_multi_arg_apply`, `lambda_multi_param_curried`,
  `lambda_curried_capture`, `lambda_gc_captures`, `lambda_shadowing_capture`,
  `function_value`, `function_partial_application`,
  `typed_hir_uses_inference_facts_for_lambda_parameter_types`,
  `typed_hir_uses_inference_facts_for_lambda_apply_results`, and the
  lambda/actor capture-boundary unit tests.

### 45. Closure ABI and Imported Callable Boundaries

Resolved hardening gap: closure values have a concrete runtime ABI, including
imported polymorphic closure values and closure values nested in tuple, list,
record, and variant wrappers. `.rsig` callable matching now recurses through
arrows, actor ids, and generic record/variant applications while preserving raw
unknown/type-variable ABI rejection.

Remaining boundary: raw unknown imported values, empty lists with unknown element
ABI, and tuples that contain raw unknown ABI still report `imported value has
unknown ABI`; only wrappers with unambiguous runtime representations are treated
as concrete.

- **Validation:** `imported_higher_order_function_uses_arrow_rsig`,
  `compile_lib_imported_higher_order_polymorphic_closure_values_have_concrete_abi`,
  `compile_lib_imported_higher_order_container_closure_params_have_concrete_abi`,
  `compile_lib_imported_higher_order_variant_and_list_closure_params_have_concrete_abi`,
  `compile_lib_imported_tuple_containing_unknown_keeps_unknown_abi_diagnostic`,
  `compiler_like_imported_closure_actor_abi`, and
  `compiler_like_imported_arrow_container_matching`.

### 46. Multi-Module Linking and Interface Dependency Boundaries

Resolved hardening gap: interface-first compilation now has typed multi-module
coverage across `.rsig` signatures and object files. Transitive `.rsig`
dependencies are walked recursively for object linking, same-command compiled
signatures are visible in memory, and dependency fingerprint mismatches are
reported even when a module was already seen as a direct import.

Remaining boundary: stage0 still uses module-level dependency fingerprints rather
than fine-grained per-export invalidation. This is enough for the current
reference compiler/linker boundary, but future build planning may still want
per-export fingerprints.

- **Validation:** `compile_lib_three_module_interfaces_link_through_object_dir`,
  `compile_multiple_sources_links_dependency_objects`,
  `imported_object_resolver_includes_transitive_dependency_objects`,
  `imported_object_resolver_rejects_transitive_dependency_fingerprint_mismatch`,
  `imported_object_resolver_checks_fingerprint_for_previously_seen_direct_imports`,
  and `binary_rsig_roundtrips_dependency_fingerprints`.

### 47. Per-Export Rsig Fingerprints

Resolved hardening gap: `.rsig` signatures store deterministic fingerprints for
exported functions, externals, and type declarations, and canonical signature
text exposes those fingerprints beside each declaration. The fingerprints are
computed from canonical declaration text after sorting, so export/type reordering
is stable while changed export or type shapes change the affected fingerprint and
the enclosing module fingerprint.

Remaining boundary: dependency invalidation still consumes module-level
fingerprints. Constructors and future actor summaries are represented through
their enclosing type/signature shapes today; future build-planner work can add
more granular dependency edges if it needs constructor- or actor-summary-level
invalidation.

- **Validation:** `rsig_export_fingerprints_are_stable_under_reorder`,
  `rsig_fingerprints_change_when_export_or_type_shape_changes`,
  `binary_rsig_roundtrips_dependency_fingerprints`, and `.rsig` binary
  roundtrip tests for functions, externals, and type declarations.

### 48. Rsig Dependency Metadata

Resolved hardening gap: `.rsig` files encode dependency metadata with module
fingerprints, canonical text exposes those dependencies, and the driver uses the
metadata for transitive object resolution and stale-dependency diagnostics.

Remaining boundary: dependency metadata is still module-granular. When per-export
fingerprints land, dependency edges should be revisited so callers can depend on
specific exported shapes instead of whole-module fingerprints.

- **Validation:** `emit_all_preserves_pipeline_phase_order`,
  `emit_all_exposes_actor_message_types_in_rsig`,
  `binary_rsig_roundtrips_dependency_fingerprints`,
  `compile_lib_three_module_interfaces_link_through_object_dir`, and the
  imported object resolver fingerprint tests.

### 49. Emit Deterministic Interface Text

Resolved hardening gap: `emit all` includes canonical `.rsig` interface text for
module name, module fingerprint, dependencies, type declarations, exported
functions/externals, per-declaration fingerprints, symbols, and actor message
summaries. The text is deterministic across repeated emits for the same source,
so reviewers can inspect interface changes without decoding binary `.rsig` data
manually.

Resolved hardening gap: `emit interface` now prints the canonical `.rsig`
interface text directly, giving reviewers a focused text-only interface surface
without slicing the `emit all` output. The same pass can also decode binary
`.rsig` artifacts into canonical text, so the primary compilation/linking
artifact has a direct review surface.

Resolved hardening gap: `interface-diff` compares two binary `.rsig` artifacts
and summarizes module fingerprint changes plus added, removed, and changed
per-type/per-export fingerprints. It can also compare two directories of `.rsig`
artifacts and summarize changed/added/removed modules, changed modules'
per-type/per-export fingerprint details, plus dependent modules impacted by
changed imports. Workspace diffs support `--output` review artifacts just like
pairwise `.rsig` diffs. Dependency-only consumer changes are also visible: when
an imported module's fingerprint changes because of an unused export, consumers
are reported as module-fingerprint changes/impacted importers without inventing
per-export changes for the consumer. This gives interface review concrete
artifact-to-artifact and workspace-directory diff paths without requiring
reviewers to manually compare canonical text blocks.

Remaining boundary: binary `.rsig` remains the primary interface artifact for
compilation and linking, and future review tooling may still want richer
workspace-wide reports for large multi-module projects. The review policy is now
partly implemented for pairwise artifacts and workspace artifact directories and
modeled in compiler-like form: stable per-export fingerprints should stay quiet,
while changed, added, or removed exports need human review. A compact
workspace-review model now also records the larger orchestration policy: direct
interface changes need review, and dependent modules should be marked impacted
when an imported module changes.

- **Validation:** `emit_all_preserves_pipeline_phase_order`,
  `emit_all_includes_stable_interface_text`,
  `emit_interface_outputs_canonical_interface_text`,
  `emit_interface_writes_review_artifact`,
  `emit_interface_decodes_binary_rsig_artifact`,
  `emit_interface_records_type_and_actor_message_shapes`,
  `emit_interface_records_external_abi_shapes`,
  `emit_interface_records_imported_dependencies`,
  `interface_diff_summarizes_rsig_review_changes`,
  `interface_diff_reports_dependency_only_workspace_changes`,
  `interface_diff_summarizes_workspace_review_changes` including workspace
  `--output` review artifacts,
  `emit_all_exposes_actor_message_types_in_rsig`,
  `emit_all_distinguishes_concrete_and_unknown_actor_message_types`,
  `compiler_like_interface_review`, and
  `compiler_like_workspace_interface_review`.

### 50. Add Compiler-Shaped Smoke Fixture

Resolved hardening gap: stage0 now has an initial multi-module compiler-shaped
smoke test. `Syntax` defines token variants, `Analyze` classifies tokens into
structured variants, `Worker` exports an `actor_id<Syntax.token>` factory that
receives token work and prints deterministic summaries, and a consumer links only
against the emitted `.rsig`/object artifacts. The fixture builds through
`compile-lib`, resolves transitive signatures/objects, runs through normal native
codegen and the actor runtime, and prints `ident`, `42`, and `plus` without any
special-case compiler path.

Remaining boundary: this is still a compact smoke fixture over hardcoded token
values rather than a real tokenizer/parser pipeline. The current module-snapshot
set is intentionally paused after the driver/artifact boundary because parser,
checker/lowering, backend/runtime, diagnostic/reporting, and driver/artifact
phase families now each have compact executable coverage; adding another
near-duplicate snapshot would provide less signal than returning to a concrete
implementation or integration boundary. Future self-hosting slices can grow this
area with real tokenizer/parser pipeline work, larger dedicated snapshots that
exercise new implementation, and loop-heavy examples once loop-carried
state/mutation or an equivalent iteration pattern exists.

- **Validation:** `compile_lib_compiler_shaped_actor_smoke` builds the smoke
  modules through `compile-lib`, links the consumer as a native executable, and
  runs deterministically through actor send/receive.
  `compiler_shaped_actor_smoke_emit_boundaries` emits the `Worker` module with
  dependency signatures and checks representative typed, `.rsig`, lambda IR, and
  Actor-IR sections stay source/interface-backed.
  `compiler_like_source_scanner` adds the first richer source-scanning model,
  walking a recursive source-character stream into word/number/symbol lexemes
  before summarizing the compiler-shaped token stream.
  `compiler_like_tokenizer_parser_pipeline` connects that boundary to parsing by
  scanning source-character streams into parser tokens, building a tiny function
  AST, and preserving recovery/error accounting for malformed input. The real
  `SourceParser` boundary now also has lower-layer coverage for lexer trivia
  spans flowing into parser AST nodes, source-backed unexpected-character,
  unterminated-block-comment, unterminated-string, invalid-string-escape, and
  invalid-character-escape diagnostics, valid escaped string/character literal
  runtime regressions, and parser-error spans after skipped line/block comments.
  `compiler_like_parser_ast_builder` adds the next compact parser-shaped model,
  consuming token variants into a tiny function/statement/expression AST,
  summarizing declarations, lets, calls, literals, and pinning representative
  parser recovery/error paths for missing delimiters and unexpected starts.
  `compiler_like_parser_name_pipeline` extends that smoke boundary from parser
  output into a tiny name/arity checking pass, preserving let-initializer order,
  function signatures, parse-error accounting, unknown callee diagnostics, and
  arity mismatch accounting in one executable compiler-shaped pipeline.
  `compiler_like_checker_lowering_pipeline` adds a compact checker-to-lowering
  model, carrying typed expression facts into scalar/value slot classification
  while keeping unknown calls and missing locals explicit.
  `compiler_like_emit_plan` extends the smoke boundary into a compact emit/codegen
  model, classifying scalar values, boxed values, helper symbols, exported call
  symbols, and returns from lowered operation facts.
  `compiler_like_module_family_snapshot` adds a compact multi-module snapshot
  model, summarizing imports, exports, typed nodes, actor nodes, and review-worthy
  diagnostics across a small compiler-shaped module family.
  `compiler_like_frontend_pipeline_snapshot` adds an adjacent frontend snapshot
  model, carrying token streams through parse-error accounting and typed/lowered
  slot summaries for a valid module plus a recovery-path module.
  `compiler_like_parser_module_snapshot` adds a dedicated parser-module snapshot
  model, summarizing scanner, AST, parser, and recovery modules across token
  cases, productions, recovery rules, snapshots, and review diagnostics.
  `compiler_like_checker_module_snapshot` adds a parallel checker/lowering module
  snapshot model, summarizing resolver, inference, checker, and lowering modules
  across declarations, constraints, substitutions, lowered nodes, diagnostics,
  and review labels.
  `compiler_like_backend_module_snapshot` adds a backend/runtime module snapshot
  model, summarizing Lambda, actor lowering, LLVM emission, and runtime modules
  across lowered nodes, AIR operations, LLVM blocks, runtime hooks, diagnostics,
  and review labels.
  `compiler_like_diagnostic_module_snapshot` adds a diagnostic/reporting module
  snapshot model, summarizing span coverage, primary messages, hints, regression
  counts, snapshot coverage, and review labels across lexer, parser, checker,
  and backend diagnostic surfaces.
  `compiler_like_driver_module_snapshot` adds a driver/artifact module snapshot
  model, summarizing command surfaces, emitted artifacts, signature/object flows,
  diagnostics, smoke coverage, and review labels across driver, signature store,
  object resolver, and emit-mode boundaries.
  `compiler_like_dependency_invalidation` records the current module-granular
  dependency invalidation boundary and contrasts it with future per-export edges
  that could avoid rebuilds when an unused export changes.
  `emit_interface_outputs_canonical_interface_text` adds a focused interface-only
  emit command that prints the same canonical interface text as the `emit all`
  `.rsig` section without the other pipeline phases,
  `emit_interface_writes_review_artifact` proves the same text can be written to
  an explicit review artifact path without stdout noise, and
  `emit_interface_records_external_abi_shapes` pins review text for exported
  external declarations and ABI symbols. `compiler_like_interface_review`
  models the adjacent interface-review policy: stable per-export fingerprints stay
  quiet, while changed, added, and removed exports are counted as review-worthy
  changes.
  `compiler_like_while_lowering_plan` records the control-flow boundary that
  guided the implemented while-loop slice: while-condition checking, loop
  block/backedge/safepoint accounting, and diagnostic-only non-bool/unknown
  conditions. `compiler_like_mutual_recursion_group_plan` records the adjacent
  recursive-group implementation boundary by modeling seeded placeholders,
  constraint convergence, and diagnostic-only missing-fact/mismatch cases.

## Documentation Slice Acceptance

This document itself is a documentation-only slice.

- **Commit:** `docs(compiler): add stage0 vertical roadmap`
- **Validation:** Run `git diff --check -- compiler/PLAN.md`.
- **Commit scope:** Stage only `compiler/PLAN.md`; leave unrelated dirty work
  untouched.
