# Effects, Continuations, and Fiber Stacks

## Source anchors

- `vendor/ocaml/runtime/caml/fiber.h`
- `vendor/ocaml/runtime/caml/stack.h`
- `vendor/ocaml/runtime/fiber.c`
- `vendor/ocaml/runtime/callback.c`
- `vendor/ocaml/runtime/backtrace_nat.c`
- `vendor/ocaml/runtime/signals_nat.c`
- `vendor/ocaml/runtime/minor_gc.c`
- `vendor/ocaml/runtime/major_gc.c`
- `vendor/ocaml/runtime/shared_heap.c`
- `vendor/ocaml/runtime/compare.c`
- `vendor/ocaml/runtime/hash.c`
- `vendor/ocaml/runtime/extern.c`
- `vendor/ocaml/runtime/amd64.S`
- `vendor/ocaml/runtime/arm64.S`
- `vendor/ocaml/runtime/power.S`
- `vendor/ocaml/runtime/riscv.S`
- `vendor/ocaml/runtime/s390x.S`
- `vendor/ocaml/runtime/amd64nt.asm`

## Runtime role

- Effects are not modeled as a small exception-side extension.
- The runtime has a dedicated stack-switching subsystem for:
  - creating fresh OCaml stacks
  - suspending the current stack into a continuation
  - resuming a suspended stack
  - walking parent handlers when an effect is reperformed
- In native code, the control-transfer primitives are split between C metadata/helpers and architecture-specific assembly.
- This spec intentionally treats the native runtime as the reference surface and ignores bytecode-interpreter-specific machinery.

## Stack and fiber model

- A fiber is a suspended OCaml stack represented by `struct stack_info`.
- `stack_info` stores:
  - the suspended stack pointer
  - the suspended exception pointer
  - a pointer to `stack_handler`
  - cache/pooling metadata
  - an internal stack id
- `stack_handler` is the per-stack effect state:
  - `handle_value`
  - `handle_exn`
  - `handle_effect`
  - `parent`
- The `parent` pointer is the control-transfer chain for effects and stack completion.
- Native OCaml stacks are chunked runtime-managed stacks, not raw OS thread stacks.
- A chunk begins when the program starts, when a fiber is created, or when C calls back into OCaml.

## Continuation representation

- Effect continuations use `Cont_tag`.
- The runtime stores the suspended stack pointer as a tagged `Val_ptr(stack)` value so the GC does not follow it as a normal OCaml pointer.
- Native perform/reperform logic also uses continuation storage to track the tail of the linked list of parent fibers while walking handlers.
- A continuation is therefore not just a high-level control token. It is a runtime object that directly references suspended stack state.

## Control-transfer primitives

- `caml_runstack new_stack function argument`
  - starts execution on a fresh OCaml stack
  - installs handlers for normal return and exception completion
  - when the child stack finishes, frees it, restores the parent stack, and runs `handle_value` or `handle_exn` on the parent
- `caml_perform effect continuation`
  - captures the current OCaml stack into the continuation
  - switches to the parent OCaml stack
  - runs the parent stack's `handle_effect`
  - if no parent stack exists, raises `Effect.Unhandled`
- `caml_reperform effect continuation last_fiber`
  - appends the current stack onto the parent-fiber chain
  - continues walking upward to the next installed effect handler
  - if the walk runs out of parent stacks, switches back to the original performer stack before raising `Effect.Unhandled`
- `caml_resume new_fiber function argument`
  - checks whether the continuation/fiber is still live
  - makes the current stack the parent of the resumed stack
  - switches execution to the resumed stack
  - raises `Effect.Continuation_already_resumed` if the continuation was already taken

## One-shot continuation behavior

- Continuations are linear by default.
- `caml_continuation_use_noexc` swaps `NULL` into the continuation's active stack slot.
- On a single domain this can be a direct store.
- With multiple domains it uses an atomic compare-and-exchange.
- `caml_continuation_use` raises `Effect.Continuation_already_resumed` when the active stack slot was already cleared.
- `caml_continuation_replace` exists, but only for tightly controlled runtime operations such as cloning/backtrace support. The header comment explicitly says the GC must not run between `use` and `replace`.

## Stack growth and movability

- Fiber stacks are heap allocations with size-class pooling/caching.
- `caml_try_realloc_stack` can grow the current stack by allocating a bigger stack and copying its contents.
- When a stack moves, the runtime rewrites:
  - exception-pointer chains
  - `c_stack_link` records used for C-to-OCaml transitions
  - saved frame pointers on architectures that need them
- Effects therefore depend on movable runtime-managed stack objects, not on a fixed native stack address.

## GC and root-scanning behavior

- `caml_scan_stack` walks the current stack and every `parent` stack.
- For each stack, the runtime scans:
  - live frame roots
  - `handle_value`
  - `handle_exn`
  - `handle_effect`
- `caml_maybe_expand_stack` also ensures a free `gc_regs` bucket exists before returning from C to OCaml code.
- This makes fiber switching part of the GC root model, not an isolated control-flow subsystem.

## Continuations under minor/major GC

- Continuations are not treated like ordinary scanned blocks.
- Minor GC oldifies `Cont_tag` specially:
  - it copies the continuation block itself
  - then scans the suspended stack referenced by the continuation
- Major GC marking also special-cases `Cont_tag`:
  - `caml_darken_cont` marks the continuation
  - then recursively scans the suspended stack roots behind it
- Shared-heap verification and compaction do the same kind of continuation-aware traversal.
- Observable consequence: suspended computations stay live because the runtime follows their stack roots explicitly, not because continuation fields behave like a normal object graph.

## Callback boundary behavior

- C-to-OCaml callbacks deliberately break implicit effect propagation across the callback boundary.
- Before entering the callback, the runtime:
  - captures the current `parent` stack in a continuation-like root
  - clears the current stack's parent link
- This forces an unhandled effect inside the callback to become `Effect.Unhandled` instead of silently skipping over the C frame.
- The saved parent stack is kept alive as a root while the callback executes, then restored afterwards.

## Backtraces and observability

- Native callstack capture explicitly includes parent fibers.
- `caml_get_current_callstack` and `caml_get_continuation_callstack` both traverse the parent-fiber chain.
- Continuation backtrace capture temporarily takes the continuation stack, inspects it, then restores it with `caml_continuation_replace`.
- Effects are therefore visible in debugging behavior, not only in execution results.

## Interaction with generic runtime services

- Structural comparison on continuations raises `Invalid_argument("compare: continuation value")`.
- Generic hashing does not inspect continuation contents; all continuations hash to the same value.
- Marshaling a continuation is rejected with `Invalid_argument("output_value: continuation value")`.
- These behaviors are documented in more detail in:
  - [`comparison-hashing.md`](./comparison-hashing.md)
  - [`marshaling-and-code-loading.md`](./marshaling-and-code-loading.md)

## Signals, GC polling, and shared stack machinery

- Native `caml_garbage_collection` enters through generated assembly only.
- It relies on frame descriptors plus the current runtime-managed OCaml stack chunk layout.
- The effect runtime shares this stack/chunk/frame-descriptor machinery with GC polling and signal handling.
- For zort, stack switching, root scanning, polling, and backtrace walking should be designed as one subsystem boundary, not four unrelated features.

## Observable error paths

- Performing with no enclosing effect handler raises `Effect.Unhandled`.
- Resuming an already-consumed continuation raises `Effect.Continuation_already_resumed`.
- Callback boundaries intentionally turn cross-boundary effect propagation into the same `Effect.Unhandled` behavior.
- Stack growth failure raises `Stack_overflow` instead of silently truncating the computation.

## zort takeaways

- Effects are a mandatory runtime subsystem if zort intends to support modern OCaml control flow.
- A maintainable rewrite should model continuations as typed, one-shot handles over suspended stack state, not as generic opaque values.
- Parent-stack linkage, handler triples, and continuation linearity are the core semantics to preserve if zort wants effect compatibility at the behavior level.
- If zort chooses a different implementation strategy, it still needs explicit answers for:
  - where suspended stack state lives
  - how parent handlers are linked
  - how root scanning sees suspended computations
  - how C/FFI boundaries block or permit effect propagation

## zort control-kernel baseline notes

Executable model:

- [`effects/Continuations.tla`](./effects/Continuations.tla)
- [`effects/README.md`](./effects/README.md)

- zort now has a dedicated `ControlKernel` subsystem in `zort/src/control_kernel.zig`.
- The current semantic model includes:
  - typed `FiberHandle`s with explicit parent links,
  - typed `ContinuationHandle`s with owned captured stack state and roots,
  - per-fiber handler stacks with explicit `handle_effect` / `handle_value` / `handle_exn` fields.
- Fibers now own explicit managed stacks:
  - frame records carry a site id plus frame-owned roots,
  - stack limits are explicit runtime policy through `StackLimits`,
  - frame/root storage now grows explicitly from configured initial capacities up to configured maxima instead of depending on container-default growth,
  - overflow is reported as a typed `StackOverflow` error only once that managed-stack growth policy cannot satisfy the requested frame/root count.
- Suspended continuations now store an explicit `SuspendedStack` payload:
  - owner domain
  - capture site id
  - captured frame count
  - captured root count
  - the managed stack frames themselves
- Suspended stacks can now be snapshotted as deep copies for debugging and inspection:
  - `snapshotContinuationStack` copies the suspended managed-stack payload without resuming it,
  - the snapshot remains valid even after the original continuation is resumed or dropped,
  - zort therefore does not need an OCaml-style temporary `use`/`replace` dance just to inspect a suspended continuation.
- Suspended continuations expose their captured values to the collector through the generic `RootProvider` interface instead of special GC-only hooks.
- Capturing a continuation transfers the managed stack out of the active fiber into the continuation state.
- Resuming a continuation restores that managed stack to the resumed fiber, so the continuation stops providing those roots only because ownership moved back to the active stack.
- Resuming a continuation may now migrate the resumed fiber into a different domain:
  - the resumed fiber adopts the active domain of the resumer,
  - the suspended stack payload is the migration unit,
  - ownership transfer is explicit at resume time rather than an implicit raw-pointer move,
  - zort therefore treats resume as the future cross-domain handoff point rather than forcing same-domain resumes forever.
- `perform` now walks the current fiber's handler stack and then the parent-fiber chain to find the nearest matching handler.
- `resumeContinuation` consumes a continuation once:
  - the first resume reactivates the captured fiber,
  - the second resume fails with an explicit `AlreadyResumed` error.
- Dropping a continuation now follows ownership state explicitly:
  - dropping a still-suspended continuation tears down its suspended-fiber ownership,
  - dropping a resumed continuation only frees the continuation record,
  - zort does not discard the resumed fiber after ownership has already moved back into scheduler/control state.
- `reperform` now exists as a distinct search rule:
  - it skips handlers on the current fiber,
  - it resumes handler search at the parent-fiber chain,
  - it still captures the current fiber into a new one-shot continuation.
- `perform` with no matching handler fails with an explicit `UnhandledEffect` error.
- Callback boundaries are explicit:
  - entering a callback boundary saves the current parent link,
  - parent traversal is cleared while the callback runs,
  - effect search and backtrace walking both stop at that boundary,
  - the saved parent link is restored when the callback exits.
- `captureBacktrace` now walks managed frames across the parent-fiber chain instead of only reporting the current fiber.
- `captureContinuationBacktrace` now inspects a suspended continuation's managed stack plus its parent-fiber chain without resuming it.
- Runtime-owned control-state setup is now explicit:
  - callers push handlers, frames, frame roots, and callback boundaries through `Runtime`,
  - `Runtime.controlKernel()` is now the read-only inspection seam rather than the default mutation API,
  - raw mutable `ControlKernel` access is reserved for internal/runtime tests and other deliberate escape hatches.
- External primitive/API entrypoints now use the same callback-boundary rule:
  - `PrimitiveRegistry.callWithBoundary(...)` enters and exits the callback boundary explicitly,
  - the compatibility shim routes exported primitive calls through that mediated path,
  - a primitive that performs an effect therefore sees only handlers installed inside that callback-owned chain.
- Fibers now also move through explicit per-domain scheduler lanes:
  - one active `current` fiber per domain,
  - a runnable queue,
  - a parked queue used for explicit suspension/wakeup policy,
  - and a scheduler-owned suspended queue for fibers captured into one-shot continuations.
- Scheduler lanes now also expose explicit coordination state:
  - atomic queue counters,
  - an atomic current-fiber mirror,
  - an atomic wake-request flag that cross-domain runnable transfer can consume without peeking into queue internals,
  - and an atomic owner token so future worker loops can claim exclusive mutation rights per domain lane.
- The runtime now exposes cross-domain runnable transfer as an explicit capability:
  - runnable fibers can be moved between attached domains with running workers,
  - transfer updates scheduler ownership and the fiber's semantic domain together,
  - transfer does not choose balancing policy; userland decides when to call it.
- Fibers are migratable by default at the runtime layer:
  - continuation resume may rebind a suspended fiber to a new domain,
  - runnable transfer may move a queued fiber to a new domain lane,
  - any pinned or domain-affine policy belongs in userland rather than the core runtime contract.
- Scheduler-owned fibers are now collector-visible through an explicit `fiber_scheduler` root provider:
  - parked fibers keep their managed-stack roots alive without routing through `RootRegistry`,
  - runnable/current fibers use the same ownership seam,
  - fibers captured by effects stay scheduler-owned through the suspended lane,
  - while `suspended_continuations` owns only the captured payload/roots/suspended-stack snapshot.
- The runtime now exposes explicit stop-the-world hooks around collection:
  - STW requests with a target participant count,
  - per-domain safepoint acknowledgements,
  - and world-resume events.
- Control-kernel activity is now observable through typed events carrying:
  - action kind
  - effect id
  - optional site id
  - fiber handle
  - continuation handle
  - handler fiber/index
  - parent-fiber depth
- Bench runs can surface those events with `--trace-effects`.
- This is intentionally a semantic control-state model, not a direct mirror of OCaml's raw stack chunk and assembly-switching implementation.
- The remaining control-kernel work is behavioral:
  - userland scheduling policy on top of the new transfer capability,
  - real parallel stop-the-world/safepoint handshakes instead of the current single-threaded coordination scaffold,
  - richer backtrace integration beyond managed-frame walking,
  - lower-level stack/runtime switching mechanics if zort chooses to model them explicitly.
