# Exceptions, Callbacks, Effects Boundary, and Backtraces

## Source anchors

- `vendor/ocaml/runtime/fail.c`
- `vendor/ocaml/runtime/fiber.c`
- `vendor/ocaml/runtime/callback.c`
- `vendor/ocaml/runtime/backtrace.c`
- `vendor/ocaml/runtime/backtrace_nat.c`
- `vendor/ocaml/runtime/printexc.c`
- `vendor/ocaml/runtime/caml/mlvalues.h`

## Exception representation

- Exception constructors are `Object_tag` blocks.
- Raising from C is usually a two-step process:
  - build an exception value / bucket
  - call `caml_raise(...)`
- Buckets are ordinary tag-`0` blocks whose first field is the constructor and later fields are arguments.
- Convenience helpers exist for:
  - constant exceptions
  - one arg / N args
  - string payloads
  - common runtime errors such as `Invalid_argument`, `Out_of_memory`, `Stack_overflow`, `End_of_file`, `Not_found`, array bounds, and blocked I/O

## `caml_result`

- The runtime now exposes `caml_result` as the typed “value-or-exception” carrier.
- Older encoded-exception-in-`value` tricks still exist for compatibility but are explicitly marked unsafe.

## Callback semantics

- C-to-OCaml callbacks come in `_exn` and raising forms.
- The runtime tries not to extend argument liveness longer than OCaml call semantics require.
- Callbacks preserve arguments across helper allocations only when necessary.

## Effect boundary behavior

- The detailed stack-switching model lives in [`effects-and-continuations.md`](./effects-and-continuations.md).
- The callback-facing rule is narrower and observable:
  - before a callback, the runtime may clear the current stack parent and wrap it in a continuation value
  - this forces unhandled effects to surface as `Effect.Unhandled` instead of crossing the C callback boundary implicitly
  - after the callback returns, the stack parent is restored

## Backtraces

- Backtrace recording is runtime-managed state.
- Enabling/disabling backtraces resets the saved exception/backtrace state.
- Raw backtraces can be copied out to OCaml values and restored later.
- Printing a backtrace depends on debug info availability.
- In native code, callstack capture includes parent fibers.
- Continuation callstack capture temporarily consumes the continuation, inspects its suspended stack, then restores it.

## Uncaught exception printing

- The runtime formats exceptions using constructor names plus simple rendering of primitive payloads.
- It runs `Pervasives.do_at_exit` before printing the fatal uncaught exception message.
- If backtraces are active and a debugger is not intercepting, it prints the exception backtrace.
- Process termination is:
  - `abort()` when `caml_abort_on_uncaught_exn` is set
  - otherwise `exit(2)`

## zort takeaways

- A small, maintainable zort runtime should keep “exception value building” separate from “non-local control transfer”.
- Effects crossing C boundaries are a real semantic issue, not an implementation footnote.
- If zort wants a typed replacement API, `caml_result` is a better reference point than the older encoded exception convention.

## zort callback and backtrace baseline

- `ControlKernel.enterCallbackBoundary` / `exitCallbackBoundary` now model callback boundaries explicitly in `src/control_kernel.zig`.
- `Runtime.deliverPendingActions(...)` now uses those callback boundaries when delivering:
  - pending signal handlers from `RuntimeServices`,
  - ready finalizer callbacks from `ManagedLiveness`.
- `PrimitiveRegistry.callWithBoundary(...)` now uses the same callback-boundary entry/exit protocol for external primitive dispatch.
- The compatibility shim in `src/caml_compat/api.zig` routes exported `zort_primitive_call*` entrypoints through that mediated dispatch path instead of calling primitives naked.
- `Runtime` now also supports configured checkpoint-driven delivery:
  - scheduler safepoints,
  - blocking entry,
  - blocking exit,
  - and STW pause acknowledgement all enter the same callback-boundary delivery path.
- Entering a callback boundary:
  - saves the current parent-fiber link,
  - clears parent traversal for the duration of the callback,
  - prevents implicit upward effect search past that boundary.
- Observable consequence:
  - `perform` inside the callback fails with `UnhandledEffect` if no handler exists inside the callback-owned chain,
  - `captureBacktrace` only reports managed frames reachable inside that boundary.
- zort backtrace capture is currently semantic and managed-stack-based:
  - it walks `site_id` frames recorded on fibers,
  - it follows parent-fiber links when they are visible,
  - it can inspect suspended continuations through `captureContinuationBacktrace`,
  - it does not yet mirror OCaml's native frame-descriptor machinery.
