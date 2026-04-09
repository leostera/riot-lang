# Signals, Alternate Signal Stacks, and Stack Overflow

## Source anchors

- `vendor/ocaml/runtime/signals.c`
- `vendor/ocaml/runtime/signals_nat.c`
- `vendor/ocaml/runtime/fail_nat.c`
- `vendor/ocaml/runtime/win32.c`
- `vendor/ocaml/runtime/amd64.S`
- `vendor/ocaml/runtime/arm64.S`
- `vendor/ocaml/runtime/riscv.S`
- `vendor/ocaml/runtime/power.S`
- `vendor/ocaml/runtime/s390x.S`

## Signal-number translation

- The runtime exposes conversions between OCaml-facing negative signal numbers and platform `signo` values.
- Unknown values round-trip unchanged.
- Installing a handler for an unavailable signal raises `Invalid_argument("Sys.signal: unavailable signal")`.

## Alternate signal stack behavior

- On POSIX builds, the runtime allocates an alternate signal stack for domain 0.
- The signal-stack memory is allocated with `malloc`/`mmap` directly, not `caml_stat_alloc_noexc`.
- The source comment makes the policy explicit:
  - leaking is preferred over freeing the wrong alternate stack and causing undefined behavior
- During teardown, the runtime:
  - disables the current alternate stack
  - checks whether some other component installed its own signal stack
  - restores that foreign signal stack if needed
- Existing `SIGPROF` handlers are upgraded to `SA_ONSTACK` if necessary.

## OCaml signal-handler installation

- Runtime-installed signal handlers use `SA_ONSTACK` on POSIX.
- The three observable action classes are:
  - default
  - ignore
  - handle via OCaml callback
- Installing an OCaml handler initializes the handler table lazily and registers it as a global root.
- Low-level POSIX handlers do not run OCaml code directly. They record the signal and preserve `errno`.

## Native polling path

- In native code, `caml_garbage_collection` is the assembly-only poll/GC entrypoint.
- The source explicitly forbids calling it from ordinary C code.
- When the current frame encodes zero allocations, the entry behaves as a poll and just processes pending actions.
- Otherwise it reconstructs the combined allocation size from frame descriptors and dispatches allocation work.

## Native raise path interactions

- `caml_raise` performs runtime cleanup before unwinding:
  - channel cleanup for locked channels
  - pending-action processing with the exception bucket kept alive as a root
  - trimming of C local-root frames above the current C-stack boundary
- If no C stack exists, the runtime:
  - terminates signal handling
  - reports a fatal uncaught exception

## OCaml stack overflow

- Native OCaml stack overflow is part of the normal runtime control path.
- Assembly/runtime stack-growth failure raises the predefined `Stack_overflow` exception directly.
- This is the overflow path used by native OCaml stacks/fibers when the managed OCaml stack cannot be expanded further.

## Windows system-stack overflow

- Windows has an additional native path for system stack overflow.
- If `EXCEPTION_STACK_OVERFLOW` occurs while executing inside an OCaml code fragment:
  - the runtime switches to a small alternate stack
  - restores the guard-page protection on the faulting stack page
  - raises `Stack_overflow`
- On x86_64 Windows, the handler also refreshes `young_ptr` from saved registers before jumping to the recovery helper.

## Scope note

- In the inspected native sources, explicit signal-based recovery for system stack overflow appears in `win32.c`.
- On Unix-native paths, the inspected sources show:
  - alternate signal-stack management for signal delivery
  - managed OCaml-stack overflow raised from the normal stack-growth failure path
- That distinction is an inference from the inspected sources, not a claim about every historical OCaml runtime variant.

## zort takeaways

- zort should distinguish:
  - OCaml managed-stack overflow
  - OS thread-stack overflow
  - ordinary deferred signal delivery
- Alternate signal-stack ownership is part of runtime policy, not just platform glue.
- A credible native replacement runtime needs an explicit story for:
  - signal-stack setup/teardown
  - signal recording vs callback execution
  - overflow recovery behavior by platform

## zort baseline status

- `RuntimeServices` in `src/runtime_services.zig` now owns:
  - pending-signal recording as an explicit bitset,
  - blocking-section depth as explicit runtime service state,
  - a runtime-local signal-handler table rooted through the collector seam,
  - process-global native signal-ingress installation claimed by one runtime at a time,
  - and owned alternate signal-stack setup / restore / teardown records.
- Signal handlers are now explicit runtime-local values:
  - `registerSignalHandler` stores a handler per signal number,
  - `deliverPendingActions` drains pending bits and delivers those handlers through callback boundaries.
- Native signal ingress is now wired on Unix-like targets:
  - `installSignalIngress` installs a POSIX handler that records the signal and preserves the runtime's pending-signal bitset semantics,
  - `enableAlternateSignalStack` installs a runtime-owned `sigaltstack`,
  - teardown disables zort's alternate stack before attempting to restore a foreign prior stack,
  - if the prior foreign stack cannot be restored because the platform rejects it, zort leaves signal delivery disabled rather than re-entering undefined behavior.
- `ControlKernel` in `src/control_kernel.zig` now owns managed-stack limits:
  - frame-count overflow,
  - frame-root overflow,
  - explicit managed-stack growth from configured initial capacities up to configured maxima,
  - typed `StackOverflow` errors once that managed-stack growth policy reaches its configured max.
- This is still not full signal/runtime parity:
  - zort's signal ingress is Unix-first and intentionally bounded,
  - pending actions still drain at explicit runtime checkpoints instead of trying to run signal callbacks eagerly,
  - native thread-stack overflow recovery remains platform work for later.
