# Startup, Shutdown, Domains, and Signals

## Source anchors

- `vendor/ocaml/runtime/startup_aux.c`
- `vendor/ocaml/runtime/startup_nat.c`
- `vendor/ocaml/runtime/domain.c`
- `vendor/ocaml/runtime/signals.c`
- `vendor/ocaml/runtime/signals_nat.c`
- `vendor/ocaml/runtime/sys.c`

## Startup and shutdown

- `caml_startup` is reference-counted:
  - the first call performs real initialization
  - later calls are ignored
- `caml_shutdown` is also reference-counted and is only effective on the final matching call.
- Calling `caml_startup` after shutdown is fatal.
- Calling `caml_shutdown` without a matching startup is fatal.

## Native startup sequence

- Native startup performs, in order:
  - parse runtime parameters
  - optional pool setup
  - code fragment init
  - locale init
  - custom-ops registration
  - OS parameter init
  - GC init
  - runtime-events init
  - code-segment registration
  - signal init
  - sys/process init
  - stack expansion
  - entry into the program closure
- Native execution after startup relies on runtime-managed OCaml stack chunks, frame descriptors, and assembly entry points for GC polling, callback transitions, and effect stack switching.

## Domain model

- Domains are explicit runtime participants in stop-the-world coordination.
- STW sections are used for at least:
  - minor GC
  - major GC phase changes
- Domains entering blocking sections hand STW servicing to backup threads.
- Newly spawning domains are blocked from mutator execution during active STW sections.

## Signal model

- Pending signals are stored in a bitset.
- Signal delivery records the signal and interrupts all domains so handling happens promptly.
- Signal handlers run as OCaml callbacks, not directly in the POSIX signal context.
- Entering a blocking section processes pending actions first when possible.
- Leaving a blocking section may force pending-signal handling even if another thread previously cleared the action flag.
- In native code, the GC/signal polling entry point (`caml_garbage_collection`) is only valid from generated assembly and depends on the current OCaml stack/fiber layout plus frame descriptors.

## Exit behavior

- `caml_do_exit` may print GC stats depending on verbosity settings.
- Exit tears down runtime events.
- If `cleanup_on_exit` is enabled, shutdown runs automatically on exit.
- Shutdown calls registered OCaml shutdown hooks such as `Pervasives.do_at_exit` and `Thread.at_shutdown`.

## zort takeaways

- A maintainable replacement runtime should treat “startup” as a composition of services, not a single monolith.
- Domain coordination and signal delivery are core runtime policy, not optional utilities.
- If zort initially stays single-threaded, it should say so and isolate the future STW/domain surface instead of baking concurrency assumptions into unrelated APIs.

## zort runtime-services baseline

Executable model:

- [`domains/RunnableTransfer.tla`](./domains/RunnableTransfer.tla)
- [`domains/README.md`](./domains/README.md)
- [`runtime/PendingActionDrain.tla`](./runtime/PendingActionDrain.tla)
- [`runtime/README.md`](./runtime/README.md)

- `RuntimeServices` now exists as a separate subsystem in `src/runtime_services.zig`.
- `DomainRegistry` now exists as a separate subsystem in `src/domain_registry.zig`.
- The current service model includes:
  - reference-counted startup/shutdown state,
  - pending-signal recording,
  - blocking-section depth,
  - string-keyed named values rooted through the collector's `RootProvider` seam,
  - runtime-local signal handlers delivered through explicit callback-boundary entry,
  - native signal-ingress installation claimed by one runtime at a time,
  - and explicit alternate signal-stack ownership with restore records.
- Pending actions now drain through explicit runtime checkpoints rather than only through ad hoc manual delivery:
  - scheduler safepoints after current-fiber activation/switch,
  - blocking-section entry before the domain becomes blocked,
  - blocking-section exit after the domain becomes attached again,
  - stop-the-world pause acknowledgements.
- zort's drain path is now lossless under delivery failure:
  - pending signals are cleared only after successful delivery,
  - ready finalizers are acknowledged one by one after successful delivery,
  - a failed callback leaves the action pending for retry instead of dropping it silently.
- zort's mixed pending-action path is now test-locked for blocking transitions:
  - blocking entry drains both signals and ready finalizers at `.blocking_enter`,
  - failed mixed delivery leaves both the signal bit and ready finalizer pending,
  - retry drains each action exactly once.
- The current domain model includes:
  - a main attached domain created at runtime startup,
  - a main worker bootstrapped at runtime startup with an explicit owner token,
  - explicit domain creation plus attach/detach lifecycle,
  - per-domain blocking depth and blocked/attached state transitions,
  - explicit worker lifecycle states (`stopped`, `running`, `stopping`) separate from attach/detach,
  - fibers and suspended continuations carrying domain ownership in the control kernel,
  - per-domain scheduler lanes with explicit current/runnable/parked fiber states,
  - per-domain scheduler coordination snapshots with atomic wake flags, queue counters, and claimable owner tokens,
  - a stop-the-world coordinator with explicit request/acknowledge/resume hooks,
  - per-domain STW acknowledgement slots keyed by domain handle,
  - and STW coordination snapshots with atomic active/generation/target/paused-count mirrors.
- This is an intentional simplification of OCaml's runtime-global model:
  - services are attached to one `Runtime` instance,
  - named values are runtime-local rather than process-global,
  - named-value and signal-handler mutation is serialized by a runtime-local mutex,
  - scheduler lanes and STW coordination now expose shared-memory-safe atomic coordination state,
  - STW pause acknowledgements can arrive independently per registered domain,
  - worker lifecycle is now explicit at the runtime layer,
  - and scheduler queue mutation now requires a claimed lane owner token on every mutable scheduler path.
- zort's intended split is now explicit:
  - runtime owns capabilities such as domain workers, lane claims, cross-domain resume, and runnable transfer,
  - userland owns balancing policy such as work stealing, fairness, and actor placement.
- zort now explicitly allows a suspended fiber to resume in a different attached domain:
  - the resumed fiber adopts the resumer's active domain,
  - this is the current migration seam for multicore fibers,
  - fibers are migratable by default at the runtime layer,
  - any domain-affine or pinned placement semantics are now userland policy rather than core runtime behavior,
  - backup-thread/STW servicing still remain future work.
- The important architectural change is that startup/signal/native-service state now has a home outside the semantic value and collector core.
