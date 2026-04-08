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
