# actors spec AGENTS

This directory contains the readable formal model for the actor runtime now implemented under `packages/std/src/runtime`.

## Source Map

- `packages/std/src/runtime/scheduler*.ml`: receive semantics, timer handling, lifecycle transitions, and worker queue behavior
- `packages/std/src/runtime/process.ml` and `packages/std/src/runtime/proc_state.ml`: process state, links, monitors, and effect handling
- `packages/std/src/runtime/mailbox.ml`: mailbox and save-queue semantics
- `packages/std/src/runtime/timer.ml` and `packages/std/src/runtime/timer_wheel.ml`: timer identity, expiration, and interval rescheduling
- `packages/std/tests/*.ml`: executable expectations that should stay aligned with the modeled design intent

## Rules

1. Readability wins over cleverness. Use descriptive variable, action, and invariant names even when they are longer.
2. Keep the current full-runtime model as the integration spec for scheduler/mailbox/timer/lifecycle interactions. Add slice specs for local semantic laws such as receive timeouts, timer identity, or exit cleanup.
3. Keep utility operators in helper modules such as `QueueUtils.tla`; keep each spec focused on one state machine and its properties.
4. Comment every non-obvious variable, action family, invariant, and temporal property with the production behavior it corresponds to.
5. Prefer named TLA+ actions over one monolithic PlusCal algorithm unless PlusCal is clearly easier to read for that specific subsystem.
6. Give every constant an `ASSUME`, especially model values and sentinel values like `NoTimer` or `NoSelector`, so slice specs stay self-explanatory.
7. Separate `TypeOK`/bounds invariants from behavioral laws. Transition restrictions should usually become action properties or temporal properties.
8. Use decomposed variables for mutable process state. Keep message payloads and other immutable data as records or tuples.
9. Keep safety and liveness checks in different configs. Liveness configs should be smaller and more explicit about fairness assumptions.
10. When exploring large spaces, document any `TLCGet("level")` constraints or other temporary cutoffs in the local README as exploration bounds.
11. Use auxiliary variables only for history, debugging, or bounding a model. Keep modeled machine semantics on primary state.
12. When slicing around a known bug, keep the slice in its historical semantics first and add a dedicated `*Bug.cfg` that still fails until the model is intentionally updated.
13. Keep passing smoke configs small and separate from failing bug configs so it stays obvious whether TLC just parsed the model or actually reproduced the bug.
14. When a modeled bug is fixed in the runtime, update `README.md` so it distinguishes historical findings from still-open issues.

## TLC Checks

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/actors/ActorsRuntime.tla \
  -config specs/actors/ActorsRuntime.cfg
```

Keep the default config small. Add extra `.cfg` files when investigating one narrow behavior in depth.
