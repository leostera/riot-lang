# miniriot spec AGENTS

This directory contains the readable formal model for `packages/miniriot`.

## Source Map

- `packages/miniriot/src/scheduler.ml`: receive semantics, timer handling, lifecycle transitions, worker queue behavior
- `packages/miniriot/src/process.ml`: mailbox/save-queue semantics, process state, links, and monitors
- `packages/miniriot/src/timer.ml` and `packages/miniriot/src/timer_wheel.ml`: timer identity, expiration, and interval rescheduling
- `packages/miniriot/tests/*.ml`: executable expectations that should stay aligned with the modeled design intent

## Rules

1. Readability wins over cleverness. Use descriptive variable, action, and invariant names even when they are longer.
2. Keep the current full-runtime model as the integration spec for scheduler/mailbox/timer/lifecycle interactions. Add slice specs for local semantic laws such as receive timeouts, timer identity, or exit cleanup.
3. Keep utility operators in helper modules such as `QueueUtils.tla`; keep each spec focused on one state machine and its properties.
4. Comment every non-obvious variable, action family, invariant, and temporal property with the production behavior it corresponds to.
5. Prefer named TLA+ actions over one monolithic PlusCal algorithm unless PlusCal is clearly easier to read for that specific subsystem.
6. Give every constant an `ASSUME`, especially model values and sentinel values like `NoTimer` or `NoSelector`, so slice specs stay self-explanatory.
7. Separate `TypeOK`/bounds invariants from behavioral laws. Transition restrictions should usually become action properties or temporal properties instead of being forced into state invariants.
8. Use decomposed variables for mutable process state. Keep message payloads and other immutable data as records or tuples.
9. Keep safety and liveness checks in different configs. Liveness configs should be smaller and more explicit about fairness assumptions.
10. When exploring large spaces, document any `TLCGet("level")` constraints or other temporary cutoffs in the local README and avoid treating them as proofs.
11. Use auxiliary variables only for history, debugging, or bounding a model. Do not let the modeled machine semantics depend on them.
12. When slicing around a known bug, keep the slice in its historical semantics first and add a dedicated `*Bug.cfg` that still fails until the model is intentionally updated.
13. Keep passing smoke configs small and separate from failing bug configs so it stays obvious whether TLC just parsed the model or actually reproduced the bug.
14. When a modeled bug is fixed in the runtime, update `README.md` so it distinguishes historical findings from still-open issues.

## Validate

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/miniriot/MiniriotRuntime.tla \
  -config specs/miniriot/MiniriotRuntime.cfg
```

Keep the default config small. Add extra `.cfg` files when investigating one narrow behavior in depth.
