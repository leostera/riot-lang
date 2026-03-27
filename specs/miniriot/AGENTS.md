# miniriot spec AGENTS

This directory contains the readable formal model for `packages/miniriot`.

## Source Map

- `packages/miniriot/src/scheduler.ml`: receive semantics, timer handling, lifecycle transitions, worker queue behavior
- `packages/miniriot/src/process.ml`: mailbox/save-queue semantics, process state, links, and monitors
- `packages/miniriot/src/timer.ml` and `packages/miniriot/src/timer_wheel.ml`: timer identity, expiration, and interval rescheduling
- `packages/miniriot/tests/*.ml`: executable expectations that should stay aligned with the modeled design intent

## Rules

1. Readability wins over cleverness. Use descriptive variable, action, and invariant names even when they are longer.
2. Keep utility operators in helper modules such as `QueueUtils.tla`; keep the main runtime module focused on state, actions, and invariants.
3. Comment every non-obvious variable, action family, and invariant with the production behavior it corresponds to.
4. Prefer named TLA+ actions over one monolithic PlusCal algorithm unless PlusCal is clearly easier to read for that specific subsystem.
5. Make fairness and liveness choices explicit in comments. If a property is intentionally only checked as safety in TLC, say so.
6. When a modeled bug is fixed in the runtime, update `README.md` so it distinguishes historical findings from still-open issues.

## Validate

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/miniriot/MiniriotRuntime.tla \
  -config specs/miniriot/MiniriotRuntime.cfg
```

Keep the default config small. Add extra `.cfg` files when investigating one narrow behavior in depth.
