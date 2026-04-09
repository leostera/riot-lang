# Runtime TLA+ Models

This directory contains bounded TLA+ models for runtime-service protocols that
cut across GC, scheduling, callback delivery, and domain coordination.

## Current Models

- `PendingActionDrain.tla`: deterministic pending-action draining for mixed runtime callbacks at scheduler safepoints, blocking transitions, and STW pause acknowledgements.
- `PendingActionDrain.cfg`: tiny TLC smoke config for the pending-action drain slice.

## Source Contracts

- [`../startup-domains-and-signals.md`](../startup-domains-and-signals.md)
- [`../gc-control-and-stats.md`](../gc-control-and-stats.md)

## How To Run TLC

From the repo root:

```sh
timeout 30 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  zort/spec/runtime/PendingActionDrain.tla \
  -config zort/spec/runtime/PendingActionDrain.cfg
```

The smoke config is intentionally bounded with `TLCGet("level") < 7`.

The model treats pending actions abstractly so it covers both pending signal
callbacks and ready finalizers without baking callback policy into the state
machine itself.
