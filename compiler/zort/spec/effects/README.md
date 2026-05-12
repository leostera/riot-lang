# Effects TLA+ Models

This directory contains bounded TLA+ models for zort's control-kernel behavior.

These models focus on the protocol-level rules that are easiest to get wrong:

- one-shot continuation ownership,
- callback-boundary visibility,
- cross-domain resume,
- and where handler search begins for `perform` versus `reperform`.

The callback-boundary rules modeled here are shared by:

- pending signal/finalizer delivery checkpoints,
- external primitive/API entrypoints routed through `PrimitiveRegistry.callWithBoundary(...)`.

## Current Models

- `Continuations.tla`: one-shot resumable continuations, callback boundaries, and cross-domain resume.
- `Continuations.cfg`: tiny TLC smoke config for the continuation slice.

## Source Contracts

- [`../effects-and-continuations.md`](../effects-and-continuations.md)
- [`../exceptions-callbacks-and-backtraces.md`](../exceptions-callbacks-and-backtraces.md)

## How To Run TLC

From the repo root:

```sh
timeout 30 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  zort/spec/effects/Continuations.tla \
  -config zort/spec/effects/Continuations.cfg
```

The smoke config is intentionally bounded with `TLCGet("level") < 7`.
