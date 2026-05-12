# Domains TLA+ Models

This directory contains bounded TLA+ models for zort's domain and scheduler capabilities.

The current scope is intentionally capability-level:

- claimed scheduler lanes,
- runnable/current/parked/suspended ownership,
- and explicit cross-domain runnable transfer.

It does not model balancing or work-stealing policy. That remains userland policy.

## Current Models

- `RunnableTransfer.tla`: claimed lane ownership and explicit cross-domain runnable transfer.
- `RunnableTransfer.cfg`: tiny TLC smoke config for the transfer slice.

## Source Contracts

- [`../startup-domains-and-signals.md`](../startup-domains-and-signals.md)
- [`../effects-and-continuations.md`](../effects-and-continuations.md)

## How To Run TLC

From the repo root:

```sh
timeout 30 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  zort/spec/domains/RunnableTransfer.tla \
  -config zort/spec/domains/RunnableTransfer.cfg
```

The smoke config is intentionally bounded with `TLCGet("level") < 7`.
