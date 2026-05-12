# GC TLA+ Models

This directory contains small executable protocol models for zort's GC behavior.

These models are intentionally semantic and bounded:

- they model roots, remembered edges, domain acknowledgements, and collection phases,
- they do not try to encode concrete heap slots, object headers, or Zig allocator details.

## Current Models

- `GenerationalGC.tla`: nursery/major behavior, remembered-set coverage, and STW-gated collection phases.
- `GenerationalGC.cfg`: tiny TLC smoke config for the collector slice.

## Source Contracts

- [`../gc-strategy.md`](../gc-strategy.md)
- [`../gc-roots.md`](../gc-roots.md)

## How To Run TLC

From the repo root:

```sh
timeout 30 java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  zort/spec/gc/GenerationalGC.tla \
  -config zort/spec/gc/GenerationalGC.cfg
```

The smoke config is intentionally bounded with `TLCGet("level") < 7`.
