---
title: The Standard Library
description: What std is for and why it is a major part of the Riot stack.
---

`std` is Riot's standard library, but it is not trying to be a tiny core plus a
long list of future decisions you have to make yourself.

It is meant to cover most of what you need to build actual applications and
systems.

## What std is for

The design goal is to make application-level OCaml feel cohesive. That means
`std` tries to cover a large amount of everyday surface area in one place:

- core datatypes and collections
- iterators and utilities
- dates, time, and timers
- filesystems and processes
- networking
- JSON, TOML, CSV, and other data formats
- logging and telemetry
- configuration
- actor supervision and runtime-facing pieces
- cryptography and encodings
- testing and benchmarking support

The landing page frames this as "~80% of everything you'll ever need". The
point is not the exact percentage. The point is that Riot wants to give you a
clear base to build from.

## Why it matters

`std` is foundational because it gives the rest of the stack a consistent shape:

- similar performance characteristics
- similar concurrency assumptions
- a shared operational vocabulary
- fewer ad hoc choices for every new project

In other words, `std` is part of the reason Riot feels like a stack rather than
just a command plus a few packages.

## Related RFDs

- [RFD0005 Kernel and Std Snapshot](/rfds/rfd0005-kernel-and-std-snapshot/)
- [RFD0029 Std Archive and Compression](/rfds/rfd0029-std-archive-and-compression/)
