# Changelog

All notable changes to `std` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for structured data parsers, URI/glob/path parsers, scalar parsers, and encoding decoders.

### Fixed

- TOML arrays now reject unexpected tokens instead of recursing indefinitely.
- Glob character classes now handle trailing and malformed ranges without unchecked reads.
- `Log.flush` now returns after a bounded wait if the stdout handler is stale or unavailable instead of blocking forever.
- Snapshot color tests and timer measurement tests were stabilized so they do not race shared environment state or scheduler wakeups.
- Empty version requirements now return a typed parse error instead of reading past the input.

## 0.0.34 - 2026-05-10

### Added

- Added telemetry spans, ordered index sets, and concurrent hash map work backed by the new swisstable package. Runtime mailbox internals also avoid more locking on hot paths.

## 0.0.32 - 2026-05-04

### Changed

- `Std.Test.fuzz` adds the public test-case surface used by `riot fuzz`, including seed replay, corpus metadata, mutator hints, and `run-fuzz-case` execution support inside generated suite binaries.
- Conversion helpers continue moving to explicit `from_*` names, with unchecked or panic-capable conversions named accordingly. This keeps public APIs clearer about whether a value is parsed, converted, or assumed valid.

## 0.0.30 - 2026-05-02

### Changed

- `Std.Log.start_link` now reads `RIOT_LOG` and configures the default log level automatically, removing the need for every application to reimplement the same environment parsing boilerplate.
- `Std.Test` suites now support optional `setup` and `teardown` hooks. Setup failures fail the suite before tests run, while teardown failures are reported after the suite completes.
- The Riot standard library no longer exposes the old `Char.chr` spelling. Use the explicit `from_int` / unchecked conversion APIs instead.

## 0.0.29 - 2026-05-01

### Changed

- `Std.Fs.File.write_string`, `write_all`, and file-backed writers now route string data through `IO.Buffer` and vectored IO. High-level file writes use the same off-heap safe path as the rest of the IO stack while preserving writable retry behavior.

## 0.0.27 - 2026-05-01

### Changed

- Snapshot tests now recreate pending `.expected.new` files on every failing run, even when a pending file already exists. This keeps snapshot failures fresh while iterating.
- Snapshot diffs now prefer colored diff output, making review of expected/actual changes easier in terminals.
- Fixture discovery no longer sorts eagerly when ordering is not semantically relevant, reducing unnecessary latency before tests start running.

## 0.0.26 - 2026-04-28

### Changed

- `Std.Crypto` now exposes HMAC-SHA256 helpers used by Suri session, CSRF, and LiveView signing paths.
- `Std.Http.Status` gained equality helpers for status comparisons.
- `Std.Test` output now reports per-test timings in the human runner while preserving JSON mode for automation.
- `Vector.concat` and `Vector.extend` support efficient vector concatenation without building temporary lists. `extend` mutates the left vector in place, which is useful in hot parser, formatter, and analysis paths.
- `Std.Collections.HashMap` now uses a SwissTable-style backing table for denser storage and faster lookup, insertion, removal, and traversal while preserving the existing public API.
- `Std.Collections.ConcurrentHashMap` now uses single atomic bucket heads plus masked bucket and striped-counter selection, reducing hashing overhead and bucket indirection in core lock-free operations.
- Queue, Deque, HashMap, HashSet, Heap, TypedKeyHashMap, iterator, mutable iterator, IO reader/writer, buffered reader, and Unicode helpers now have tighter semantics around order, mutation, borrowed slices, and invalid input.
- `Std.Command.output` remains safe around inherited stdout/stderr pipes and delayed output, preserving idle callbacks and streamed line callbacks for long-running commands.

## 0.0.25 - 2026-04-27

### Added

- Added `Std.Order.is_lt`, `is_lte`, `is_eq`, `is_gte`, and `is_gt`, so callers can work directly with `Order.t` compare results without converting through integers.

### Changed

- Replaced remaining deprecated list helper usage in downstream packages after the standard collection cleanup.

## 0.0.24 - 2026-04-24

### Changed

- Collapsed concurrent queue variants into a single lock-free queue surface.
- Migrated comparison APIs toward `Std.Order.t` return values across the stack.

## 0.0.20 - 2026-04-22

### Changed

- Preserved `IoVec` module casing correctly across `kernel` / `std`, fixing the broken alias/module naming path that was surfacing in kernel builds and planner probes.

## 0.0.19 - 2026-04-22

### Added

- Added `Std.Collections.Proplist` for duplicate-friendly property-list workflows.

### Changed

- Continued IO/runtime hardening and performance work across `std`, `kernel`, `http`, and `serde-json`, including vectored TLS fallback support and lower-overhead buffer/reader paths.

## 0.0.18 - 2026-04-15

### Added

- Added a dedicated `std` events seam (`feat(kernel): add fs events seam for std`) and absorbed actors/runtime pieces into kernel/std ownership.

### Changed

- Expanded and standardized the new `std` API surface for common project-wide primitives (`path`, `read_dir`, IO, and core runtime entrypoints), with many call sites moved from legacy kernel usage.
- Refactored runtime ownership: moved tar and gzip engines out of kernel into std, and ported FS facade + core runtime/IO onto the smaller kernel model.
- Standardized behavior around bootstrap and runtime correctness (`self-host bootstrap`, sandbox path normalization, valid float-literal restoration, and warning/validation cleanups).

### Removed

- Removed legacy runtime debt by dropping `kernel-old` and completing staged migration checkpoints (`kernel,std`, `workspace`, `typ`) across validation and bootstrap.
