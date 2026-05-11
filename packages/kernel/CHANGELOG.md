# Changelog

All notable changes to `kernel` are documented here.

## 0.0.29 - 2026-05-01

### Changed

- Blocking file and socket IO now copies OCaml heap-backed buffers before entering blocking sections, and copies read data back after returning. This prevents the GC from moving heap buffers while native read/write calls are in progress.

## 0.0.26 - 2026-04-28

### Changed

- Kernel gained a queue surface for FIFO work management in lower-level runtime code.
- Async readiness handling is more complete across pipes, timers, UDP, TCP, processes, deregistration, duplicate registration, mixed event sources, closed sources, and invalid polling limits.
- File and filesystem operations now report more precise typed errors for missing paths, dangling symlinks, invalid file kinds, invalid read/write slices, directory removal failures, copy/rename behavior, and link metadata.
- IO buffer, IoVec, IoSlice, process, environment, monotonic time, and system time paths were tightened so low-level runtime APIs preserve byte ranges, timestamps, and OS error context more reliably.

## 0.0.21 - 2026-04-23

### Added

- Added the Linux implementation path for the new kernel async backend, including epoll/timerfd process, pipe, timer, TCP, and UDP readiness support.
- Added `Kernel.Thread.sleep_ns` for low-level blocking sleeps used by polling paths that must not depend on the actor scheduler.

## 0.0.20 - 2026-04-22

### Changed

- Preserved `IoVec` module casing correctly across `kernel` / `std`, fixing the broken alias/module naming path that was surfacing in kernel builds and planner probes.

## 0.0.19 - 2026-04-22

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
