# Package Boundaries

Use this reference when deciding where a new API, fix, or abstraction belongs.

## Core Stack

The repository layers the core runtime roughly like this:

```text
kernel -> std -> higher packages and applications
```

Applications and most packages should reach for `Std`, not `Kernel`, unless they are implementing low-level foundations.

## Kernel

Put behavior in `kernel` when it is a low-level platform or runtime primitive:

- file descriptors, OS handles, system calls, polling, timers, host/system information;
- off-heap buffers, `IoSlice`, `IoVec`, and syscall-facing byte storage;
- OS randomness and entropy sources;
- primitive synchronization used below the actor/runtime layer;
- direct platform/native edges and narrow native stubs.

Keep kernel APIs narrow, mechanical, checked by default, and explicit about copy boundaries. Public surfaces expose portable handles; platform-native handles like `Unix.file_descr` stay inside backend implementation code. Policy, ergonomics, and broad composition usually belong above kernel.

## Runtime And Actor Semantics

Runtime and actor-facing behavior lives under `std`, especially `std/src/runtime` and the public `Std.Process`, `Std.Agent`, `Std.Supervisor`, and related surfaces.

Put behavior in the runtime area when it changes scheduler, process, mailbox, timer, receive, supervision, or runtime semantics. Actor protocols should be explicit. Prefer named message payload records over positional encodings.

## Std

Put behavior in `std` when it is a shared ergonomic surface for the rest of the repo:

- `Path`, `Fs`, `IO.Reader`, `IO.Writer`, and higher-level file helpers;
- collections, iterators, string builders, data formats, config, telemetry, testing, benchmarks;
- process-facing APIs built on actors;
- application startup, worker pools, agents, supervisors, and shared runtime services.

Add behavior to `std` when it is genuinely shared across packages. Changes here have high blast radius, so prefer additive evolution and stable signatures.

Inside `std`, prefer `open Global`. Outside `kernel` and runtime internals, most Riot code should use `open Std`; direct `Stdlib`, `Unix`, `Sys`, or `Obj` access belongs only where the local package explicitly owns that boundary.

## Riot Command Packages

Keep `riot-cli` thin. It parses command-line flags, resolves workspace context, delegates to command packages, and renders events.

Domain logic belongs in the command/package that owns it:

- formatting logic in `krasny`, with `riot-fmt` as the wrapper;
- fix/lint orchestration in `riot-fix`, shared rule types in `fixme`;
- workspace/package models in `riot-model`;
- dependency solving and package management in `riot-deps`, `pubgrub`, and `pkgs-ml`;
- build planning in `riot-planner`;
- build execution in `riot-build`;
- artifact storage in `riot-store` and generic content-addressed storage in `contentstore`;
- toolchain invocation in `riot-toolchain`;
- benchmark history in `riot-bench`.

If a CLI command starts duplicating planner, executor, registry, formatter, or test-runner behavior, move that logic down to the owning package.

## Syntax And Formatting

- `syn` owns parsing, syntax tree construction, AST views, diagnostics, and dependency extraction.
- `krasny` owns formatting and layout policy.
- Downstream packages should consume semantic `Syn.Ast` views and helpers.

When a formatter or analyzer needs more structure, prefer exposing a better typed view in `syn` over reconstructing source text in the consumer.

## Boundary Questions

Ask these before placing code:

- Is this a syscall/platform primitive? Put it in `kernel`.
- Is this scheduler/process behavior? Put it in `std` runtime or the relevant public `Std` actor-facing surface.
- Is this a shared ergonomic library API used broadly? Consider `std`.
- Is this specific to build planning, dependency solving, formatting, fixing, publishing, or running? Put it in the owning `riot-*` package.
- Is this only useful to one package? Keep it local.
- Does this need to work before `riot` exists? Consider `bootstrap.py` / `miniriot`, and keep it bootstrap-specific.
