---
title: Runtime and Stack
description: High-level view of the Riot runtime, toolchain, and stack boundaries.
---

Riot is more than a CLI. The stack is designed around a few opinions that fit
together:

- the runtime favors actors and message passing
- the toolchain favors one CLI over many loosely connected tools
- the standard library favors application and systems work over minimalism
- the registry favors artifact-native publishing and a split between control and read planes

These pieces are meant to reinforce each other instead of competing for control.

## Runtime model

At the runtime level, Riot is closest in spirit to Erlang and Elixir:

- processes are long-lived
- communication happens via messages
- supervision matters
- failure is expected, not treated as exceptional architecture

The runtime is multicore-ready and built around message passing and cheap
spawning. The intended programming model is not "threads plus a few utilities".
It is applications built as communicating processes.

## A quick mental model

If OCaml gives you the type system and language core, Riot tries to answer a
different question: how should a real long-running system be structured so it
remains understandable, observable, and resilient when things go wrong?

Riot's answer is:

- actors
- messages
- supervisors
- explicit failure handling

That makes it closer in spirit to Erlang and Elixir than to the usual OCaml
application story, while still staying rooted in OCaml's strengths.

## Toolchain model

At the tool level, Riot tries to reduce orchestration overhead:

- one command surface
- one workflow
- one lockfile story
- one publication path

## Stack boundaries

Riot the stack includes:

- the `riot` CLI
- the runtime and actor model
- `std`
- `pkgs.ml`

It does not try to be a drop-in layer over the traditional OCaml toolchain. It
is a different, more vertically integrated workflow.

## Related RFDs

- [RFD0004 Actors Runtime Snapshot](/rfds/rfd0004-actors-runtime-snapshot/)
- [RFD0010 Actors Multicore Work-Stealing Runtime](/rfds/rfd0010-actors-multicore-work-stealing-runtime/)
- [RFD0011 Actors Pinned and Blocking Spawn](/rfds/rfd0011-actors-pinned-and-blocking-spawn/)

## Docs boundaries

This site documents the stack itself. It is the right place for:

- CLI behavior
- registry architecture
- install and upgrade flows
- runtime and stack-level concepts

This site is not the right place for generated package docs. Those belong on
`docs.pkgs.ml`.
