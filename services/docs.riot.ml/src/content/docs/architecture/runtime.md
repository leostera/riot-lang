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

## Runtime model

At the runtime level, Riot is closest in spirit to Erlang and Elixir:

- processes are long-lived
- communication happens via messages
- supervision matters
- failure is expected, not treated as exceptional architecture

## Toolchain model

At the tool level, Riot tries to reduce orchestration overhead:

- one command surface
- one workflow
- one lockfile story
- one publication path

## Docs boundaries

This site documents the stack itself. It is the right place for:

- CLI behavior
- registry architecture
- install and upgrade flows
- runtime and stack-level concepts

This site is not the right place for generated package docs. Those belong on
`docs.pkgs.ml`.
