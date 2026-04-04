---
title: Introduction
description: What Riot is, what it includes, and how the stack is divided.
---

Riot is an opinionated OCaml stack for building applications and systems. It is
designed around one coherent workflow instead of a pile of loosely connected
tools.

At a high level, Riot is four things:

1. `riot`, a single CLI for package management, builds, testing, formatting,
   linting, toolchains, publishing, and upgrades.
2. A multicore-ready actor-model runtime for long-running applications.
3. `std`, a batteries-included standard library for real systems work.
4. `pkgs.ml`, the package registry and distribution surface for Riot packages.

Riot is intentionally integrated. The goal is not to present a menu of
interchangeable defaults. The goal is to give you a workflow from creating a
workspace to publishing a package without first assembling your own stack.

## The central idea

Riot leans on a few strong opinions:

- one tool instead of many wrappers
- one lockfile story through `riot.lock`
- one package story through `pkgs.ml`
- one runtime model centered on actors, messages, and supervision
- one standard library with a clear systems-programming bias

That is why the landing page describes Riot as "my stack". It is not trying to
be every possible OCaml workflow. It is trying to be a cohesive one.

## What these docs cover

This site documents the Riot stack itself:

- how to install Riot and get moving quickly
- how the `riot` CLI is organized
- how package management, lockfiles, and publishing work
- how the registry is split across `pkgs.ml`, `api.pkgs.ml`, and `cdn.pkgs.ml`
- how the runtime, `std`, and the stack fit together

This site does **not** host generated package documentation. Package docs will
be served separately from `docs.pkgs.ml`.

## Related surfaces

- `riot.ml`: the landing page for Riot
- `pkgs.ml`: registry UI, stats, activity, and account management
- `api.pkgs.ml`: registry control-plane API
- `cdn.pkgs.ml`: sparse index and immutable artifact downloads
- `docs.pkgs.ml`: generated package documentation surface

## Read next

- [Installation](/start-here/installation/) for the install and upgrade flow
- [Quickstart](/start-here/quickstart/) for the shortest path to a running app
- [CLI Overview](/reference/cli/) for the shape of the tool
