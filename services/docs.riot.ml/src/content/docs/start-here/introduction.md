---
title: Introduction
description: What Riot is, what it includes, and how the stack is divided.
---

Riot is an opinionated OCaml stack centered around four pieces:

1. `riot`, a single CLI for package management, building, testing, formatting, linting, toolchains, and publishing.
2. An actor-model runtime for multicore OCaml applications.
3. A modern standard library aimed at application and systems work.
4. The `pkgs.ml` registry for discovering, downloading, and publishing packages.

Riot is intentionally integrated. The goal is not to present a menu of
interchangeable defaults. The goal is to give you one coherent workflow from
creating a project to publishing a package.

## What these docs cover

This site documents the Riot stack itself:

- how to install Riot
- how the `riot` command surface is organized
- how the registry works at the stack level
- how the runtime and broader stack fit together

This site does **not** host generated package documentation. Package docs will
be served separately from `docs.pkgs.ml`.

## Related surfaces

- `riot.ml`: the landing page for Riot
- `pkgs.ml`: registry UI, stats, activity, and account management
- `api.pkgs.ml`: registry control-plane API
- `cdn.pkgs.ml`: sparse index and immutable artifact downloads
- `docs.pkgs.ml`: generated package documentation surface
