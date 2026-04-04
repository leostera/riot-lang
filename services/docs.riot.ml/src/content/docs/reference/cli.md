---
title: CLI Overview
description: The Riot CLI surface and the areas it covers.
---

`riot` is the central tool in the stack. Instead of combining a package
manager, a formatter, a linter, a toolchain manager, a build runner, and a
publishing client from separate ecosystems, Riot exposes one CLI for all of it.

## Usage

```text
riot
OCaml build system and package manager

Usage: riot [OPTIONS] [COMMAND]

Options:
  -v, --verbose  Enable verbose output
```

## Areas of responsibility

The command surface groups into a few broad areas:

- workspace and package lifecycle: `init`, `new`, `clean`
- dependency management: `add`, `rm`, `update`, `search`
- build and execution: `build`, `run`, `install`
- quality and feedback loops: `fmt`, `fix`, `test`, `bench`, `snapshots`
- tooling and editor support: `lsp`, `completions`, `doc`
- distribution and lifecycle: `publish`, `login`, `logout`, `upgrade`, `version`
- compiler management: `toolchain`

For detailed command listings, see [Command Surface](/reference/commands/).
