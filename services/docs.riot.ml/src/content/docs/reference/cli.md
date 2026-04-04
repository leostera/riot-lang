---
title: CLI Overview
description: The Riot CLI surface and the areas it covers.
---

`riot` is the center of the stack. Instead of combining a package manager, a
formatter, a linter, a toolchain manager, a build runner, and a publishing
client from separate ecosystems, Riot exposes one CLI for all of it.

Operationally, Riot is a one-shot tool. Each command discovers the workspace,
performs the requested work, streams output, and exits. There is no required
daemon or background RPC service on the core path.

## Usage

```text
riot
OCaml build system and package manager

Usage: riot [OPTIONS] [COMMAND]

Options:
  -v, --verbose  Enable verbose output
```

## What Riot owns

Riot tries to own the full development loop:

- workspace creation
- dependency management
- lockfile refresh
- build planning and execution
- running binaries, tests, and benchmarks
- formatting and linting
- documentation generation
- package publishing
- OCaml toolchain installation
- upgrades of the Riot binary itself

This is the practical meaning of "a single tool" on the landing page.

## Areas of responsibility

The command surface groups into a few broad areas:

- workspace and package lifecycle: `init`, `new`, `clean`
- dependency management: `add`, `rm`, `update`, `search`
- build and execution: `build`, `run`, `install`
- quality and feedback loops: `fmt`, `fix`, `test`, `bench`, `snapshots`
- tooling and editor support: `lsp`, `completions`, `doc`
- distribution and lifecycle: `publish`, `login`, `logout`, `upgrade`, `version`
- compiler management: `toolchain`

## Core files

The CLI revolves around a few important files:

- `riot.toml`: workspace and package manifests
- `riot.lock`: the exact resolved dependency graph
- `ocaml-toolchain.toml`: OCaml version and target configuration

Riot reads and writes these directly as part of normal workflow. In particular,
`riot.lock` is a real part of the contract, not an optional cache file.

## Command behavior

Riot commands are designed to be:

- workspace-aware
- non-interactive by default where possible
- scriptable
- machine-readable for agents and tooling

Some commands support structured output directly, such as:

- `riot build --json`
- `riot fmt --json`
- `riot fix --json`
- `riot add --json`
- `riot search --json`

See [JSON and Agents](/reference/json-and-agents/) for the machine-facing side
of the CLI, and [Command Reference](/reference/commands/) for the full command
surface.

## Related RFDs

- [RFD0001](/rfds/rfd0001-simplify-riot/) for the one-shot tool model
- [RFD0026](/rfds/rfd0026-riot-package-management/) for package-management behavior
- [RFD0027](/rfds/rfd0027-toolchain-manifest/) for toolchain manifest rules
