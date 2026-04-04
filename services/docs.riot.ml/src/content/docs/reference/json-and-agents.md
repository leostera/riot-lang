---
title: JSON and Agents
description: How Riot exposes machine-readable command output and agent-friendly workflows.
---

Riot is designed to work well with agents, scripts, and editor integrations.
That is not an afterthought. It is part of the stack's design.

## Machine-readable output

Several commands emit structured output today:

- `riot build --json`
- `riot fmt --json`
- `riot fix --json`
- `riot add --json`
- `riot search --json`

The common pattern is:

- human-readable output by default
- structured output when explicitly requested
- non-interactive execution suitable for automation

For example:

```sh
riot build --json
riot fmt --check --json
riot fix --check --json
riot search json --json
```

## Why this matters

Riot is trying to be "clanker-friendly": easy to drive from tools and agents.
In practice that means:

- commands should be callable without TTY-only interaction
- results should be parseable
- diagnostics should have stable identities where possible
- workflows should not require shelling out to five other tools first

## Completion and discovery

The CLI also exposes structured completion-oriented queries through
`riot completions`, including:

- packages
- binaries
- tests
- benchmarks
- package-provided commands

This makes it easier to build editor integrations and command discovery
surfaces without scraping human help text.

## Registry-facing automation

For package publishing and registry queries:

- use `riot login` to save a `pkgs.ml` token
- use `riot publish` for artifact-native package publishing
- use `riot search` for registry search from the CLI
- use `pkgs.ml/llms.txt` and `pkgs.ml/api` for machine and human registry docs

## Where this is heading

The current CLI-driven machine interface is not the final boundary for editor
and agent tooling. Riot is also growing toward a proper language-server story,
so syntax diagnostics, formatting, and code actions can move onto a real
protocol surface instead of living only behind shell command integrations.

## Related surfaces

- `https://riot.ml/llms.txt`
- `https://pkgs.ml/llms.txt`
- `https://pkgs.ml/api`

## Related RFDs

- [RFD0007 Riot Fix](/rfds/rfd0007-riot-fix/)
- [RFD0036 LSP Protocol Package and Riot Language Server](/rfds/rfd0036-lsp-protocol-package-and-riot-language-server/)
