---
title: Clanker-Friendly Tooling
description: Why Riot is designed to work well with agents and other machine-driven workflows.
---

Riot is built to be friendly to agents, scripts, and editor tooling.

This does not just mean "it has a CLI". It means the stack tries to expose
machine-usable behavior deliberately.

## The design goals

The broad goals are:

- commands should work non-interactively
- structured output should exist where it matters
- diagnostics should be explainable
- workflows should be easy to automate

## Practical examples

Today that shows up in places like:

- `riot build --json`
- `riot fmt --json`
- `riot fix --json`
- `riot search --json`
- `riot completions --packages`
- `riot completions --commands`

The package registry also provides:

- `pkgs.ml/llms.txt`
- `pkgs.ml/api`
- `api.pkgs.ml` for machine-facing registry actions

## Why Riot cares about this

The project is increasingly built and exercised with agents. That makes
machine-readable command design a product requirement, not an optional extra.

This is also why Riot invests in its own parser, diagnostics, and editor-facing
tooling: good machine output requires owning more of the pipeline.

## Related docs

- [JSON and Agents](/reference/json-and-agents/)
- `https://riot.ml/llms.txt`
- `https://pkgs.ml/llms.txt`

## Related RFDs

- [RFD0007 Riot Fix](/rfds/rfd0007-riot-fix/)
- [RFD0015 Syn Typed CST](/rfds/rfd0015-syn-typed-cst/)
- [RFD0036 LSP Protocol Package and Riot Language Server](/rfds/rfd0036-lsp-protocol-package-and-riot-language-server/)
