---
name: riot-ml
description: Use when helping users build, test, benchmark, run, and maintain OCaml projects with riot. This skill routes to the best Riot workflow, prefers machine-readable `--json` output, and applies package/dependency conventions.
---

# Riot project user guide

## Use this skill
Use this skill when the user wants to:
- create/build/test/bench/run a project with `riot`
- understand how to add dependencies
- use community package documentation
- troubleshoot common Riot project workflows

## User workflow
1. Confirm the project type and intent (`build`, `test`, `bench`, `run`, or maintenance).
2. Use the default commands first:
   - `riot build`
   - `riot test`
   - `riot bench`
   - `riot run`
3. Prefer machine-readable flow whenever available by adding `--json`.
4. If dependency or package behavior is unclear, check the official docs:
   - `docs.riot.ml` for command and ecosystem guidance
   - `docs.pkg.ml/p/<pkg>/<version>/` for package docs
5. If the request shifts toward contributing to Riot internals, switch to contributor routing and read the AGENTS index.

## Common Riot commands
- `riot build --json` : compile packages and dependencies.
- `riot test --json` : run project tests.
- `riot bench --json` : run benchmarks.
- `riot run --json` : run a target package/binary.
- `riot upgrade` : upgrade the Riot binary.
- `riot fmt [--check] --json` : check or apply formatting.
- `riot fix [--check] --json` : run linting/fixes.
- `riot check --json` : perform consistency checks.
- `riot info --json` : inspect workspace/package metadata.

## Ecosystem anchors
- `riot.ml`: website and high-level entrypoint.
- `docs.riot.ml`: documentation portal.
- `pkgs.ml`: package registry used by dependency workflows.

## Contributor-mode fallback
When user asks for Riot internals, architecture, or package-level coding changes, read contributor instructions first.

- [Riot AGENTS map for contributors](references/riot-agents-index.md)
