---
name: riot
description: Build, configure, run, test, benchmark, format, and troubleshoot projects that use the Riot toolchain. Use when Codex needs to work with `riot.toml`, `.riot/config.toml`, `ocaml-toolchain.toml`, `riot init`, `riot add`, `riot build`, `riot run`, `riot test`, `riot bench`, `riot fmt`, or `riot fix` in a Riot workspace or detached Riot package.
---

# Riot

Use this skill when the project uses Riot as the package manager, build system,
and app workflow.

Prefer Riot-native commands and Riot-native project layout. Do not default to
`dune`, `opam`, or raw `ocamlc` workflows unless the project explicitly says it
is doing something outside the normal Riot flow.

If the repository ships its own local contributor guidance, treat that as
higher priority than this generic user skill.

## Quick start

1. Find the nearest `riot.toml`.
2. Decide whether you are in a workspace root or a detached single-package root.
3. Use the narrowest Riot command that answers the task.
4. Read the matching reference file only when needed.

## Workflow map

### Scaffold or inspect a project

- Use `riot init [path]` to create a new workspace.
- Use `riot new packages/<name>` to add another package to an existing
  workspace.
- Read [references/workspaces.md](references/workspaces.md) for the workspace,
  package, and config-file mental model.

### Add or update dependencies

- Use `riot add <package>`.
- Use `riot rm <package>`.
- Use `riot update` or `riot update <package>`.
- Prefer changing Riot manifests over introducing a second dependency workflow.

### Build or typecheck

- Use `riot build` for the full workspace.
- Use `riot build <package>` when the task is package-local.
- Use `riot check` when the task is primarily about typing rather than a full
  build.
- Read [references/commands.md](references/commands.md) for command patterns.

### Run a binary

- Use `riot run <binary>` when the workspace has a single obvious binary or the
  target name is unambiguous.
- Use `riot run -p <package> <binary>` when you need package disambiguation.
- Do not guess binary paths under `_build`; let Riot resolve the runnable.

### Run tests or benchmarks

- Use `riot test` for the default test pass.
- Use `riot test <query>` to filter cases by name.
- Use `riot test <package:suite>` to narrow suite discovery.
- Use `riot bench` for benchmarks.
- Read [references/testing.md](references/testing.md) for selector and policy
  details.

### Troubleshoot

- First check root detection, selector shape, and build-lane assumptions.
- Then check config file placement and target-dir assumptions.
- Read [references/troubleshooting.md](references/troubleshooting.md).

## Core rules

- Treat Riot as the source of truth for build, run, test, bench, formatting,
  and dependency management.
- Do not assume `_build`; respect `[riot].target_dir` when the workspace sets
  one.
- Default to the `debug` profile unless the user explicitly asks for another
  profile such as `--release`.
- A package-local `riot.toml` with `[package]` can still be a valid build root.
  Riot does not require every task to start at a workspace root.
- Keep project semantics in `riot.toml`.
- Keep repository-local operational behavior in `.riot/config.toml`.
- Keep user-local settings such as registry auth in `~/.riot/config.toml`.

## Command selection rules

- If the user asks whether one package builds, start with `riot build <package>`
  before `riot build`.
- If the user asks about one binary, prefer `riot run -p <package> <binary>`
  over guessing an artifact path.
- If the user asks about one failing suite, prefer `riot test <package:suite>`
  before a workspace-wide `riot test`.
- If the user asks for machine-readable output, prefer Riot commands with
  `--json` instead of scraping pretty text.
- If the task is formatting or linting, use `riot fmt` and `riot fix` before
  reaching for unrelated external tools.

## Testing and selection rules

- Use `-p` or `--package` for package narrowing when the command supports it.
- For `riot test`, `package:suite` narrows suite discovery.
- Plain trailing query text filters test-case names.
- `riot test --small`, `riot test --large`, and `riot test --flaky` partition
  the matched case set.
- Repository-shared test policy lives in `.riot/config.toml` under
  `[riot.test]`, not in `riot.toml`.

## References

- Read [references/workspaces.md](references/workspaces.md) for workspace roots,
  detached packages, manifests, and output directories.
- Read [references/commands.md](references/commands.md) for practical command
  patterns.
- Read [references/testing.md](references/testing.md) for test and benchmark
  workflows.
- Read [references/troubleshooting.md](references/troubleshooting.md) when Riot
  behavior looks surprising.
