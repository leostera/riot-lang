open Std

type file = {
  relative_path: string;
  content: string;
  executable: bool;
}

let riot_skill_files = [
  {
    relative_path = ".agents/skills/riot/SKILL.md";
    content = {|---
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
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/agents/openai.yaml";
    content = {|interface:
  display_name: "Riot"
  short_description: "Build, run, and test Riot projects"
  default_prompt: "Use $riot to create, build, run, test, or troubleshoot a Riot workspace."
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/commands.md";
    content = {|# Command patterns

Use this reference when you know the task and need the right Riot command
shape.

## Scaffold

Create a new workspace:

```sh
riot init
```

Create a new package inside an existing workspace:

```sh
riot new packages/my-package
```

## Dependencies

Add a dependency:

```sh
riot add std
```

Remove a dependency:

```sh
riot rm std
```

Refresh dependencies:

```sh
riot update
riot update std
```

## Build and typecheck

Build the whole workspace:

```sh
riot build
```

Build one package:

```sh
riot build my-package
```

Typecheck the workspace or a package:

```sh
riot check
riot check -p my-package
```

When the task is narrow, prefer package-scoped commands before workspace-wide
ones.

## Run binaries

Run a local binary:

```sh
riot run my-binary
```

Disambiguate by package:

```sh
riot run -p my-package my-binary
```

Forward args after `--`:

```sh
riot run -p my-package my-binary -- --port 8080
```

Riot can also run remote sources:

```sh
riot run leostera/create-riot-app
```

## Tests and benchmarks

Run all tests:

```sh
riot test
```

Filter by test-case name:

```sh
riot test parser
```

Narrow by suite:

```sh
riot test my-package:parser_tests
```

Run only small, large, or flaky cases:

```sh
riot test --small
riot test --large
riot test --flaky
```

Run benchmarks:

```sh
riot bench
riot bench hashmap
```

## Formatting and fixes

Check formatting:

```sh
riot fmt --check
```

Apply or inspect fixes:

```sh
riot fix --check .
riot fix --apply .
```

## Machine-readable output

Use `--json` when a machine-readable stream is better than scraping human
output:

```sh
riot build --json
riot test --json
riot bench --json
riot fmt --check --json
```
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/testing.md";
    content = {|# Testing and benchmarking

Use this reference when the task involves `riot test`, `riot bench`, suite
selection, or repository-shared test policy.

## Mental model

At the top level, `riot test` and `riot bench` are Riot commands that:

1. build the needed packages once
2. discover suite binaries
3. run those suite binaries through their machine-readable contracts
4. aggregate the results

That means the right user workflow is usually:

- narrow by package or suite first
- then narrow by query if needed
- use `--json` when tooling needs structured results

## Test selection

These are different selectors:

- `riot test`
  Runs the default test set.
- `riot test <query>`
  Filters test cases by substring.
- `riot test <package:suite>`
  Narrows suite discovery before running cases.
- `riot test --small`
  Runs only cases marked small.
- `riot test --large`
  Runs only cases marked large.
- `riot test --flaky`
  Runs only cases marked flaky.

Do not confuse package or suite selection with case-name selection.

## Shared test policy

Repository-shared test policy lives in `.riot/config.toml`:

```toml
[riot.test]
small_test_timeout = "500ms"
flaky_max_retries = 3
```

Use that file for repository-local policy. Do not put this policy in
`riot.toml`, and do not assume every user should store it in
`~/.riot/config.toml`.

## Advanced note: suite binaries

If you are debugging the generated suite binary directly, Riot test suites
typically expose subcommands such as:

- `list-tests`
- `run-tests [query]`

Benchmark binaries similarly expose:

- `list-benchmarks`
- `run-benchmarks [query]`

Most user tasks should still go through `riot test` or `riot bench` first.

## When to use `--json`

Use `--json` when:

- you need to feed results into tooling
- you need reliable machine-readable timing or status output
- scraping human output would be fragile

Examples:

```sh
riot test --json
riot bench --json
```
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/troubleshooting.md";
    content = {|# Troubleshooting

Use this reference when Riot behavior seems surprising or a command that should
be simple does not behave as expected.

## Root detection problems

Symptoms:

- Riot says it cannot find a workspace
- the wrong package set is being built
- commands behave differently from a parent directory and a package directory

Checks:

- find the nearest `riot.toml`
- decide whether it is a `[workspace]` root or a `[package]` root
- remember that a detached package root is still valid

## Build-path problems

Symptoms:

- a guessed binary path does not exist
- a script assumes `_build/...` but the project uses another directory
- artifacts appear to be missing after a profile or target change

Checks:

- read `[riot].target_dir` from `riot.toml`
- remember that artifacts are lane-scoped by `profile + target`
- prefer `riot run` or `riot build` over path guessing

## Selector problems

Symptoms:

- `riot test` appears to run the wrong set of tests
- a query filters nothing
- package narrowing is confused with case-name filtering

Checks:

- use `-p` or `--package` for package narrowing when supported
- use `package:suite` to narrow suite discovery
- use plain trailing text for a test-case query
- use `--small`, `--large`, and `--flaky` only for case policy filtering

## Config-placement problems

Symptoms:

- a setting does not seem to affect the repository
- one user sees different behavior from another
- test policy is applied inconsistently

Checks:

- project semantics belong in `riot.toml`
- repository-local operational policy belongs in `.riot/config.toml`
- user-local settings belong in `~/.riot/config.toml`

## Toolchain problems

Symptoms:

- behavior differs across machines
- builds break after a toolchain change
- the wrong OCaml version appears to be in use

Checks:

- read `ocaml-toolchain.toml`
- confirm the user is actually using Riot's toolchain flow
- avoid mixing unrelated external setup steps into a normal Riot workflow
|};
    executable = false;
  };
  {
    relative_path = ".agents/skills/riot/references/workspaces.md";
    content = {|# Workspaces and config

Use this reference when you need to decide what kind of Riot project you are in
and which config file owns a behavior.

## Project roots

Riot supports two common roots:

- a workspace root with a `riot.toml` that contains `[workspace]`
- a detached package root with a `riot.toml` that contains `[package]`

Do not assume every Riot project has a multi-package workspace. A single
package can be a valid build root on its own.

## File roles

### `riot.toml`

This is the project semantics file.

Use it for things like:

- workspace membership
- package metadata
- dependencies
- binaries and libraries
- build profiles
- build-path settings such as `[riot].target_dir`

### `.riot/config.toml`

This is repository-local operational policy.

Use it for behavior that should apply to everyone working in the repository but
is not part of the package or workspace semantics.

Current examples include test policy:

```toml
[riot.test]
small_test_timeout = "500ms"
flaky_max_retries = 3
```

### `~/.riot/config.toml`

This is user-local Riot configuration.

Use it for machine- or user-specific state such as registry auth or personal
settings. Do not put repository behavior here unless the user explicitly asks
for a local override.

### `ocaml-toolchain.toml`

This pins the OCaml toolchain Riot should use for the project.

If the user reports toolchain drift, build failures after version changes, or
cross-machine mismatches, read this file.

## Build output layout

By default Riot writes build artifacts under `_build`, but that is only the
default.

If the workspace sets `[riot].target_dir`, use that instead.

The build root is lane-scoped:

```text
<target_dir>/<profile>/<target>/...
```

Important consequences:

- do not hardcode `_build`
- do not assume host-default paths if the workspace or user requested a target
- do not guess executable paths when `riot run` can resolve them for you

## Package layout

The common workspace shape is:

```text
riot.toml
ocaml-toolchain.toml
packages/
  my-package/
    riot.toml
    src/
    tests/
```

`riot init` scaffolds a workspace with:

- a root `riot.toml`
- `ocaml-toolchain.toml`
- a starter package under `packages/<name>/`
- a root `Dockerfile`
- a GitHub Actions workflow at `.github/workflows/ci.yml`

## Default runtime binary

If a package has `src/main.ml` and does not declare explicit `[[bin]]` entries,
Riot can treat that as the default runtime binary for the package.

That means `riot run` may work without extra manifest boilerplate when the
package layout is conventional.
|};
    executable = false;
  };
]

let dev_config_toml = fun ~workspace_name ->
  {|[app]
name = "|} ^ workspace_name ^ {|"
log_level = "info"
|}

let workspace_riot_config_toml = {|
[riot.cache]
keep_generations = 5
max_size = "700 MiB"
|}

let pre_commit_hook = {|
#!/bin/sh
set -eu

riot fmt
riot fix
riot build
riot test --small
|}
