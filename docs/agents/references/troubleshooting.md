# Troubleshooting

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
