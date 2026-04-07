# Workspaces and config

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
