---
title: Installation
description: Install Riot and understand what the installer sets up.
---

Install Riot with the hosted installer:

```sh
curl -sSL https://get.riot.ml | sh
```

The installer fetches the Riot binary and places it in Riot's managed paths.
Once installed, `riot` becomes the entrypoint for the rest of the stack.

## Verify the install

Install Riot with:

```sh
riot version
riot --help
```

You should see the top-level command list, including `add`, `build`, `fmt`,
`fix`, `publish`, `toolchain`, and `upgrade`.

## What Riot manages

Riot is not just a binary wrapper. It manages:

- the `riot` CLI itself
- your OCaml toolchains through `ocaml-toolchain.toml`
- dependency resolution and lockfiles through `riot.lock`
- package publishing credentials for `pkgs.ml`

## After install

Create a workspace and build it:

```sh
riot init hello
cd hello
riot build
```

Or try a remote starter:

```sh
riot run leostera/create-riot-app
```

## Upgrades

Upgrade the globally installed Riot binary with:

```sh
riot upgrade
```

You can also request a specific version:

```sh
riot upgrade --version <version>
```

## Authentication for publishing

When you are ready to publish packages to `pkgs.ml`, save a publish token with:

```sh
riot login
```

The token is created and managed through your `pkgs.ml` account.

## Toolchains

Riot's toolchain story is driven by `ocaml-toolchain.toml`. That file describes
the OCaml version and targets your workspace needs. Riot then installs missing
toolchains and uses them during build and run flows.

See [CLI Overview](/reference/cli/) and [Runtime and Stack](/architecture/runtime/)
for the bigger picture, or jump to [Quickstart](/start-here/quickstart/) for
the shortest end-to-end workflow.
