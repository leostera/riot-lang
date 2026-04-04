---
title: Quickstart
description: The fastest way to get from install to a running Riot project.
---

If you want the shortest path from zero to a running Riot app, use this flow.

## 1. Install Riot

```sh
curl -sSL https://get.riot.ml | sh
```

## 2. Create a workspace

```sh
riot init hello
cd hello
```

`riot init` creates a workspace. By default it scaffolds a library package,
but it can also create a binary-focused workspace with `--bin`.

## 3. Add or create a package

Inside a workspace, create another package with:

```sh
riot new app --bin
```

Or add a dependency from the registry:

```sh
riot add minttea
```

`riot add` updates the relevant manifest section and refreshes `riot.lock`.

## 4. Build and run

```sh
riot build
riot test
riot run app
```

These commands are workspace-aware. If you omit a package name, Riot operates
across the workspace.

## 5. Format and lint

```sh
riot fmt
riot fix --check
```

`riot fmt` uses the zero-config `krasny` formatter. `riot fix` runs the
extensible linter and can apply safe fixes with `--apply`.

## 6. Publish when ready

```sh
riot login
riot publish --dry-run
riot publish
```

Publishing goes through `pkgs.ml`. Riot packages are published as package-root
artifacts, not as raw repository snapshots.

## A more opinionated starting point

If you want to skip the blank-workspace step and start from a real application
template, run:

```sh
riot run leostera/create-riot-app
```

That is the same "do something real quickly" flow showcased on `riot.ml`.
