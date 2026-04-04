---
title: Installation
description: Install Riot and understand what the installer sets up.
---

Install Riot with:

```sh
curl -sSL https://get.riot.ml | sh
```

The installer is served from `get.riot.ml` and installs the Riot binary into
your local Riot paths.

## After install

Use the CLI directly:

```sh
riot version
riot new hello
cd hello
riot build
```

## Upgrades

Upgrade the globally installed Riot binary with:

```sh
riot upgrade
```

## Authentication for publishing

When you are ready to publish packages to `pkgs.ml`, save a publish token with:

```sh
riot login
```

The token is created and managed through your `pkgs.ml` account.
