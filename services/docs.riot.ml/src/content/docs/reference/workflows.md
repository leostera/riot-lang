---
title: Common Workflows
description: The day-to-day Riot workflow for creating, building, testing, and publishing packages.
---

This page collects the common paths through the `riot` tool.

## Start a workspace

Create a workspace:

```sh
riot init hello
cd hello
```

Create an additional package inside that workspace:

```sh
riot new app --bin
```

## Add dependencies

Add a registry package:

```sh
riot add minttea
```

Add a version requirement:

```sh
riot add minttea@0.4.2
```

Add a GitHub package:

```sh
riot add github.com/owner/repo
riot add github.com/owner/repo/path/to/pkg#main
```

Add a dependency into a specific section:

```sh
riot add sqlite --dev
riot add suri --build
```

By default, `riot add` updates the relevant manifest and refreshes `riot.lock`.

## Build, test, and run

Build the whole workspace:

```sh
riot build
```

Build a target architecture:

```sh
riot build --target linux
```

Run tests or benchmarks:

```sh
riot test
riot bench
```

Run one binary:

```sh
riot run app
```

Install a binary into `~/.riot/bin` and the project root:

```sh
riot install app
```

## Keep code clean

Format sources:

```sh
riot fmt
```

Check formatting only:

```sh
riot fmt --check
```

Lint and apply safe fixes:

```sh
riot fix --check
riot fix --apply
```

## Work with snapshots

Riot also has a snapshot-review workflow:

```sh
riot snapshots review
riot snapshots approve
riot snapshots reject
```

This sits inside Riot's testing story instead of being a separate tool.

## Manage the toolchain

List the toolchains your workspace wants:

```sh
riot toolchain
```

Riot's toolchain story is driven by `ocaml-toolchain.toml`, including
cross-compilation targets. Missing toolchains can then be installed through the
toolchain command surface.

## Publish to pkgs.ml

Save your publish token:

```sh
riot login
```

Run local publish checks first:

```sh
riot publish --dry-run
```

Then publish:

```sh
riot publish
```

To publish one package from a workspace:

```sh
riot publish -p app
```

The package and registry model is covered in more detail in
[Publishing Packages](/registry/publishing/).

## Related RFDs

- [RFD0009 Testing System Snapshot](/rfds/rfd0009-testing-system-snapshot/)
- [RFD0026 Riot Package Management](/rfds/rfd0026-riot-package-management/)
- [RFD0028 Local Artifact Publishing](/rfds/rfd0028-local-artifact-publishing/)
