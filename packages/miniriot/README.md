# miniriot

The original bootstrap build tool that helped get Riot off the ground.

`miniriot` is a historical, intentionally minimal build system that predates
the full Riot toolchain. Its job is to build enough of the repository in the
right order to bootstrap the real toolchain.

This is not the package most users want. It is mainly useful if you are
studying Riot's bootstrapping story or maintaining the bootstrap path itself.

## Install

```sh
riot add miniriot
```

## What it includes

`miniriot` contains the small pieces needed for that bootstrap flow:

- package discovery and manifest reading;
- dependency-graph scanning over source files;
- build-plan construction and action execution;
- basic OCaml platform/toolchain helpers;
- a simple bootstrap-oriented CLI entrypoint.

## What it is not

`miniriot` is not the modern Riot package manager and build system.

It does not aim to provide:

- the full `riot` dependency-management surface;
- the registry client and publishing flow;
- the richer cache, planning, and execution model used by the main toolchain.

## When to read it

Reach for this package when you want to understand:

- how Riot originally bootstrapped itself;
- the minimal moving parts required to compile the repo in dependency order;
- the earlier design tradeoffs before the current `riot-*` toolchain packages
  took over.

For normal package and workspace builds, use `riot` itself instead.
