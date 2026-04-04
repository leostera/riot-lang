# riot-model

Shared workspace, package, lockfile, and build types for Riot.

`riot-model` is the vocabulary package for the build system. If two Riot tools
need to agree on what a package is, what a workspace looks like, how a lockfile
is shaped, or what events build and package-management flows emit, that shared
shape belongs here.

## Should you use it directly?

Yes, if you are building Riot tooling.

Probably not, if you are just building an application. End-user code usually
interacts with `riot-cli`, `riot-build`, or `riot-deps`, which already depend
on this package.

## Install

```sh
riot add riot-model
```

## What lives here

- workspace and package manifests;
- lockfile and dependency entry shapes;
- build targets, profiles, actions, and events;
- user config and registry configuration vocabulary shared by multiple tools.

## Why it matters

Keeping these types in one package prevents every tool from inventing its own
slightly different idea of the same domain. That is what lets planning,
execution, publishing, and editor tooling compose cleanly.

## Start here

- `src/Riot_model.mli` is the package entrypoint.
- `src/package.mli`, `src/lockfile.mli`, and `src/event.mli` are the most
  useful files to read first.
