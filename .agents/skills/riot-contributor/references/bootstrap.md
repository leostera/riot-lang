# Bootstrap And Miniriot

Use this reference when touching `bootstrap.py`, `packages/miniriot`, toolchain provisioning, or anything that must work before the normal `riot` binary exists.

## Mental Model

The bootstrap path has two stages:

1. `bootstrap.py` gets the repository to a minimal working builder.
2. `./miniriot` uses that builder to compile enough of the repository to produce the real `riot-cli`.

Treat bootstrap as intentionally narrower than the steady-state `riot build` system.

## Run The Bootstrap

From the repository root:

```sh
./bootstrap.py
./miniriot
```

`bootstrap.py`:

- detects the host triple;
- ensures an OCaml toolchain under `~/.riot/toolchains/<version>/<host-triple>`;
- recreates `_build/bootstrap`;
- generates `const.ml`;
- copies the `packages/miniriot/src` bootstrap source set;
- compiles `./miniriot` with direct `ocamlopt`.

The default OCaml version comes from `OCAML_VERSION`, falling back to the value in `bootstrap.py`. The CDN base can be overridden with `RIOT_OCAML_CDN_URL`.

## Debugging Bootstrap Failures

Start by identifying which stage failed:

- toolchain download/provisioning: inspect `bootstrap.py`, `OCAML_VERSION`, `RIOT_OCAML_CDN_URL`, and `~/.riot/toolchains/...`;
- compiling `miniriot`: inspect `_build/bootstrap/sandbox/miniriot` and the direct `ocamlopt` command printed by `bootstrap.py`;
- package dependency planning: inspect `packages/miniriot/src/dep_graph.ml` and generated `_build/bootstrap/out/<pkg>/graph.dot`;
- compiler/linker actions: inspect `packages/miniriot/src/action.ml` and `packages/miniriot/src/ocaml_platform.ml`;
- package metadata: inspect `packages/miniriot/src/package.ml` and the package's `riot.toml`.

`miniriot` prints each package as it builds it and writes a dependency graph DOT file at:

```text
_build/bootstrap/out/<pkg>/graph.dot
```

Use that graph to debug missing module edges, wrong package order, or source scanning errors.

## Miniriot Boundaries

`miniriot` is a bootstrap-only builder, not the modern Riot build system.

It intentionally has:

- a smaller package model than `riot-model`;
- a simpler source scanner and dependency graph than `riot-planner`;
- direct filesystem and process helpers;
- direct compiler command execution through `ocaml_platform.ml`;
- a forward-only `Build_results` handoff between already-built packages.

When fixing bootstrap, keep the implementation boring and direct. Normal Riot runtime, planner, store, and package-manager behavior belong in the steady-state build system unless the bootstrap design is intentionally changing.

## When To Update Bootstrap

Update bootstrap/miniriot when:

- a package required to build `riot-cli` changes its manifest or dependency shape;
- source scanning or module dependency extraction changes in a way bootstrap must mirror;
- compiler flags, C stubs, or native link flags become required earlier in the build chain;
- the OCaml toolchain layout or version contract changes;
- the first real `riot-cli` can no longer be produced from the existing bootstrap set.

If a change only affects normal workspace builds after `riot-cli` exists, it probably belongs in the `riot-*` packages.

## Read More

- `docs/rfds/RFD0002-riot-bootstrap.md` for the current bootstrap design snapshot.
- `packages/miniriot/README.md` for package scope.
- `bootstrap.py` for stage-1 mechanics.
