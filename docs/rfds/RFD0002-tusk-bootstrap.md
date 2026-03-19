# RFD0002 - Tusk Bootstrap Process

- Feature Name: `tusk_bootstrap_process`
- Start Date: `2026-03-19`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD documents the bootstrap process for `tusk`. It explains how the repository gets from source code to a first working `tusk` binary without requiring `tusk` itself to already exist. The bootstrap path consists of `bootstrap.py`, the generated bootstrap sandbox under `_build/bootstrap`, and the standalone `minitusk` builder that compiles the real `tusk-cli`.

## Motivation
[motivation]: #motivation

The build system is self-hosted, which means it needs an explicit bootstrap path.

That bootstrap path has its own architecture and constraints:

- it cannot depend on the full mainline `tusk` runtime
- it must be able to start from a plain OCaml toolchain
- it has to construct enough package/dependency/build logic to compile the first real `tusk`
- it uses a smaller, separate implementation in `packages/minitusk`

This is a different problem from documenting how steady-state `tusk build` works after `tusk` is already installed.

The purpose of this RFD is to capture the bootstrap system on its own terms:

- how `bootstrap.py` finds or installs a toolchain
- how it materializes a standalone `minitusk`
- how `minitusk` scans packages, plans work, and executes builds
- how outputs from early packages become inputs for later packages
- how the first working `tusk-cli` binary is produced

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The bootstrap process has two stages.

### Stage 1: Build `minitusk`

`bootstrap.py` is the external bootstrap script.

It:

1. determines the host platform
2. ensures an OCaml toolchain exists under `~/.tusk/toolchains`
3. creates a bootstrap sandbox under `_build/bootstrap`
4. generates a `const.ml` file that tells `minitusk` where its toolchain is
5. copies the `minitusk` source files into the bootstrap sandbox
6. compiles those files directly with `ocamlopt` into `./minitusk`

### Stage 2: Use `minitusk` to build `tusk`

Once `./minitusk` exists, it becomes the build tool.

`minitusk`:

1. reads package manifests
2. scans package source trees
3. computes a package-local dependency graph
4. turns that graph into a bootstrap build plan
5. executes compiler and filesystem actions
6. promotes the resulting outputs
7. carries those outputs forward so later packages can depend on earlier ones

Eventually, that sequence builds `tusk-cli`, which can then be promoted and used as the real `tusk`.

### Bootstrap chain

```mermaid
flowchart TD
  A[bootstrap.py] --> B[detect host triple]
  B --> C[ensure OCaml toolchain]
  C --> D[generate const.ml]
  D --> E[copy minitusk sources]
  E --> F[compile ./minitusk]
  F --> G[run ./minitusk]
  G --> H[build tusk-cli]
  H --> I[promote or install tusk]
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. `bootstrap.py`

The entrypoint is the top-level `bootstrap.py` script.

It is responsible for bootstrapping from an ordinary host environment without `tusk`.

### 1.1 Host detection

`bootstrap.py` determines:

- operating system
- machine architecture
- libc flavor on Linux (`gnu` vs `musl`)

From those values it computes a host triple such as:

- `aarch64-apple-darwin`
- `x86_64-apple-darwin`
- `x86_64-unknown-linux-gnu`
- `aarch64-unknown-linux-musl`

### 1.2 Toolchain provisioning

The script then ensures that an OCaml toolchain exists under:

```text
~/.tusk/toolchains/<version>/<host-triple>
```

The current default version is taken from `OCAML_VERSION`, falling back to `5.5.0`.

Provisioning works in this order:

1. if `bin/ocamlopt.opt` already exists, reuse the toolchain
2. otherwise try downloading a prebuilt tarball from `https://cdn.riot.ml/ocaml/`
3. if download fails, build OCaml from source using `riot-ocaml`

That makes `bootstrap.py` responsible both for bootstrapping the builder and for bootstrapping the compiler used by the builder.

### 1.3 Bootstrap sandbox creation

After the toolchain is available, `bootstrap.py`:

1. removes `./_build/bootstrap`
2. creates `./_build/bootstrap/sandbox/minitusk`
3. writes a generated `const.ml` file into that directory

The generated `const.ml` contains:

- filename suffix constants
- the current host triple
- the OCaml version
- the computed toolchain root
- the toolchain `bin` directory
- the toolchain `lib/ocaml` directory

This file is what allows the copied `minitusk` sources to be compiled and run as a standalone bootstrap tool.

### 1.4 Source materialization

`bootstrap.py` copies the bootstrap source set into the sandbox:

- `io.ml`
- `ocaml_platform.ml`
- `toml.ml`
- `file_scanner.ml`
- `graph.ml`
- `package.ml`
- `dep_graph.ml`
- `action.ml`
- `main.ml`

Together with generated `const.ml`, this forms the complete bootstrap program.

### 1.5 Direct compilation of `minitusk`

`bootstrap.py` compiles `minitusk` with a direct `ocamlopt` invocation using the toolchain it just provisioned.

It links against `unix.cmxa` and emits:

```text
./_build/bootstrap/sandbox/minitusk/minitusk
```

That binary is then copied to the repository root as:

```text
./minitusk
```

### 1.6 `bootstrap.py` control flow

```mermaid
flowchart TD
  A[start bootstrap.py] --> B[detect host triple]
  B --> C{toolchain exists?}
  C -->|yes| D[reuse toolchain]
  C -->|no| E[download prebuilt toolchain]
  E -->|fail| F[build toolchain from source]
  E -->|ok| D
  F --> D
  D --> G[recreate _build/bootstrap]
  G --> H[write const.ml]
  H --> I[copy minitusk sources]
  I --> J[compile minitusk with ocamlopt]
  J --> K[copy binary to ./minitusk]
```

## 2. `minitusk`

`minitusk` is the standalone bootstrap builder.

Its job is not to expose the full `tusk` feature set. Its job is to build enough of the workspace to produce the first real `tusk-cli`.

### 2.1 Package build order

The build order is hardcoded in `packages/minitusk/src/main.ml`.

The sequence currently includes:

- `kernel`
- `miniriot`
- `std`
- support packages
- the `tusk-*` build packages
- finally `tusk-cli`

This means bootstrap correctness depends on a manually maintained topological sequence, rather than on a general workspace planner.

### 2.2 Bootstrap package model

`packages/minitusk/src/package.ml` reads a package's `tusk.toml` and extracts only the fields bootstrap needs:

- package name
- package path
- dependencies
- binaries
- whether the package uses `stdlib`
- whether it uses `unix`
- whether it uses `dynlink`
- target-specific `cc_flags`
- target-specific `ld_flags`

This bootstrap package model is intentionally smaller than `tusk-model.Package.t`.

### 2.3 File scanning

`packages/minitusk/src/file_scanner.ml` walks directory trees and builds a simple file tree representation.

That file tree is used as the basis for bootstrap dependency analysis.

### 2.4 Dependency graph construction

`packages/minitusk/src/dep_graph.ml` builds a package-local module dependency graph.

Important features of this layer:

- module names are normalized and namespaced
- generated files are represented in the graph
- `ocamldep` is used to discover OCaml module dependencies
- cross-package dependencies are modeled through `Build_results`

This graph is specific to bootstrap and is not the same structure used by `tusk-planner` in the mainline build system.

### 2.5 `Build_results`

`Build_results` is one of the key bootstrap mechanisms.

It records, for each package that has already been built:

- the package's module/archive name
- the output files produced by that package
- transitive `cc_flags`
- transitive `ld_flags`
- whether the package requires `stdlib`
- whether the package requires `unix`
- whether the package requires `dynlink`

Later package builds use this registry to:

- copy already-built artifacts into their sandbox
- inherit link flags and compile flags
- know whether `stdlib`, `unix`, or `dynlink` need to be added transitively

This is how bootstrap threads build products forward from earlier packages to later packages.

### 2.6 Bootstrap action language

`packages/minitusk/src/action.ml` defines the action language used by bootstrap plans.

The main actions are:

- `WriteFile`
- `CopyFile`
- `CompileInterface`
- `CompileImplementation`
- `CompileC`
- `CreateArchive`
- `CreateExecutable`
- `SetPermissions`

This is a deliberately small action set, but it is enough to build the packages required for `tusk-cli`.

### 2.7 Toolchain and command execution

`packages/minitusk/src/ocaml_platform.ml` wraps direct compiler invocations.

It knows how to:

- locate the bootstrap `ocamlc.opt`
- locate `ocamldep.opt`
- compile interfaces
- compile implementations
- generate interfaces
- compile C sources
- build archives
- link executables

`packages/minitusk/src/io.ml` provides the filesystem and process helpers used by these actions:

- read and write files
- create directories
- copy files
- run shell commands
- collect command output

So the bootstrap executor is intentionally direct: it shells out to the bootstrap toolchain and manipulates files explicitly.

### 2.8 Bootstrap package build lifecycle

For each package in the hardcoded sequence, `minitusk`:

1. reads the package manifest
2. builds a package dependency graph
3. prints the file tree for debugging
4. dumps a DOT graph to `_build/bootstrap/out/<pkg>/graph.dot`
5. lowers the dependency graph into a build plan
6. executes the build plan
7. promotes outputs
8. registers the package's outputs in `Build_results`

### 2.9 Bootstrap output flow

Bootstrap outputs are promoted under `_build/bootstrap/out/...`.

Those promoted outputs are then copied into later package sandboxes as needed.

This means bootstrap uses a forward-only artifact handoff model:

- build package A
- promote outputs of package A
- register outputs of package A
- when building package B, copy package A outputs into B's sandbox

It is not using the package hash and artifact store model used by mainline `tusk`.

### 2.10 `minitusk` control flow

```mermaid
flowchart TD
  A[minitusk main] --> B[select next package from hardcoded order]
  B --> C[read tusk.toml]
  C --> D[scan files]
  D --> E[build dependency graph]
  E --> F[lower to action plan]
  F --> G[execute actions]
  G --> H[promote outputs]
  H --> I[register in Build_results]
  I --> J{more packages?}
  J -->|yes| B
  J -->|no| K[tusk-cli built]
```

## Drawbacks
[drawbacks]: #drawbacks

- the package build order is hardcoded
- bootstrap package modeling is narrower and separate from the mainline `tusk-model`
- bootstrap uses direct shelling and filesystem operations rather than the richer runtime abstractions used by `tusk`
- bootstrap artifacts are threaded through `Build_results`, which is simple but specialized

## Prior art
[prior-art]: #prior-art

The main prior art for this RFD is the code in:

- `bootstrap.py`
- `packages/minitusk/src/main.ml`
- `packages/minitusk/src/dep_graph.ml`
- `packages/minitusk/src/action.ml`
- `packages/minitusk/src/ocaml_platform.ml`

More generally, this is a classic self-hosting bootstrap arrangement:

- start from a minimal external compiler environment
- compile a reduced internal builder
- use that builder to compile the real tool

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- How closely should `minitusk` track the semantics of the mainline build system?
- How much bootstrap-specific logic should remain hardcoded versus being inferred?
- Should the hardcoded build order eventually be generated from manifests?

## Future possibilities
[future-possibilities]: #future-possibilities

- document the exact contract between `bootstrap.py` and `minitusk`
- simplify the bootstrap builder further
- make the bootstrap package order derived instead of hardcoded
- tighten the relationship between bootstrap outputs and the eventual `tusk` install/promotion flow
