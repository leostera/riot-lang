# RFD0017 - OCaml Cross-Compilation Snapshot

- Feature Name: `ocaml_cross_compilation_snapshot`
- Start Date: `2026-03-23`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD documents the current cross-compilation state carried by Riot's
vendored OCaml fork under `vendor/ocaml`.

The current state is centered on one large fork commit,
`ef81d5fd5 feat(cross): add relocatable cross-compilation tooling`, plus the
supporting documents in:

- `vendor/ocaml/LINEAR.md`
- `vendor/ocaml/CROSS_COMPILE_GUIDE.md`
- `vendor/ocaml/CROSS_MSVC.md`
- `vendor/ocaml/RELOCATABLE.md`
- `vendor/ocaml/SUMMARY.md`

At a high level, the fork currently provides:

- relocatable OCaml compiler installs
- scripted native and cross builds under `vendor/ocaml/cross/`
- Linux glibc and musl target support
- MinGW Windows target scripts
- an exploratory MSVC design document rather than an implemented MSVC pipeline

This RFD is a snapshot, not a proposal. It exists to make the current shape of
the fork explicit before Riot integrates it more deeply into its own bootstrap,
toolchain, and CI flows.

## Motivation
[motivation]: #motivation

Riot now vendors its OCaml fork under `vendor/ocaml`, and that fork carries a
substantial cross-compilation system that is larger than a normal "patch a few
compiler files" story.

Right now the relevant information is split across:

- fork-local implementation scripts in `vendor/ocaml/cross/`
- multiple design and summary documents in the fork root
- one large fork commit that mixes code, scripts, packaging, and CI
- Riot-side bootstrap and toolchain code that is beginning to point at the
  vendored compiler tree

That makes it hard to answer questions like:

- what is actually implemented today
- which target combinations are scripted today
- whether Windows support is real or exploratory
- where relocatability comes from
- how much of this is in the compiler fork versus Riot's own workflows

This RFD exists to capture the current state before more integration work lands.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The current cross-compilation story has two layers:

1. a vendored OCaml fork in `vendor/ocaml`
2. Riot-side tooling that can eventually consume compiler installs produced by
   that fork

The fork is where almost all of the implemented behavior lives today.

### Contributor mental model

The current fork should be understood as:

- an OCaml source tree with a relocatable install model
- a `cross/` directory full of build and packaging scripts
- a matrix of native and cross target configuration files
- optional CI definitions that were moved out of active `.github/workflows` in
  the fork

It should not be understood as:

- a fully integrated Riot build flow yet
- a fully general target-agnostic cross-compilation framework
- an implemented MSVC pipeline

### What "relocatable" means here

The fork is designed to produce compiler installs that can be moved anywhere on
disk as long as they preserve the usual layout:

```text
bin/
lib/ocaml/
```

The important property is:

- the compiler and runtime should resolve `../lib/ocaml` relative to the
  executable location instead of relying only on a fixed absolute install path

That is what makes tarball distribution practical for Riot's toolchain story.

### What the current scripts do

The main user-facing scripts live under `vendor/ocaml/cross/`:

- `build-native.sh`
- `build-cross.sh`
- `build.sh`
- `package.sh`
- `download-toolchains.sh`
- `test-musl.sh`
- `test-relocatable.sh`

The target definitions live in `vendor/ocaml/cross/targets/`.

Today the scripted targets include:

- native macOS ARM64
- native Linux x86_64 glibc
- native Linux ARM64 glibc
- macOS ARM64 to Linux x86_64 glibc
- macOS ARM64 to Linux ARM64 glibc
- macOS ARM64 to Linux x86_64 musl
- macOS ARM64 to Linux ARM64 musl
- Linux x86_64 glibc to Linux ARM64 glibc
- Linux ARM64 glibc to Linux x86_64 glibc
- Linux x86_64 glibc to Linux x86_64 musl
- Linux ARM64 glibc to Linux ARM64 musl
- macOS ARM64 to Windows x86_64 MinGW
- Linux x86_64 glibc to Windows x86_64 MinGW

That means the current implemented Windows story is MinGW-based.
`CROSS_MSVC.md` is an exploration of how an MSVC-targeting path could work, but
it is not the same as saying Riot currently ships MSVC cross-compilers.

### Where Riot itself is today

The Riot repository is beginning to grow a self-contained compiler story around
this vendored fork, but the fork is still the authoritative place for the
cross-compilation implementation itself.

In current terms:

- `vendor/ocaml` is the compiler source and scripted build surface
- Riot bootstrap/toolchain code can be taught to prefer that vendored source
- Riot does not yet have a fully settled end-to-end workflow that exercises all
  of the fork's target combinations from this repository alone

That distinction matters: the compiler fork already contains a lot of behavior,
but Riot has not yet fully absorbed it into its own steady-state workflows.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. The cross-compilation implementation currently lives in one fork commit

The current cross-compilation system in the vendored compiler is primarily the
result of one fork commit:

- `vendor/ocaml@ef81d5fd5`

That commit introduces:

- cross build scripts
- packaging and test helpers
- target definitions
- relocatability documentation
- Windows exploration documentation
- vendored FlexDLL support under `cross/flexdll`
- disabled fork-local GitHub workflow definitions under
  `.github/workflows-disabled`

So the current implementation should be treated as a fork-local subsystem,
not as a small patch series sprinkled through upstream OCaml.

## 2. Relocatability is implemented by source changes, not only scripts

The fork documentation is slightly inconsistent about how small the core
compiler change is.

`cross/README.md` emphasizes a one-line Makefile change, but the current source
tree and the other fork documents show a broader implementation that currently
touches at least these files:

- `Makefile`
- `utils/config.generated.ml.in`
- `utils/config.common.ml.in`
- `runtime/startup_byt.c`
- `configure.ac`

The durable current story is:

- the install layout is made relocatable by compiler/runtime path-resolution
  changes in the OCaml source tree
- the `cross/` scripts are built on top of that relocatable behavior

### 2.1 Runtime and compiler path resolution

The current source confirms:

- `utils/config.common.ml.in` uses `Sys.executable_name`
- `runtime/startup_byt.c` uses `caml_executable_name()`
- both compiler/runtime config paths now work with relative stdlib locations

So the current fork should be described as "relocatable compiler/runtime path
resolution is implemented in source", not merely "packaging scripts arrange
portable directories".

## 3. Current scripted target surface

The target definitions currently present under `vendor/ocaml/cross/targets/`
are:

- `aarch64-apple-darwin.sh`
- `x86_64-unknown-linux-gnu.sh`
- `aarch64-unknown-linux-gnu.sh`
- `aarch64-apple-darwin-x-aarch64-unknown-linux-gnu.sh`
- `aarch64-apple-darwin-x-x86_64-unknown-linux-gnu.sh`
- `aarch64-apple-darwin-x-aarch64-unknown-linux-musl.sh`
- `aarch64-apple-darwin-x-x86_64-unknown-linux-musl.sh`
- `x86_64-unknown-linux-gnu-x-aarch64-unknown-linux-gnu.sh`
- `aarch64-unknown-linux-gnu-x-x86_64-unknown-linux-gnu.sh`
- `x86_64-unknown-linux-gnu-x-x86_64-unknown-linux-musl.sh`
- `aarch64-unknown-linux-gnu-x-aarch64-unknown-linux-musl.sh`
- `aarch64-apple-darwin-x-x86_64-w64-mingw32.sh`
- `x86_64-unknown-linux-gnu-x-x86_64-w64-mingw32.sh`

That means:

- Linux glibc is implemented
- Linux musl is implemented
- MinGW Windows targets are scripted
- MSVC is not represented by target scripts here

## 4. MSVC is exploratory, not implemented

`vendor/ocaml/CROSS_MSVC.md` is a design exploration.

It describes:

- why MSVC would be desirable
- why MinGW is easier today
- why Windows SDK and `cl.exe` make Unix-hosted MSVC cross-compilation hard
- potential approaches involving Clang/LLVM, Wine, or SDK extraction

But the current fork snapshot does not include:

- active MSVC target scripts in `cross/targets/`
- an implemented MSVC build pipeline
- validated MSVC compiler artifacts

So the correct snapshot wording is:

- MinGW Windows targeting is partially scripted
- MSVC cross-compilation remains a design investigation

## 5. Fork-local CI exists, but is intentionally disabled in the fork

The cross-compilation commit moves several workflow files out of the active
`.github/workflows` directory into `.github/workflows-disabled/`.

That means the current fork carries workflow definitions and matrix ideas, but
does not currently rely on the fork repository itself to execute them.

This is important for Riot because it implies:

- Riot can study and reuse the intended CI matrix
- Riot should not assume the fork is already self-testing every target in its
  own GitHub Actions configuration

## 6. Current Riot integration boundary

At the moment, Riot should treat the vendored compiler fork as:

- the source of truth for compiler-side cross-compilation scripts and patches
- not yet the final source of truth for Riot's end-to-end workflow contracts

The integration questions that remain outside this snapshot include:

- how Riot bootstrap should prefer vendored source over downloaded tarballs
- how Riot CI should build, cache, and publish cross compilers
- whether Riot keeps using prebuilt toolchain tarballs, local vendored builds,
  or both
- how MinGW support should appear in Riot user-facing commands
- whether MSVC becomes a real supported target or remains documentation only

## Drawbacks
[drawbacks]: #drawbacks

The current state has several rough edges:

- the implementation is concentrated in a large fork commit rather than a
  smaller patch stack
- the fork docs are internally inconsistent in a few places about how minimal
  the relocatability change really is
- Linux and Windows cross-target support live together in the fork, but Riot
  has not yet turned that into one coherent local workflow here
- MSVC documentation exists next to MinGW implementation, which can easily
  overstate the actual supported surface if read too quickly

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This snapshot does not choose a future integration strategy. It only records
the current state faithfully.

The main alternative would be to skip documenting the fork until Riot's own
bootstrap and CI integration are complete. That would make the eventual design
look cleaner, but it would hide the substantial cross-compilation work that
already exists and make follow-up decisions harder to reason about.

## Prior art
[prior-art]: #prior-art

The primary prior art for this snapshot is the vendored OCaml fork itself:

- `vendor/ocaml/LINEAR.md`
- `vendor/ocaml/CROSS_COMPILE_GUIDE.md`
- `vendor/ocaml/CROSS_MSVC.md`
- `vendor/ocaml/RELOCATABLE.md`
- `vendor/ocaml/SUMMARY.md`
- `vendor/ocaml/cross/README.md`
- `vendor/ocaml@ef81d5fd5`

Within Riot, the closest related document is:

- `RFD0009-tusk-toolchain-system-snapshot.md`

That RFD explains how Riot chooses and runs toolchains today, while this RFD
captures the current compiler-fork side of the cross-compilation story.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- Should Riot treat `vendor/ocaml` as the primary source build path for
  toolchains, or only as a development/debugging path behind prebuilt tarballs?
- Should Riot absorb the fork's disabled workflow matrix into its own CI, or
  keep the compiler build/publish pipeline elsewhere?
- Should MinGW targets become first-class Riot targets, or are they only
  intermediate experiments on the path to another Windows story?
- Does Riot want to carry an actual MSVC implementation, or only keep the
  exploration document around for future reference?
