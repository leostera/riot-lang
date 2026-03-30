# OCaml Toolchain Releases

This runbook lists the commands needed to rebuild and republish every currently supported OCaml toolchain in this repository.

The source of truth for supported targets is `vendor/ocaml/cross/targets/*.sh`. If that directory changes, update this runbook in the same change.

## Command Model

- `./scripts/toolchain/ocaml.sh build <target>` builds and packages a target into `dist/toolchains/ocaml/<target>/`.
- `./scripts/toolchain/ocaml.sh publish <target>` uploads an existing tarball from `dist/toolchains/ocaml/<target>/` without rebuilding it.
- `./scripts/toolchain/ocaml.sh release <target>` builds, packages, and publishes a target.

`release` is the command to use when you are republishing a toolchain from scratch.

## Release Prerequisites

- Publish credentials must be available through `.env` or the environment. `scripts/toolchain/ocaml.sh` loads `.env` automatically.
- `aws` must be installed for `publish` and `release`.
- `docker buildx` must be installed for the native GNU/Linux host targets:
  - `x86_64-unknown-linux-gnu`
  - `aarch64-unknown-linux-gnu`
- Host-specific C cross toolchains must already be installed as expected by the target scripts under `vendor/ocaml/cross/targets/`.

## Release Order

Release the native host compiler first on each host. Cross targets can reuse the packaged host compiler from `dist/toolchains/ocaml/<host-target>/` and avoid bootstrapping that compiler again.

Distinct targets build in isolated per-target worktrees, so different targets can run in parallel. The same target should not be released in parallel twice.

Local non-Linux-host builds use `/tmp/riot/ocaml/<target>/worktree` by default, so those isolated worktrees stay out of the repository checkout.

## macOS arm64 Host

Run these on an `aarch64-apple-darwin` host.

Release the native host compiler first:

```sh
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin
```

Then release the available macOS arm64 cross-compilers:

```sh
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-aarch64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-aarch64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-w64-mingw32
```

## Linux x86_64 Host

Run these on an `x86_64-unknown-linux-gnu` host.

Release the native host compiler first:

```sh
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu
```

Then release the available Linux x86_64 cross-compilers:

```sh
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-aarch64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-x86_64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-x86_64-w64-mingw32
```

## Linux arm64 Host

Run these on an `aarch64-unknown-linux-gnu` host.

Release the native host compiler first:

```sh
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu
```

Then release the available Linux arm64 cross-compilers:

```sh
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu-x-x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu-x-aarch64-unknown-linux-musl
```

## Full Release Matrix

If you are coordinating a full republish across every supported host, these are the commands that need to run somewhere:

```sh
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-aarch64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-aarch64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release aarch64-apple-darwin-x-x86_64-w64-mingw32
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-aarch64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-x86_64-unknown-linux-musl
./scripts/toolchain/ocaml.sh release x86_64-unknown-linux-gnu-x-x86_64-w64-mingw32
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu-x-x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh release aarch64-unknown-linux-gnu-x-aarch64-unknown-linux-musl
```
