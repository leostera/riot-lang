# Toolchains

Read this when the release touches:
- `vendor/ocaml`
- cross targets under `vendor/ocaml/cross/targets`
- bundled sysroots
- `ocaml-toolchain.toml`
- default toolchain epochs such as `5.5.0-riot.4`
- Linux or MinGW cross-compilation behavior

Start with the inventory helper so you know the current default epoch and target
set before changing anything:

```bash
.agents/skills/riot-release/scripts/release_inventory.py
```

## Source of truth

- `scripts/toolchain/ocaml.sh`
- `scripts/create-sysroot.sh`
- `docker/ocaml-toolchain.Dockerfile`
- `docker/ocaml-toolchain-run.sh`
- `ocaml-toolchain.toml`

## Build or publish toolchains

Examples from the script:

```bash
./scripts/toolchain/ocaml.sh build all
./scripts/toolchain/ocaml.sh build x86_64-unknown-linux-gnu
./scripts/toolchain/ocaml.sh build aarch64-apple-darwin-x-x86_64-unknown-linux-gnu

./scripts/toolchain/ocaml.sh publish riot.4 all
./scripts/toolchain/ocaml.sh release riot.4 all
```

Use:
- `build` to produce local artifacts
- `publish` to upload already built artifacts
- `release` to build and publish in one flow

`all` expands from the vendored OCaml target scripts.

## Docker-backed Linux builds

The toolchain script builds these Linux GNU hosts through Docker Buildx:
- `x86_64-unknown-linux-gnu`
- `aarch64-unknown-linux-gnu`

It also uses Docker when the build host of a cross target is one of those Linux
GNU hosts.

That means you can publish native Linux toolchains from macOS, as long as Docker
is available and the vendored targets are wired correctly.

## Sysroots

Create or refresh a Linux sysroot overlay with:

```bash
scripts/create-sysroot.sh x86_64-unknown-linux-gnu
scripts/create-sysroot.sh aarch64-unknown-linux-gnu 22.04 dist/sysroots
```

Arguments:
1. target triple
2. Ubuntu release, default `22.04`
3. output root, default current directory

The script:
- starts a Dockerized Ubuntu userspace
- installs development libraries
- extracts headers and runtime libs
- rewrites absolute symlinks inside the sysroot
- emits `sysroot-<target>.tar.gz`

When sysroots change, verify that the toolchain packaging path still picks them
up. Do not assume generating the tarball is enough.

## Toolchain release order

For a toolchain-affecting release:

1. bump the toolchain epoch everywhere it is baked in
2. build the relevant toolchains locally
3. validate them with targeted cross builds
4. smoke-test the resulting Riot binaries on Linux
5. publish the toolchains
6. verify the remote manifest

## Cross-target validation

Build Riot CLI or another representative binary with the target toolchain:

```bash
riot build -x x86_64-unknown-linux-gnu -p riot-cli
riot build -x aarch64-unknown-linux-gnu -p riot-cli
```

Then smoke the built Riot binary in Docker:

```bash
scripts/docker-smoke/riot-binary.sh --distro ubuntu --platform linux/amd64
scripts/docker-smoke/riot-binary.sh --distro archlinux --platform linux/arm64
```

## Remote verification

After publish, confirm the OCaml toolchain manifest contains the new artifacts:

```text
https://cdn.pkgs.ml/ocaml/manifest.json
```

The release is not done until the published matrix reflects the targets you
intended to ship.
