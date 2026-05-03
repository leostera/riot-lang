---
name: riot-release
description: "Prepare, validate, version, and publish Riot releases from this repository, including OCaml toolchains, Riot CLI binaries, and workspace packages. Use when asked to cut a new Riot release, bump Riot/package or toolchain versions, update CHANGELOG.md, publish packages with `riot publish`, build or publish toolchains with `scripts/toolchain/ocaml.sh`, release binaries with `scripts/release/riot.sh`, create Linux sysroots, or smoke-test Riot binaries in Docker."
---

# Riot Release

Use this skill only from the `riot-new` repository root.

Prefer the repository's release scripts over ad hoc command sequences. They are
the source of truth for packaging, upload, manifests, and publish order.

## Quick Start

1. Run the release inventory helper:

```bash
.agents/skills/riot-release/scripts/release_inventory.py
```

2. Read the references for the release type you are about to cut.
3. Use the repo scripts for the real publish steps.

## Release Map

- For a normal Riot/package release, read:
  - [references/versioning.md](references/versioning.md)
  - [references/validation.md](references/validation.md)
  - [references/packages-and-binaries.md](references/packages-and-binaries.md)
- For any toolchain, sysroot, cross-compilation, or default toolchain epoch
  change, also read:
  - [references/toolchains.md](references/toolchains.md)

## Core Rules

- Keep the Riot semver, the release commit, and the release tag aligned.
- Keep the OCaml toolchain epoch separate from the Riot semver.
  - Example: Riot `0.0.23`
  - Example: toolchain `5.5.0-riot.4`
- Bump every real release manifest, but do not touch fixture manifests under
  `tests/` or workspace-fixture directories.
- Publish packages from the exact release tag or release commit, not from newer
  `HEAD`.
- Use `riot run riot -- ...` when you need to validate behavior of the
  workspace-built Riot CLI itself.
- Use the installed `riot` for normal repo validation and publish commands
  unless the task specifically depends on just-built CLI behavior.
- Require a clean worktree before tagging or publishing.
- Never create a clean worktree by stashing, dropping, resetting, restoring, or
  otherwise moving changes you did not make. If unrelated dirty files are
  present, stop and ask the owner to clear them, or perform release preparation
  from a separate worktree.
- Treat these files as the main release inputs:
  - `packages/riot-cli/riot.toml`
  - `ocaml-toolchain.toml`
  - `CHANGELOG.md`
  - `scripts/release.sh`
  - `scripts/release/riot.sh`
  - `scripts/toolchain/ocaml.sh`
  - `scripts/create-sysroot.sh`
  - `scripts/docker-smoke/riot-binary.sh`
- Use `.agents/skills/riot-release/scripts/release_inventory.py` to discover:
  - current Riot version
  - next patch candidate
  - current default toolchain epoch
  - real release manifests to bump
  - fixture manifests to skip
  - toolchain targets
  - version mismatches across release manifests

## Workflow

1. Inspect the scope of the release.
   - Start with `.agents/skills/riot-release/scripts/release_inventory.py`.
   - Read the current Riot version from `packages/riot-cli/riot.toml`.
   - Read the current default toolchain epoch from `ocaml-toolchain.toml`.
   - Decide whether this release changes:
     - packages only
     - Riot binary behavior
     - toolchains/sysroots/cross targets

2. Prepare versions and changelog.
   - Follow [references/versioning.md](references/versioning.md).

3. Validate locally.
   - Follow [references/validation.md](references/validation.md).

4. Release toolchains first when the toolchain epoch, sysroot, or bundled cross
   toolchains changed.
   - Follow [references/toolchains.md](references/toolchains.md).

5. Publish packages.
   - Follow [references/packages-and-binaries.md](references/packages-and-binaries.md).

6. Release Riot binaries.
   - Follow [references/packages-and-binaries.md](references/packages-and-binaries.md).

7. Verify the published artifacts.
   - Check local `dist/` outputs.
   - Check remote manifests and latest metadata.
   - Run Docker smoke tests when the change affects install/build/run behavior
     on Linux.

## Practical Defaults

- For a package or binary release without toolchain changes:
  1. bump versions
  2. update `CHANGELOG.md`
  3. run `riot fmt`, `riot build --all`, and `riot test --small`
  4. publish packages from the release commit or tag
  5. run `./scripts/release/riot.sh all`

- For a toolchain release:
  1. bump the toolchain epoch everywhere it is baked in
  2. build and publish the toolchains
  3. validate the toolchains with cross builds and Docker smoke tests
  4. bump Riot/package versions if the new default toolchain is part of the
     release story
  5. publish packages and Riot binaries from the matching release point

## References

- [references/versioning.md](references/versioning.md)
- [references/validation.md](references/validation.md)
- [references/toolchains.md](references/toolchains.md)
- [references/packages-and-binaries.md](references/packages-and-binaries.md)
