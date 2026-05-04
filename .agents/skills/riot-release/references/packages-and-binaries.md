# Packages And Binaries

Read this for:
- `riot publish`
- Riot package release order
- Riot binary packaging and upload
- local and remote verification of the released artifacts

Start with the inventory helper to confirm the release version and real manifest
set:

```bash
.agents/skills/riot-release/scripts/release_inventory.py
```

## Package publish

Publishing does not require a globally clean workspace. Before publishing, make
sure the release point has committed all package changes, every real
`riot.toml` release manifest, and release inputs such as `CHANGELOG.md`. Dirty
files outside that scope can stay in the current workspace.

The workspace publish flow is:

```bash
riot publish --workspace --dry-run --skip-check
riot publish --workspace --skip-check
```

Notes:
- `--workspace` publishes workspace packages in dependency order.
- `--skip-check` skips the `riot fix --check` preflight step.
- Use `--dry-run` before the real publish when you want a local rehearsal.

You can also narrow to one package:

```bash
riot publish -p <package> --dry-run --skip-check
```

Publish from the release tag or exact release commit when version alignment
matters.

## Riot binary release

The binary release script is:

```bash
./scripts/release/riot.sh <target|all>
./scripts/release/riot.sh --force <target|all>
```

Key behavior from the script:
- it reads the release version from `packages/riot-cli/riot.toml`
- `all` uses targets from `ocaml-toolchain.toml` when present
- it refuses to republish an existing remote version unless `--force` is passed
- it strips release binaries before upload
- it uploads/update:
  - Riot tarballs
  - `install.sh`
  - `latest.json`
  - `manifest.json`

Use `--force` only when you are intentionally replacing a previously uploaded
artifact for the same version.

## Which Riot binary the release script uses

`scripts/release/riot.sh` uses an installed Riot binary by default.

If you need the release flow to use a specific built binary, set:

```bash
export RIOT_RELEASE_RIOT_BIN=/abs/path/to/riot
```

A common local path is:

```text
_build/debug/<target>/out/riot-cli/riot
```

## Local binary validation

Before uploading, build the exact target you plan to ship:

```bash
riot build -x aarch64-apple-darwin -p riot-cli
riot build -x aarch64-unknown-linux-gnu -p riot-cli
riot build -x x86_64-unknown-linux-gnu -p riot-cli
```

When CLI behavior changed, validate the workspace-built Riot directly:

```bash
riot run riot -- build -p riot-cli
riot run riot -- test -p riot-cli -f "<filter>"
```

## Remote verification

After a Riot binary release, check:

```text
https://cdn.pkgs.ml/riot/latest.json
https://cdn.pkgs.ml/riot/manifest.json
https://cdn.pkgs.ml/riot/install.sh
```

The local release directory is usually:

```text
dist/riot
```

## Linux smoke tests for binaries

For install/build/run validation on Linux, prefer the Docker smoke wrapper over
manual container setup:

```bash
scripts/docker-smoke/riot-binary.sh --distro archlinux --platform linux/amd64
scripts/docker-smoke/riot-binary.sh --distro ubuntu --platform linux/amd64
```

This mounts the just-built Riot binary into the container and validates a
generated workspace end to end.
