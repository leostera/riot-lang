# Validation

Run the narrowest checks that prove the release candidate is real, but do not
skip the core preflight.

Start by checking whether the release inventory is coherent:

```bash
.agents/skills/riot-release/scripts/release_inventory.py
```

## Core preflight

`git status --short` is an inspection step, not a demand for a globally clean
tree. Releasing from a dirty workspace is fine when all changes under
`./packages`, every real `riot.toml` release manifest, and the release inputs
being published are committed. Leave unrelated dirty files outside the release
scope alone.

```bash
git status --short
riot fmt
riot build --all
riot test --small
```

Useful variants:

```bash
riot fmt --check
riot fmt --verify
riot build -p <package> --all
riot test -p <package> -f "<filter>" --json
riot bench -p <package> -f "<filter>" --release
```

## When to use `riot run riot -- ...`

If you changed Riot CLI behavior and need to validate the just-built workspace
binary instead of whatever is globally installed, run commands through the local
Riot binary:

```bash
riot run riot -- build -p riot-cli
riot run riot -- test -p riot-cli -f "<filter>"
riot run riot -- bench -p <package> -f "<filter>"
```

Use plain installed `riot` for ordinary repo validation when the command
behavior itself is not under test.

## Benchmark-related changes

When the release affects benchmark behavior or benchmark output:

```bash
riot bench -p <package> -f "<filter>" --compare 3 --warmup 100 --iterations 1000 --release
```

Record only when you intentionally want the benchmark saved:

```bash
riot bench -p <package> -f "<filter>" --record --release
```

## Cross-target validation

Build the Riot CLI for the target you intend to ship:

```bash
riot build -x x86_64-unknown-linux-gnu -p riot-cli
riot build -x aarch64-unknown-linux-gnu -p riot-cli
```

The built Riot binary lands at:

```text
_build/debug/<target>/out/riot-cli/riot
```

## Docker smoke tests

When install/build/run behavior changed, or when validating Linux artifacts from
macOS, use the Docker smoke wrapper:

```bash
scripts/docker-smoke/riot-binary.sh --distro archlinux --platform linux/amd64
scripts/docker-smoke/riot-binary.sh --distro ubuntu --platform linux/amd64
scripts/docker-smoke/riot-binary.sh --distro archlinux --platform linux/arm64
scripts/docker-smoke/riot-binary.sh --distro ubuntu --platform linux/arm64
```

To smoke-test an already built Riot binary:

```bash
scripts/docker-smoke/riot-binary.sh \
  --distro archlinux \
  --platform linux/amd64 \
  --riot-bin _build/debug/x86_64-unknown-linux-gnu/out/riot-cli/riot
```

The Docker smoke path validates:
- `riot init hello-world --bin`
- `riot build`
- `riot run`
- `riot test --small`

using the mounted Riot binary inside Arch or Ubuntu containers.
