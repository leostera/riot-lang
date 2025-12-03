# Feature Request: Add libuuid to musl toolchains

## Problem

When cross-compiling Riot/Tusk to musl targets (e.g., `x86_64-unknown-linux-musl`, `aarch64-unknown-linux-musl`), the build fails with:

```
fatal error: uuid/uuid.h: No such file or directory
```

This occurs in `packages/kernel/native/kernel_uuid.c` which depends on `libuuid` for UUID generation.

## Current Behavior

The musl toolchains built by `riot-ocaml` do not include `libuuid` (or `util-linux` which provides it). The toolchains are located at:
- `~/.tusk/toolchains/5.5.0/x86_64-unknown-linux-musl/`
- `~/.tusk/toolchains/5.5.0/aarch64-unknown-linux-musl/`

These toolchains have `bin/` and `lib/` directories but are missing UUID library support.

## Expected Behavior

The musl toolchains should include `libuuid` headers and libraries so that C code using `<uuid/uuid.h>` can compile successfully when cross-compiling to musl targets.

## Solution

Add `libuuid` (from `util-linux` package) to the musl toolchain builds in the `riot-ocaml` repository.

For Alpine Linux musl systems, the relevant packages are:
- `libuuid` - runtime library: https://pkgs.alpinelinux.org/package/edge/main/x86/libuuid
- `util-linux-dev` - development headers: https://pkgs.alpinelinux.org/package/edge/main/x86/util-linux-dev

## Files Affected

- Riot side: `packages/kernel/native/kernel_uuid.c` uses `uuid/uuid.h`
- Toolchain side: musl sysroot needs to include libuuid headers and libraries

## Priority

High - This blocks the CI release workflow from building musl binaries which are important for Alpine Linux users and static linking scenarios.

## Workaround

Temporarily, we can skip musl builds or try to manually install libuuid during CI (fragile approach).

## Additional Context

The glibc toolchains work fine because `uuid-dev` is available via apt. The musl toolchains need the equivalent musl-compatible library bundled in the sysroot.
