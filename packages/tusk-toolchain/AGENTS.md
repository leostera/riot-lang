# tusk-toolchain AGENTS

`tusk-toolchain` wraps compiler and toolchain discovery and invocation.

## Rules

1. Keep external tool invocation centralized here instead of scattering compiler knowledge through tusk.
2. Treat path resolution and target-platform handling as core behavior.
3. When adding flags, verify whether they belong to host, target, or package profile configuration.
4. For cross builds, prefer the installed toolchain’s bundled compiler/sysroot or explicit env overrides before falling back to host PATH discovery.
5. Treat cross toolchains as incomplete until the bundled C toolchain and sysroot are present, not just the OCaml binaries.
6. Toolchain downloads replace existing installs atomically enough to avoid mixing stale archives with new contents.
7. Toolchain cache fingerprints must change when installed compiler artifacts or bundled sysroot markers change, even if the install path stays the same.
8. Toolchains shipped in release archives should include `manifest.json` with a stable `toolchain_fingerprint`, and toolchain cache hashing should prefer this manifest over probing files on disk.
9. Prepared compiler invocations should stay opaque to callers; keep execution semantics in `tusk-toolchain` instead of exposing raw process execution to higher layers.

## Validate

`timeout 30 tusk build tusk-toolchain`
