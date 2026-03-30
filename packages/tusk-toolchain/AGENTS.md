# tusk-toolchain AGENTS

`tusk-toolchain` wraps compiler and toolchain discovery and invocation.

## Rules

1. Keep external tool invocation centralized here instead of scattering compiler knowledge through tusk.
2. Treat path resolution and target-platform handling as core behavior.
3. When adding flags, verify whether they belong to host, target, or package profile configuration.
4. For cross builds, prefer the installed toolchain’s bundled compiler/sysroot or explicit env overrides before falling back to host PATH discovery.
5. Treat cross toolchains as incomplete until the bundled C toolchain and sysroot are present, not just the OCaml binaries.

## Validate

`timeout 30 tusk build tusk-toolchain`
