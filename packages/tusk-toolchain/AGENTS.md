# tusk-toolchain AGENTS

`tusk-toolchain` wraps compiler and toolchain discovery and invocation.

## Rules

1. Keep external tool invocation centralized here instead of scattering compiler knowledge through tusk.
2. Treat path resolution and target-platform handling as core behavior.
3. When adding flags, verify whether they belong to host, target, or package profile configuration.

## Validate

`timeout 30 tusk build tusk-toolchain`
