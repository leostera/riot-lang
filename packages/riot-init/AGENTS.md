# riot-init AGENTS

`riot-init` owns workspace and package scaffolding.

## Rules

1. Generated files should match the current repo conventions, not historical ones.
2. Keep templates minimal and easy to maintain.
3. If scaffolding contracts change, update CLI expectations and documentation in the same change.
4. `riot init` should leave a new workspace immediately runnable: scaffold the root Dockerfile, GitHub Actions workflow, and a starter package test alongside the package sources.

## Validate

`timeout 30 riot build riot-init`
