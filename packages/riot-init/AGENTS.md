# riot-init AGENTS

`riot-init` owns workspace and package scaffolding.

## Rules

1. Generated files should match the current repo conventions, not historical ones.
2. Keep templates minimal and easy to maintain.
3. If scaffolding contracts change, update CLI expectations and documentation in the same change.
4. `riot init` should leave a new workspace immediately runnable: scaffold the root Dockerfile, GitHub Actions workflow, and a starter package test alongside the package sources.
5. `Riot_init.run` is the library seam for workspace scaffolding. It should emit structured init events and leave rendering to `riot-cli` instead of printing directly.
6. `Riot_init.new_package` owns package scaffolding inside an existing workspace. It should accept typed `Path.t` inputs, not raw strings, update `[workspace].members` when it creates a package, and it should not route through `riot-build`.
7. Workspace names and starter package names are different contracts: workspace names may contain dots, but the starter package and binary names must still be valid Riot package names.

## Validate

`timeout 30 riot build riot-init`
