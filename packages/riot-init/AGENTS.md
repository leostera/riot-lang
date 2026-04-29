# riot-init AGENTS

`riot-init` owns workspace and package scaffolding.

## Rules

1. Generated files should match the current repo conventions, not historical ones.
2. Keep templates minimal and easy to maintain.
3. If scaffolding contracts change, update CLI expectations and documentation in the same change.
4. `riot init` should leave a new workspace immediately runnable: scaffold the root Dockerfile, GitHub Actions workflow, and a starter package test alongside the package sources.
5. `Riot_init.run` is the library seam for workspace scaffolding. It should emit structured init events and leave rendering to `riot-cli`.
6. `Riot_init.new_package` owns package scaffolding inside an existing workspace, and `Riot_init.new_standalone_package` owns detached package scaffolding outside a workspace. Both should accept typed `Path.t` inputs, and only the workspace variant should update `[workspace].members`. Neither should route through `riot-build`.
7. Workspace names and starter package names are different contracts: workspace names may contain dots, but the starter package and binary names must still be valid Riot package names.
8. `riot init` should scaffold the repo-operational defaults we expect new workspaces to use: `.agents/skills/riot/*`, `config/dev.toml`, `.riot/config.toml`, and `.githooks/pre-commit`. Starter binaries should load `Std.Config` and start `Std.Log` by default so generated workspaces boot with config and logging already wired.
