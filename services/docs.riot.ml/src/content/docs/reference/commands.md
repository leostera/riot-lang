---
title: Command Surface
description: Current top-level Riot command surface.
---

The current Riot command surface is:

```text
add            Add a registry, local path, or GitHub dependency and refresh riot.lock
bench          Run benchmarks with optional substring matching
build          Build packages
clean          Clean build artifacts
completions    Generate shell completions or list completion data
doc            Generate documentation
fix            Lint OCaml code and optionally apply safe fixes
fmt            Format OCaml with krasny
init           Initialize a new Riot workspace
install        Install a binary to ~/.riot/bin and project root
login          Save your pkgs.ml API token
logout         Remove your saved pkgs.ml API token
lsp            Start Riot LSP server
new            Create a new package
publish        Publish packages to the registry
rm             Remove a dependency from a manifest section and refresh riot.lock
run            Run a binary
search         Search pkgs.ml for packages by name
snapshots      Review and manage pending snapshot candidates
test           Run tests with optional substring matching
toolchain      Manage OCaml toolchains
update         Re-resolve the workspace graph, update locked package versions, and rewrite riot.lock
upgrade        Upgrade the globally installed riot binary
version        Show riot version
```

## Dependency and registry commands

- `add`
- `rm`
- `update`
- `search`
- `publish`
- `login`
- `logout`

## Build and execution commands

- `build`
- `run`
- `install`
- `clean`

## Quality and development commands

- `fmt`
- `fix`
- `test`
- `bench`
- `snapshots`
- `doc`
- `lsp`
- `completions`

## Environment and lifecycle commands

- `init`
- `new`
- `toolchain`
- `upgrade`
- `version`

Use `riot <command> --help` for detailed usage of each command.
