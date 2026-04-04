---
title: Command Reference
description: The current top-level Riot command surface and what each command is for.
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

### `riot add`

Add a registry, local path, or GitHub dependency and refresh `riot.lock`.

```sh
riot add <name>
riot add <name>@<version>
riot add ../path
riot add github.com/<owner>/<repo>[/pkg][#ref]
```

Useful flags:

- `-p, --package` to edit a specific package manifest
- `--workspace` to edit the workspace root manifest
- `--build` for `[build-dependencies]`
- `--dev` for `[dev-dependencies]`
- `--json` for machine-readable output

### `riot rm`

Remove a dependency from a manifest section and refresh `riot.lock`.

### `riot update`

Re-resolve the workspace graph, update locked package versions, and rewrite
`riot.lock`.

### `riot search`

Search `pkgs.ml` by package name. Supports `--json` and `--limit`.

### `riot publish`

Publish packages to the registry. Important flags:

- `-p, --package` for one package
- `--workspace` to publish workspace packages in dependency order
- `--dry-run` to run local checks without uploading
- `--skip-check` to skip `riot fix --check`

### `riot login` and `riot logout`

Save or remove your `pkgs.ml` API token. `riot login --token <token>` allows
non-interactive login.

## Build and execution commands

### `riot build`

Build packages for the current workspace. Supports:

- package filters
- `--target` for target architecture selection
- `--release`
- `--json` for JSONL build events

### `riot run`

Run a binary by name, optionally narrowed with `-p, --package`.

### `riot install`

Install a binary to `~/.riot/bin` and the project root. Use `--local` to skip
the global install path.

### `riot clean`

Clean build artifacts.

## Quality and development commands

### `riot fmt`

Format OCaml with `krasny`. Supports:

- `--check`
- `--verify`
- `--json`
- `--explain <code>` for parser diagnostic explanations

### `riot fix`

Run Riot's linter. Supports:

- `--apply`
- `--check`
- `--list-rules`
- `--list-diagnostics`
- `--limit <n>`
- `--explain <rule-id>`
- `--json`

### `riot test`

Run tests with optional substring matching, optionally scoped with
`-p, --package`.

### `riot bench`

Run benchmarks with optional substring matching, optionally scoped with
`-p, --package`.

### `riot snapshots`

Review and manage snapshot candidates. The subcommand surface includes
`approve`, `reject`, and `review`.

### `riot doc`

Generate documentation.

### `riot lsp`

Start Riot's language server.

### `riot completions`

Generate shell completions or list structured completion data for packages,
binaries, tests, benchmarks, and package commands.

## Environment and lifecycle commands

### `riot init`

Initialize a new Riot workspace. Supports `--lib` and `--bin`.

### `riot new`

Create a new package inside a workspace. Supports `--lib` and `--bin`.

### `riot toolchain`

Manage OCaml toolchains. The current subcommands are:

- `list`
- `install`

### `riot upgrade`

Upgrade the globally installed Riot binary. Supports `--version` to request a
specific version.

### `riot version`

Print the current Riot version.

Use `riot <command> --help` for detailed usage of each command.
