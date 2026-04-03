# Riot ML

VS Code integration for Riot.

This first slice stays intentionally thin and shells out to Riot instead of
reimplementing formatter or linter logic in TypeScript.

## Features

- `Riot: Install Riot` installs a managed Riot binary for the extension by
  fetching `https://get.riot.ml`
- shows startup notifications for the resolved Riot version and whether a newer
  published Riot is available
- contributes bundled language support for `*.ml` and `*.mli`
- formats `*.ml` and `*.mli` files through `riot fmt`
- surfaces parser diagnostics from `riot fmt --check --json` while you edit
- surfaces lint diagnostics from `riot fix --check --json` on open and save
- adds `Riot: Build Workspace` and `Riot: Test Workspace` commands
- contributes VS Code tasks for `riot build` and `riot test`

## Requirements

- macOS or Linux
- a Riot workspace rooted at `riot.toml` for build and test commands

## Extension Settings

This extension contributes the following settings:

- `riot.path`: explicit Riot executable path
- `riot.installUrl`: installer URL used by `Riot: Install Riot`
- `riot.latestMetadataUrl`: release metadata URL used for startup upgrade checks
- `riot.formatOnSave`: format `*.ml` and `*.mli` on save
- `riot.diagnostics.enabled`: enable Riot diagnostics
- `riot.diagnostics.runFix`: include `riot fix --json` diagnostics

## Development

This extension uses Bun for local development:

```sh
bun run compile
bun run watch
```

## Known Issues

- build and test integration currently shells out through VS Code tasks rather
  than `riot lsp`
