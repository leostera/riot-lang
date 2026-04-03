# vscode-riot-ml AGENTS

`vscode-riot-ml` owns the VS Code-facing editor integration for Riot.

## Rules

1. Keep the extension thin. Formatting, diagnostics, build, test, and install behavior belong in Riot itself.
2. Prefer shelling out to stable Riot CLI surfaces over reimplementing Riot logic in TypeScript.
3. Prefer machine-readable Riot output such as `riot fmt --check --json` and `riot fix --check --json` over parsing human text.
4. Root detection should follow the nearest `riot.toml`; do not hardcode repo-local assumptions.
5. Install managed Riot into extension-owned storage; do not mutate the user's shell configuration from the extension.
6. Save hooks must avoid save loops and should only touch file-backed `*.ml` and `*.mli` documents.
7. Startup checks may log the resolved Riot binary/version and compare it to the latest published Riot, but they must stay lightweight and avoid prompting repeatedly for unmanaged PATH installs.
8. Keep the MVP focused on install, format, build/test commands, and diagnostics. Leave LSP features to `riot lsp`.

## Validate

From [editors/vscode-riot-ml](/Users/leostera/Developer/github.com/leostera/riot/editors/vscode-riot-ml):

```sh
bun run compile
```
