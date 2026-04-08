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
8. Prefer `riot lsp stdio` for editor-facing formatting/diagnostics when Riot is available, and keep the CLI formatter/diagnostics path as a fallback instead of duplicating Riot logic in TypeScript.
9. Workspace/package discovery in the extension should prefer `riot info --json`; runnable, test, and benchmark discovery should prefer the corresponding `riot ... --list --json` surfaces over local manifest scanning.
10. Shared extension services should back both human UI entrypoints and extension-contributed chat tools; do not fork separate shell logic for the chat-facing tool layer.

## Validate

From [editors/vscode-riot-ml](/Users/leostera/Developer/github.com/leostera/riot/editors/vscode-riot-ml):

```sh
bun run compile
```
