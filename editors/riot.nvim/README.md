# riot.nvim

Neovim integration for Riot.

`riot.nvim` keeps the editor-side behavior thin and shells out to Riot’s CLI
surfaces for formatting, diagnostics, workspace/package discovery, runnable
listing, tests, benchmarks, and dependency management.

## Status

Current features:

- `:RiotFmt`, `:RiotFix`, `:RiotFixAll`, and `:RiotExplain`
- automatic `riot lsp stdio` startup for `*.ml` and `*.mli`
- save-time formatting through the LSP server when attached, with CLI fallback
- live parser and lint diagnostics through `vim.diagnostic` from `riot-lsp`
- `:RiotBuild`, `:RiotCheck`, `:RiotRun`, `:RiotRunBinary`, and `:RiotRunExample`
- `:RiotTest`, `:RiotTestWorkspace`, `:RiotTestPackage`, `:RiotTestFile`, and `:RiotTestNearest`
- `:RiotBench`, `:RiotBenchPackage`, `:RiotBenchFile`, `:RiotBenchNearest`, and `:RiotBenchLast`
- `:RiotAdd` and `:RiotRemove`
- `:RiotLogs`, `:RiotLspLogs`, `:RiotLspStart`, `:RiotLspStop`, `:RiotLspRestart`, and `:RiotLspInfo`

Planned next:

- richer benchmark result rendering inside Neovim
- tighter `riot-lsp` code-action integration as the server surface grows
- more exact nearest-position discovery once Riot can expose test and benchmark source spans directly

## Install

The plugin lives inside the Riot repo for now. With `lazy.nvim`, a local setup
looks like:

```lua
{
  dir = "~/Developer/github.com/leostera/riot/editors/riot.nvim",
}
```

No setup is required for the default behavior. Loading the plugin is enough to
enable format-on-save.

You do not need `nvim-lspconfig` just to use `riot-lsp` with this plugin.
`riot.nvim` starts `riot lsp stdio` directly.

## Repo-local Autoload

This repo also ships a root-level [.nvim.lua](/Users/leostera/Developer/github.com/leostera/riot/.nvim.lua)
that adds `editors/riot.nvim` to `runtimepath` and loads the plugin
automatically.

To use that path, enable Neovim project-local config in your personal config:

```lua
vim.o.exrc = true
```

Then trust the repo-local file once from inside the repo:

```vim
:trust .nvim.lua
```

After that, starting Neovim with the repo root as the current directory will
autoload `riot.nvim`.

## Configuration

If you want to override the Riot executable or tune the split sizes:

```lua
require("riot").setup({
  riot_cmd = { "riot" },
  notify = true,
  terminal_height = 14,
  logs_height = 16,
})
```

Options:

- `riot_cmd` overrides the Riot executable, for example `{ "/path/to/riot" }`
- `notify` controls whether failures are shown with `vim.notify`
- `enable_lsp` disables the automatic `riot lsp stdio` client bootstrap when set to `false`
- `terminal_height` controls the default bottom split height for Riot terminal commands
- `logs_height` controls the default split height for Riot log and explanation buffers

## Commands

- `:RiotFmt` formats the current file
- `:RiotFix` applies quick fixes for the current diagnostic when `riot-lsp` is attached
- `:RiotFixAll` applies fix-all actions for the current file
- `:RiotExplain` explains the diagnostic under the cursor
- `:RiotBuild` builds the current package when one is active, otherwise the workspace
- `:RiotCheck` typechecks the current file, or falls back to the current package/workspace
- `:RiotRun` picks a runnable from the whole workspace
- `:RiotRunBinary` and `:RiotRunExample` filter that picker by kind
- `:RiotTest` runs the current package by default and uses file-scoped behavior inside `tests/`
- `:RiotTestWorkspace`, `:RiotTestPackage`, `:RiotTestFile`, `:RiotTestNearest`
- `:RiotBench`, `:RiotBenchPackage`, `:RiotBenchFile`, `:RiotBenchNearest`, `:RiotBenchLast`
- `:RiotAdd [dep]` and `:RiotRemove [dep]`
- `:RiotLogs` opens plugin-local logs
- `:RiotLspLogs` opens the `riot-lsp` server log and Neovim LSP client log
- `:RiotLspStart`, `:RiotLspStop`, `:RiotLspRestart`, `:RiotLspInfo`

## Notes

- The plugin formats file-backed buffers only.
- Format-on-save uses the LSP formatter before the write when `riot-lsp` is
  attached, and falls back to the post-write CLI pipeline otherwise.
- When `riot lsp stdio` is available, `riot.nvim` attaches to it automatically
  and lets the server publish parser and lint diagnostics directly.
- The plugin uses the LSP formatter when attached and falls back to the legacy
  `riot fmt --json` / `riot fix --json` pipeline when the server is not
  available yet.
- While the legacy `riot fix --json` fallback is running, `riot.nvim` also
  surfaces coarse progress notifications such as waiting on the build lock or
  building the generated fix runner.
- Project/package discovery comes from `riot info --json`, not from local
  manifest parsing heuristics.
- Run pickers use `riot run --list --json` and always enumerate the whole
  workspace, even when the current buffer belongs to one package.
- Test and benchmark pickers use `riot test --list --json` and
  `riot bench --list --json`, scoped to the current package where that makes
  the UX more useful.
- Manual `:RiotFmt` formats modified buffers through the LSP client when it is
  attached; otherwise it falls back to the saved-file CLI pipeline and refreshes
  Riot diagnostics for the saved file.
- Neovim `0.10+` is assumed for `vim.system` and `vim.fs`.
