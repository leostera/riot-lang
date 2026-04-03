# riot.nvim

Neovim integration for Riot.

This first slice is intentionally small: it formats the current OCaml file and
keeps Riot diagnostics live by speaking to `riot lsp stdio` when available.
The legacy CLI path remains a fallback for buffers that cannot attach to the
server yet.

## Status

Current features:

- `:RiotFmt` formats the current buffer, preferring `riot lsp stdio` and falling back to `riot fmt`
- `:RiotFix` applies Riot quick fixes for the diagnostic under the cursor
- automatic `riot lsp stdio` startup for `*.ml` and `*.mli`
- save-time formatting through the LSP server when attached, with CLI fallback
- live parser and lint diagnostics through `vim.diagnostic` from `riot-lsp`
- save-time status notifications when the legacy CLI fallback is waiting on the build lock or building the generated fix runner

Planned next:

- `:RiotBuild`
- `:RiotTest`
- package add/remove helpers
- richer LSP commands and code actions once the server exposes them

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

If you want to override the Riot executable or notifications:

```lua
require("riot").setup({
  riot_cmd = { "riot" },
  notify = true,
})
```

Options:

- `riot_cmd` overrides the Riot executable, for example `{ "/path/to/riot" }`
- `notify` controls whether failures are shown with `vim.notify`
- `enable_lsp` disables the automatic `riot lsp stdio` client bootstrap when set to `false`

## Commands

- `:RiotFmt` formats the current file
- `:RiotFix` applies quick fixes for the current diagnostic when `riot-lsp` is attached

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
  surfaces a few coarse progress notifications such as waiting on the build
  lock or building the generated fix runner.
- Manual `:RiotFmt` formats modified buffers through the LSP client when it is
  attached; otherwise it falls back to the saved-file CLI pipeline and refreshes
  Riot diagnostics for the saved file.
- Neovim `0.10+` is assumed for `vim.system` and `vim.fs`.
