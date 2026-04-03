# riot.nvim

Neovim integration for Riot.

This first slice is intentionally small: it formats the current OCaml file and
refreshes Riot diagnostics by running `riot fmt --json <current-file>` and
`riot fix --json <current-file>` on every save.

## Status

Current features:

- `:RiotFmt` formats the current file on disk with `riot fmt`
- mandatory format-on-save for `*.ml` and `*.mli`
- save-time parser diagnostics through `vim.diagnostic` when formatting fails
- save-time lint diagnostics through `vim.diagnostic` from `riot fix --json`
- save-time status notifications when `riot fix` is waiting on the build lock or building the generated fix runner

Planned next:

- `:RiotBuild`
- `:RiotTest`
- package add/remove helpers
- LSP bootstrap once `riot lsp` is real

## Install

The plugin lives inside the Riot repo for now. With `lazy.nvim`, a local setup
looks like:

```lua
{
  dir = "~/Developer/github.com/leostera/riot/editors/nvim/riot.nvim",
}
```

No setup is required for the default behavior. Loading the plugin is enough to
enable format-on-save.

## Repo-local Autoload

This repo also ships a root-level [.nvim.lua](/Users/leostera/Developer/github.com/leostera/riot/.nvim.lua)
that adds `editors/nvim/riot.nvim` to `runtimepath` and loads the plugin
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

## Commands

- `:RiotFmt` formats the current file

## Notes

- The plugin formats file-backed buffers only.
- Format-on-save always runs after the file write and then reloads the buffer if the
  formatter changed the file on disk.
- When `riot fmt --json` reports parser diagnostics, `riot.nvim` publishes them
  to Neovim diagnostics instead of only showing a notification.
- After formatting succeeds, `riot.nvim` also runs `riot fix --json` for the
  saved file and publishes any lint warnings or errors through the same Riot
  diagnostics namespace.
- While `riot fix --json` is running, `riot.nvim` also surfaces a few coarse
  progress notifications such as waiting on the build lock or building the
  generated fix runner.
- Manual `:RiotFmt` refuses to format a modified buffer; save first so the
  formatter sees the current contents. It also refreshes Riot diagnostics for
  the saved file.
- Neovim `0.10+` is assumed for `vim.system` and `vim.fs`.
