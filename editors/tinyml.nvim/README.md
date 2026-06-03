# tinyml.nvim

Repository-local Neovim integration for TinyML.

## Setup

```lua
require("tinyml").setup()
```

By default the plugin:

- detects `*.ml` files as `tinyml` when opened inside this repository;
- provides Vim syntax highlighting for TinyML keywords, constructors, primitive types, literals, comments, and operators;
- sets `commentstring` to `// %s`;
- starts the language server with:

```sh
cargo run -q -p tinyml -- lsp
```

You can override the command:

```lua
require("tinyml").setup({
  cmd = { "tinyml", "lsp" },
})
```

## LSP features

- diagnostics
- inlay type hints
- hover type information
