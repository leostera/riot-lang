# tinyml.nvim

Repository-local Neovim syntax support for TinyML.

## Setup

```lua
require("tinyml").setup()
```

By default the plugin detects `*.ml` files as `tinyml` when opened inside this
repository. Set `detect_all_ml = true` to detect the filetype for ML files
outside the repository:

```lua
require("tinyml").setup({ detect_all_ml = true })
```

The plugin provides syntax highlighting for TinyML keywords, constructors,
primitive types, literals, comments, and operators, and sets `commentstring` to
`// %s`.
