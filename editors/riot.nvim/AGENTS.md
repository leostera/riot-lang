# riot.nvim AGENTS

`riot.nvim` owns the Neovim-facing editor integration for Riot.

## Rules

1. Keep the plugin thin. Editor UX belongs here; build, format, test, and package-management behavior belongs in Riot itself.
2. Prefer shelling out to stable Riot CLI surfaces over reimplementing Riot logic in Lua.
3. When Riot already exposes machine-readable output, prefer consuming that instead of scraping human text.
4. Prefer coarse status notifications sourced from Riot JSON events; only special-case plain stderr lines for known gaps like build-lock waiting.
5. Root detection should follow the nearest `riot.toml`; do not hardcode repo-local assumptions into the plugin.
6. Keep the first-class user flow synchronous and predictable before adding background jobs or progress UIs.
7. File-save hooks must avoid write loops and should only target file-backed OCaml buffers.
8. Document any required Neovim version or plugin-manager assumptions in the plugin README.

## Validate

`nvim --headless -n -u NONE -i NONE +"set runtimepath+=./editors/riot.nvim" +"runtime plugin/riot.lua" +"lua assert(vim.fn.exists(':RiotFmt') == 2)" +qall!`
