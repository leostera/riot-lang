# riot.nvim AGENTS

`riot.nvim` owns the Neovim-facing editor integration for Riot.

## Rules

1. Keep the plugin thin. Editor UX belongs here; build, format, test, and package-management behavior belongs in Riot itself.
2. Prefer shelling out to stable Riot CLI surfaces over reimplementing Riot logic in Lua.
3. When Riot already exposes machine-readable output, consume that output.
4. Prefer coarse status notifications sourced from Riot JSON events; only special-case plain stderr lines for known gaps like build-lock waiting.
5. Root detection should follow the nearest `riot.toml`.
6. Keep the first-class user flow synchronous and predictable before adding background jobs or progress UIs.
7. File-save hooks should prevent write loops and only target file-backed OCaml buffers.
8. Document any required Neovim version or plugin-manager assumptions in the plugin README.
9. Workspace/package discovery in the plugin should prefer `riot info --json`; runnable, test, and benchmark pickers should prefer the corresponding `riot ... --list --json` surfaces over local scanning.
10. Optional integrations like `neotest-riot` should stay additive and degrade cleanly when the external plugin is not installed.

## Local Checks

`nvim --headless -n -u NONE -i NONE +"set runtimepath+=./editors/riot.nvim" +"runtime plugin/riot.lua" +"lua assert(vim.fn.exists(':RiotFmt') == 2)" +qall!`
