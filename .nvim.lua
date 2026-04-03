local source = debug.getinfo(1, "S").source
local this_file = source:sub(1, 1) == "@" and source:sub(2) or source
local repo_root = vim.fs.dirname(this_file)
local plugin_root = repo_root .. "/editors/nvim/riot.nvim"

if vim.fn.isdirectory(plugin_root) ~= 1 then
  return
end

vim.opt.runtimepath:prepend(plugin_root)
vim.cmd.runtime({ "plugin/riot.lua", bang = true })



local ok, riot = pcall(require, "riot")
if not ok then
  return
end

vim.o.updatetime = 50

local riot_ns = riot.diagnostics_namespace

vim.api.nvim_create_autocmd("CursorHold", {
  callback = function()
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local diags = vim.diagnostic.get(0, {
      namespace = riot_ns,
      lnum = line,
    })

    if #diags == 0 then
      return
    end

    vim.diagnostic.open_float({
      namespace = riot_ns,
      scope = "cursor",
      focusable = false,
      close_events = {
        "CursorMoved",
        "CursorMovedI",
        "BufHidden",
        "InsertEnter",
        "WinLeave",
      },
      border = "rounded",
      source = "if_many",
    })
  end,
})
