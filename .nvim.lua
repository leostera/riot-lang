local source = debug.getinfo(1, "S").source
local this_file = source:sub(1, 1) == "@" and source:sub(2) or source

local repo_root = vim.fs.dirname(this_file)
local plugin_root = repo_root .. "/editors/tinyml.nvim"

if vim.fn.isdirectory(plugin_root) ~= 1 then
  return
end

vim.opt.runtimepath:prepend(plugin_root)

local ok, tinyml = pcall(require, "tinyml")
if not ok then
  return
end

vim.o.updatetime = 50

vim.filetype.add({
  extension = {
    ml = "tinyml",
  },
})

vim.diagnostic.config({
  underline = true,
  signs = true,
  severity_sort = true,
  virtual_text = {
    source = "if_many",
    spacing = 2,
  },
  float = {
    header = "",
    prefix = "",
    suffix = "",
    source = false,
    border = "rounded",
  },
})

tinyml.setup({
  root_dir = repo_root,
  cmd = { "cargo", "run", "-q", "-p", "tinyml", "--", "lsp" },
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client or client.name ~= "tinyml" then
      return
    end

    if type(client.supports_method) ~= "function" or client:supports_method("textDocument/hover") then
      vim.keymap.set("n", "K", function()
        vim.lsp.buf.hover({
          border = "rounded",
        })
      end, {
        buffer = args.buf,
        desc = "TinyML LSP hover",
      })
    end

    if vim.lsp.inlay_hint == nil then
      return
    end

    local supports_inlay_hints = false
    if type(client.supports_method) == "function" then
      supports_inlay_hints = client:supports_method("textDocument/inlayHint")
    else
      supports_inlay_hints = client.server_capabilities ~= nil
          and client.server_capabilities.inlayHintProvider ~= nil
    end

    if supports_inlay_hints then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end
  end,
})

vim.api.nvim_create_autocmd("CursorHold", {
  callback = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local line = vim.api.nvim_win_get_cursor(0)[1] - 1
    local diags = vim.diagnostic.get(bufnr, {
      lnum = line,
    })

    if #diags == 0 then
      return
    end

    vim.diagnostic.open_float(bufnr, {
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
      source = false,
    })
  end,
})
