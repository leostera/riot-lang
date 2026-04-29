local source = debug.getinfo(1, "S").source
local this_file = source:sub(1, 1) == "@" and source:sub(2) or source

local repo_root = vim.fs.dirname(this_file)
local plugin_root = repo_root .. "/editors/riot.nvim"

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

vim.diagnostic.config({
  underline = true,
  signs = true,
  severity_sort = true,
  virtual_text = {
    source = "if_many",
    spacing = 2,
  },
})

local function lsp_diagnostic_data(diagnostic)
  local user_data = diagnostic.user_data
  if type(user_data) ~= "table" then
    return nil
  end

  local lsp = user_data.lsp
  if type(lsp) ~= "table" then
    return nil
  end

  if type(lsp.data) == "table" then
    return lsp.data
  end

  return nil
end

local explain_cache = {
  ["syn"] = {},
  ["typ"] = {},
  ["riot-fix"] = {},
}

local function strip_prefix(text, prefix)
  if text:sub(1, #prefix) == prefix then
    return text:sub(#prefix + 1)
  end

  return text
end

local function trim(text)
  return (text:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function split_explanation_output(text)
  local normalized = text:gsub("\r\n", "\n")
  local body = normalized:match("\n%s*\n(.*)$")
  if body ~= nil then
    return trim(body)
  end

  return trim(normalized)
end

local function riot_command_for_buffer(bufnr)
  local clients = vim.lsp.get_clients({ bufnr = bufnr, name = "riot-lsp" })
  local client = clients[1]
  if client and type(client.config) == "table" and type(client.config.cmd) == "table" then
    local cmd = vim.deepcopy(client.config.cmd)
    local count = #cmd
    if count >= 3 and cmd[count - 1] == "lsp" and cmd[count] == "stdio" then
      table.remove(cmd, count)
      table.remove(cmd, count - 1)
    end

    if #cmd > 0 then
      return cmd
    end
  end

  return { "riot" }
end

local function explain_command(diagnostic)
  local code = diagnostic.code
  local source = diagnostic.source

  if type(code) ~= "string" or code == "" then
    return nil, nil
  end

  if source == "syn" then
    return "syn", { "fmt", "--explain", code }
  end

  if source == "typ" then
    return "typ", { "check", "--explain", code }
  end

  if source == "riot-fix" then
    return "riot-fix", { "fix", "--explain", code }
  end

  return nil, nil
end

local function explain_text(diagnostic)
  local cache_key, subcommand = explain_command(diagnostic)
  local code = diagnostic.code
  if cache_key == nil or type(code) ~= "string" or code == "" then
    return nil
  end

  local cache = explain_cache[cache_key]
  if cache[code] ~= nil then
    return cache[code] or nil
  end

  local command = riot_command_for_buffer(vim.api.nvim_get_current_buf())
  vim.list_extend(command, subcommand)
  local result = vim.system(command, { text = true }):wait()
  if result.code ~= 0 or type(result.stdout) ~= "string" or trim(result.stdout) == "" then
    cache[code] = false
    return nil
  end

  local body = split_explanation_output(result.stdout)
  if body == "" then
    cache[code] = false
    return nil
  end

  cache[code] = body
  return body
end

local function format_riot_diagnostic(diagnostic)
  local code = diagnostic.code
  local message = diagnostic.message or ""
  local source = diagnostic.source
  local data = lsp_diagnostic_data(diagnostic)
  local explanation = explain_text(diagnostic)

  if source == "typ" then
    local name = type(data) == "table" and data.name or nil
    if type(name) == "string" and name ~= "" then
      local title = name:gsub("%-", "_")
      local body = explanation or strip_prefix(message, name:gsub("%-", " ") .. ": ")
      if type(code) == "string" and code ~= "" then
        return string.format("typ: %s (%s)\n\n%s", title, code, body)
      end

      return string.format("typ: %s\n\n%s", title, body)
    end
  end

  if source == "riot-fix" then
    local header
    if type(code) == "string" and code ~= "" then
      header = "riot-fix: " .. code
    else
      header = "riot-fix"
    end

    if type(explanation) == "string" and explanation ~= "" then
      return string.format("%s\n\n%s", header, explanation)
    end

    return string.format("%s\n\n%s", header, message)
  end

  if source == "syn" and type(explanation) == "string" and explanation ~= "" then
    if type(code) == "string" and code ~= "" then
      return string.format("[%s]\n\n%s", code, explanation)
    end

    return explanation
  end

  if type(code) == "string" and code ~= "" then
    return string.format("[%s] %s", code, message)
  end

  return message
end

vim.diagnostic.config({
  float = {
    header = "",
    prefix = "",
    suffix = "",
    source = false,
    format = format_riot_diagnostic,
  },
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if not client or client.name ~= "riot-lsp" then
      return
    end

    if type(client.supports_method) ~= "function" or client:supports_method("textDocument/hover") then
      vim.keymap.set("n", "K", function()
        vim.lsp.buf.hover({
          border = "rounded",
        })
      end, {
        buffer = args.buf,
        desc = "Riot LSP hover",
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
