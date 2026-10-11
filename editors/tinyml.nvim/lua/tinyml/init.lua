local M = {}
local uv = vim.uv or vim.loop

local function find_root(start)
  local dir = vim.fs.dirname(start)
  local found = vim.fs.find({ "Cargo.toml", ".git" }, { path = dir, upward = true })[1]
  if found then
    return vim.fs.dirname(found)
  end
  return uv.cwd()
end

local function looks_like_tinyml_repo(path)
  local root = find_root(path)
  return uv.fs_stat(root .. "/tinyml/Cargo.toml") ~= nil
end

function M.setup(opts)
  opts = opts or {}
  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = "*.ml",
    callback = function(args)
      if opts.detect_all_ml or looks_like_tinyml_repo(args.file) then
        vim.bo[args.buf].filetype = "tinyml"
      end
    end,
  })
end

return M
