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
  local filetypes = opts.filetypes or { "tinyml" }
  local cmd = opts.cmd or { "cargo", "run", "-q", "-p", "tinyml", "--", "lsp" }

  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = "*.ml",
    callback = function(args)
      if opts.detect_all_ml or looks_like_tinyml_repo(args.file) then
        vim.bo[args.buf].filetype = "tinyml"
      end
    end,
  })

  vim.api.nvim_create_autocmd("FileType", {
    pattern = filetypes,
    callback = function(args)
      local root = opts.root_dir or find_root(vim.api.nvim_buf_get_name(args.buf))
      vim.lsp.start({
        name = "tinyml",
        cmd = cmd,
        root_dir = root,
        capabilities = opts.capabilities,
      }, { bufnr = args.buf })
    end,
  })
end

return M
