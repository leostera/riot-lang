local M = {}

local defaults = {
  notify = true,
  enable_lsp = true,
  riot_cmd = { "riot" },
}

local diagnostics_namespace = vim.api.nvim_create_namespace("riot.nvim")

local state = {
  config = vim.deepcopy(defaults),
}

local function json_value(value)
  if value == vim.NIL then
    return nil
  end

  return value
end

local function normalize_riot_cmd(riot_cmd)
  if type(riot_cmd) == "string" and riot_cmd ~= "" then
    return { riot_cmd }
  end

  if type(riot_cmd) == "table" and #riot_cmd > 0 then
    return vim.deepcopy(riot_cmd)
  end

  return { "riot" }
end

local function merge_config(opts)
  local config = vim.tbl_deep_extend("force", vim.deepcopy(defaults), opts or {})
  config.riot_cmd = normalize_riot_cmd(config.riot_cmd)
  return config
end

local function notify(message, level)
  if not state.config.notify then
    return
  end

  vim.notify(message, level or vim.log.levels.INFO, { title = "riot.nvim" })
end

local function current_buffer_path(bufnr)
  local path = vim.api.nvim_buf_get_name(bufnr)
  if path == "" then
    return nil, "current buffer is not backed by a file"
  end

  return path
end

local function supported_file(path)
  return path:match("%.ml$") ~= nil or path:match("%.mli$") ~= nil
end

local function workspace_root(path)
  local root_files = vim.fs.find("riot.toml", {
    path = vim.fs.dirname(path),
    upward = true,
    type = "file",
  })

  if #root_files == 0 then
    return vim.fs.dirname(path)
  end

  return vim.fs.dirname(root_files[1])
end

local function riot_lsp_command()
  local command = vim.deepcopy(state.config.riot_cmd)
  table.insert(command, "lsp")
  table.insert(command, "stdio")
  return command
end

local function nearest_manifest(path)
  local manifests = vim.fs.find("riot.toml", {
    path = vim.fs.dirname(path),
    upward = true,
    type = "file",
  })

  return manifests[1]
end

local function read_file(path)
  local lines = vim.fn.readfile(path)
  if vim.v.shell_error ~= 0 then
    return nil
  end

  return lines
end

local function parse_package_name(manifest_path)
  local lines = read_file(manifest_path)
  if not lines then
    return nil
  end

  local in_package = false

  for _, line in ipairs(lines) do
    local trimmed = vim.trim(line)

    if trimmed:match("^%[package%]$") then
      in_package = true
    elseif trimmed:match("^%[") then
      in_package = false
    elseif in_package then
      local name = trimmed:match('^name%s*=%s*"([^"]+)"')
      if name ~= nil then
        return name
      end
    end
  end

  return nil
end

local function package_for_path(path)
  local manifest_path = nearest_manifest(path)
  if not manifest_path then
    return nil
  end

  local name = parse_package_name(manifest_path)
  if not name then
    return nil
  end

  return {
    name = name,
    manifest_path = manifest_path,
    root = vim.fs.dirname(manifest_path),
  }
end

local function fmt_command(path)
  local command = vim.deepcopy(state.config.riot_cmd)
  table.insert(command, "fmt")
  table.insert(command, "--json")
  table.insert(command, path)
  return command
end

local function fix_command(path)
  local command = vim.deepcopy(state.config.riot_cmd)
  table.insert(command, "fix")
  table.insert(command, "--json")
  table.insert(command, path)
  return command
end

local function clear_diagnostics(bufnr)
  vim.diagnostic.reset(diagnostics_namespace, bufnr)
end

local function riot_lsp_clients(bufnr)
  return vim.lsp.get_clients({ bufnr = bufnr, name = "riot-lsp" })
end

local function riot_lsp_active(bufnr)
  return #riot_lsp_clients(bufnr) > 0
end

local function start_riot_lsp(bufnr)
  if not state.config.enable_lsp then
    return false
  end

  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    return false
  end

  if not supported_file(path) then
    return false
  end

  local root = workspace_root(path)
  if type(root) ~= "string" or root == "" then
    return false
  end

  if riot_lsp_active(bufnr) then
    clear_diagnostics(bufnr)
    return true
  end

  local started = false
  vim.api.nvim_buf_call(bufnr, function()
    local client_id = vim.lsp.start({
      name = "riot-lsp",
      cmd = riot_lsp_command(),
      root_dir = root,
      single_file_support = true,
      on_attach = function(_, attached_bufnr)
        if vim.api.nvim_buf_is_valid(attached_bufnr) then
          clear_diagnostics(attached_bufnr)
        end
      end,
    })

    started = client_id ~= nil
  end)

  if started then
    clear_diagnostics(bufnr)
  end

  return started or riot_lsp_active(bufnr)
end

local function format_with_riot_lsp(bufnr)
  if not riot_lsp_active(bufnr) then
    return false
  end

  vim.lsp.buf.format({
    bufnr = bufnr,
    async = false,
    filter = function(client)
      return client.name == "riot-lsp"
    end,
  })

  return true
end

local function diagnostics_under_cursor(bufnr)
  local line = vim.api.nvim_win_get_cursor(0)[1] - 1
  return vim.diagnostic.get(bufnr, { lnum = line })
end

local function diagnostic_contains_cursor(diagnostic, line, col)
  local start_line = diagnostic.lnum
  local end_line = diagnostic.end_lnum or diagnostic.lnum
  local start_col = diagnostic.col or 0
  local end_col = diagnostic.end_col or start_col

  if line < start_line or line > end_line then
    return false
  end

  if start_line == end_line then
    if end_col <= start_col then
      return col == start_col
    end

    return col >= start_col and col <= end_col
  end

  if line == start_line then
    return col >= start_col
  end

  if line == end_line then
    return col <= end_col
  end

  return true
end

local function riot_lsp_diagnostics_under_cursor(bufnr)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1] - 1
  local col = cursor[2]
  local items = {}

  for _, client in ipairs(riot_lsp_clients(bufnr)) do
    local push_namespace = vim.lsp.diagnostic.get_namespace(client.id, false)
    local pull_namespace = vim.lsp.diagnostic.get_namespace(client.id, true)

    vim.list_extend(items, vim.diagnostic.get(bufnr, { namespace = push_namespace, lnum = line }))
    vim.list_extend(items, vim.diagnostic.get(bufnr, { namespace = pull_namespace, lnum = line }))
  end

  return vim.tbl_filter(function(diagnostic)
    return diagnostic.user_data
      and diagnostic.user_data.lsp
      and diagnostic_contains_cursor(diagnostic, line, col)
  end, items)
end

local function set_diagnostics(bufnr, items)
  vim.diagnostic.set(diagnostics_namespace, bufnr, items or {})
end

local function parse_jsonl(output)
  local events = {}

  for _, line in ipairs(vim.split(output or "", "\n", { trimempty = true })) do
    local ok, decoded = pcall(vim.json.decode, line)
    if ok and type(decoded) == "table" then
      table.insert(events, decoded)
    end
  end

  return events
end

local function split_lines(text)
  local lines = {}

  for _, line in ipairs(vim.split(text or "", "\n", { trimempty = true })) do
    table.insert(lines, line)
  end

  return lines
end

local function stream_lines(buffer_ref, chunk, on_line)
  if chunk == nil then
    local line = buffer_ref[1]

    if line ~= "" then
      on_line(line)
      buffer_ref[1] = ""
    end

    return
  end

  buffer_ref[1] = buffer_ref[1] .. chunk

  while true do
    local newline = buffer_ref[1]:find("\n", 1, true)

    if not newline then
      break
    end

    local line = buffer_ref[1]:sub(1, newline - 1)
    buffer_ref[1] = buffer_ref[1]:sub(newline + 1)

    if line ~= "" then
      on_line(line)
    end
  end
end

local function normalized_path(path, base_path)
  if type(path) ~= "string" or path == "" then
    return nil
  end

  local candidate = path

  if base_path and not path:match("^/") then
    candidate = vim.fs.joinpath(base_path, path)
  end

  local realpath = nil

  if vim.uv and vim.uv.fs_realpath then
    realpath = vim.uv.fs_realpath(candidate)
  elseif vim.loop and vim.loop.fs_realpath then
    realpath = vim.loop.fs_realpath(candidate)
  end

  return vim.fs.normalize(realpath or candidate)
end

local function file_event(events, path, cwd)
  local target_path = normalized_path(path)
  local found = nil

  for _, event in ipairs(events) do
    if event.type == "file" then
      local event_path = normalized_path(json_value(event.file), cwd)

      if target_path == nil or event_path == target_path then
        found = event
      end
    end
  end

  return found
end

local function offset_to_position(lines, offset)
  offset = math.max(offset or 0, 0)

  if #lines == 0 then
    return 0, 0
  end

  local line_start = 0

  for index, line in ipairs(lines) do
    local line_end = line_start + #line
    if offset <= line_end then
      return index - 1, math.min(offset - line_start, #line)
    end

    line_start = line_end + 1
  end

  local last = lines[#lines]
  return #lines - 1, #last
end

local function syn_diagnostic_message(diagnostic)
  local kind = json_value(diagnostic.kind) or {}
  local found = json_value(kind.found) or {}
  local message = ("expected %s, found %s"):format(
    kind.expected or "syntax",
    found.kind or "unknown"
  )

  local fix = json_value(kind.fix)
  local hint = json_value(kind.hint)

  if fix then
    message = message .. "\nfix: " .. fix
  end

  if hint then
    message = message .. "\nhint: " .. hint
  end

  return message, kind.id
end

local function syn_diagnostic_items(lines, diagnostics, source)
  if type(diagnostics) ~= "table" then
    return {}
  end

  local items = {}

  for _, diagnostic in ipairs(diagnostics) do
    local span = json_value(diagnostic.span) or {}
    local start_lnum, start_col = offset_to_position(lines, span.start)
    local end_lnum, end_col = offset_to_position(lines, span["end"])
    local message, code = syn_diagnostic_message(diagnostic)

    table.insert(items, {
      lnum = start_lnum,
      col = start_col,
      end_lnum = end_lnum,
      end_col = end_col,
      message = message,
      severity = vim.diagnostic.severity.ERROR,
      source = source,
      code = code,
    })
  end

  return items
end

local function fix_diagnostic_severity(severity)
  if severity == "error" then
    return vim.diagnostic.severity.ERROR
  end

  if severity == "warning" then
    return vim.diagnostic.severity.WARN
  end

  if severity == "info" then
    return vim.diagnostic.severity.INFO
  end

  return vim.diagnostic.severity.HINT
end

local function fix_diagnostic_message(diagnostic)
  local message = diagnostic.message or "riot fix reported an issue"
  local suggestion = json_value(diagnostic.suggestion)
  local fix = json_value(diagnostic.fix) or {}
  local fix_title = json_value(fix.title)

  if suggestion then
    message = message .. "\nsuggestion: " .. suggestion
  end

  if fix_title then
    message = message .. "\nfix: " .. fix_title
  end

  return message, diagnostic.rule_id
end

local function fix_diagnostic_items(lines, diagnostics)
  if type(diagnostics) ~= "table" then
    return {}
  end

  local items = {}

  for _, diagnostic in ipairs(diagnostics) do
    local span = json_value(diagnostic.span) or {}
    local start_lnum, start_col = offset_to_position(lines, span.start)
    local end_lnum, end_col = offset_to_position(lines, span["end"])
    local message, code = fix_diagnostic_message(diagnostic)

    table.insert(items, {
      lnum = start_lnum,
      col = start_col,
      end_lnum = end_lnum,
      end_col = end_col,
      message = message,
      severity = fix_diagnostic_severity(diagnostic.severity),
      source = "riot fix",
      code = code,
    })
  end

  return items
end

local function append_items(items, new_items)
  if type(new_items) ~= "table" then
    return
  end

  for _, item in ipairs(new_items) do
    table.insert(items, item)
  end
end

local function diagnostic_items_from_fix_event(lines, event)
  local items = {}
  append_items(
    items,
    syn_diagnostic_items(lines, json_value(event.parse_diagnostics), "riot fix")
  )
  append_items(items, fix_diagnostic_items(lines, json_value(event.diagnostics)))
  return items
end

local function diagnostic_items_from_fmt_event(lines, event)
  return syn_diagnostic_items(lines, json_value(event.diagnostics), "riot fmt")
end

local function current_lines(bufnr)
  return vim.api.nvim_buf_get_lines(bufnr, 0, -1, true)
end

local function progress_reporter()
  local last_message = nil

  local function report(message)
    if not state.config.notify or message == nil or message == last_message then
      return
    end

    last_message = message

    vim.schedule(function()
      notify(message)
    end)
  end

  local function on_event(event)
    local event_type = json_value(event.type)
    local package = json_value(event.package) or {}
    local target = json_value(event.target)

    if event_type == "WorkspaceStarted" and target == "fixme-runner" then
      report("Building fix runner...")
      return
    end

    if
      event_type == "BuildCompleted"
      and package.name == "fixme-runner"
    then
      report("Built fix runner")
    end
  end

  local function on_stderr(line)
    if vim.trim(line) == "build lock is taken, waiting..." then
      report("Waiting on build lock...")
    end
  end

  return {
    on_event = on_event,
    on_stderr = on_stderr,
  }
end

local function run_command(command, opts)
  opts = opts or {}

  local stdout_buffer = { "" }
  local stderr_buffer = { "" }
  local events = {}
  local stderr_lines = {}

  local function on_stdout_line(line)
    local ok, decoded = pcall(vim.json.decode, line)

    if ok and type(decoded) == "table" then
      table.insert(events, decoded)

      if opts.on_event then
        opts.on_event(decoded)
      end
    end
  end

  local function on_stderr_line(line)
    table.insert(stderr_lines, line)

    if opts.on_stderr then
      opts.on_stderr(line)
    end
  end

  local result = vim.system(command, {
    cwd = opts.cwd,
    text = true,
    stdout = function(_, data)
      stream_lines(stdout_buffer, data, on_stdout_line)
    end,
    stderr = function(_, data)
      stream_lines(stderr_buffer, data, on_stderr_line)
    end,
  }):wait()

  if #events == 0 then
    events = parse_jsonl(result.stdout)

    if opts.on_event then
      for _, event in ipairs(events) do
        opts.on_event(event)
      end
    end
  end

  if #stderr_lines == 0 then
    stderr_lines = split_lines(result.stderr)

    if opts.on_stderr then
      for _, line in ipairs(stderr_lines) do
        opts.on_stderr(line)
      end
    end
  end

  return result, events, stderr_lines
end

local function command_message(result, event, fallback)
  local message = ""

  if event then
    message = vim.trim(json_value(event.error) or "")
  end

  if message == "" then
    local stderr = vim.trim(result.stderr or "")
    local stdout = vim.trim(result.stdout or "")

    if stderr ~= "" then
      message = stderr
    else
      message = stdout
    end
  end

  if message == "" then
    message = fallback
  end

  return message
end

local function reload_buffer(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end

  vim.api.nvim_buf_call(bufnr, function()
    vim.cmd("silent! checktime")
  end)
end

local function run_fmt(bufnr, opts)
  opts = opts or {}

  local path, err = current_buffer_path(bufnr)
  if not path then
    if not opts.silent then
      notify(err, vim.log.levels.WARN)
    end
    return { ok = false, skipped = true, items = {} }
  end

  if not supported_file(path) then
    return { ok = false, skipped = true, items = {} }
  end

  if vim.bo[bufnr].modified then
    if not opts.silent then
      notify("save the buffer before running :RiotFmt", vim.log.levels.WARN)
    end
    return { ok = false, skipped = true, items = {} }
  end

  local cwd = workspace_root(path)
  local result, events = run_command(fmt_command(path), { cwd = cwd })
  local event = file_event(events, path, cwd)
  local items = diagnostic_items_from_fmt_event(current_lines(bufnr), event or {})
  local has_diagnostics = #items > 0

  if result.code ~= 0 then
    if not has_diagnostics and not opts.silent then
      notify(command_message(result, event, "riot fmt failed"), vim.log.levels.ERROR)
    end

    return { ok = false, skipped = false, items = items }
  end

  if event and event.status == "formatted" then
    reload_buffer(bufnr)
  end

  return { ok = true, skipped = false, items = items }
end

local function run_fix(bufnr, opts)
  opts = opts or {}

  local path, err = current_buffer_path(bufnr)
  if not path then
    if not opts.silent then
      notify(err, vim.log.levels.WARN)
    end
    return { ok = false, skipped = true, items = {} }
  end

  if not supported_file(path) then
    return { ok = false, skipped = true, items = {} }
  end

  if vim.bo[bufnr].modified then
    if not opts.silent then
      notify("save the buffer before running :RiotFmt", vim.log.levels.WARN)
    end
    return { ok = false, skipped = true, items = {} }
  end

  local cwd = workspace_root(path)
  local reporter = progress_reporter()
  local result, events = run_command(fix_command(path), {
    cwd = cwd,
    on_event = reporter.on_event,
    on_stderr = reporter.on_stderr,
  })
  local event = file_event(events, path, cwd)
  local items = diagnostic_items_from_fix_event(current_lines(bufnr), event or {})
  local has_diagnostics = #items > 0

  if result.code ~= 0 then
    if not has_diagnostics and not opts.silent then
      notify(command_message(result, event, "riot fix failed"), vim.log.levels.ERROR)
    end

    return { ok = false, skipped = false, items = items }
  end

  return { ok = true, skipped = false, items = items }
end

local function run_cli_save_pipeline(bufnr, opts)
  local fmt = run_fmt(bufnr, opts)

  if fmt.skipped then
    return false
  end

  if not fmt.ok then
    set_diagnostics(bufnr, fmt.items)
    return false
  end

  local fix = run_fix(bufnr, opts)
  local items = {}
  append_items(items, fmt.items)
  append_items(items, fix.items)
  set_diagnostics(bufnr, items)

  return fix.ok
end

local function run_lsp_save_pipeline(bufnr)
  if not start_riot_lsp(bufnr) then
    return false
  end

  return format_with_riot_lsp(bufnr)
end

local function configure_format_on_save()
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("riot.nvim.format_pre", { clear = true }),
    pattern = { "*.ml", "*.mli" },
    desc = "Format OCaml files with riot lsp before save",
    callback = function(args)
      vim.b[args.buf].riot_lsp_formatted = run_lsp_save_pipeline(args.buf)
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup("riot.nvim.format", { clear = true }),
    pattern = { "*.ml", "*.mli" },
    desc = "Refresh riot diagnostics with the CLI fallback after save",
    callback = function(args)
      if vim.b[args.buf].riot_lsp_formatted then
        vim.b[args.buf].riot_lsp_formatted = nil
        return
      end

      run_cli_save_pipeline(args.buf, { silent = true })
    end,
  })
end

local function configure_lsp_autostart()
  vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
    group = vim.api.nvim_create_augroup("riot.nvim.lsp", { clear = true }),
    pattern = { "*.ml", "*.mli" },
    desc = "Start riot-lsp for OCaml files",
    callback = function(args)
      start_riot_lsp(args.buf)
    end,
  })
end

function M.setup(opts)
  state.config = merge_config(opts)
  configure_lsp_autostart()
  configure_format_on_save()
end

function M.format_current_buffer()
  local bufnr = vim.api.nvim_get_current_buf()

  if start_riot_lsp(bufnr) and format_with_riot_lsp(bufnr) then
    return true
  end

  return run_cli_save_pipeline(bufnr)
end

function M.fix_current_diagnostic()
  local bufnr = vim.api.nvim_get_current_buf()

  if not start_riot_lsp(bufnr) then
    notify("riot-lsp is not attached for this buffer", vim.log.levels.WARN)
    return false
  end

  local diagnostics = riot_lsp_diagnostics_under_cursor(bufnr)
  if #diagnostics == 0 then
    notify("no riot-lsp diagnostic under the cursor", vim.log.levels.WARN)
    return false
  end

  vim.lsp.buf.code_action({
    apply = true,
    context = {
      diagnostics = vim.tbl_map(function(diagnostic)
        return diagnostic.user_data.lsp
      end, diagnostics),
      only = { "quickfix" },
    },
  })

  return true
end

function M.current_package(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  local path = current_buffer_path(bufnr)
  if not path then
    return nil
  end

  return package_for_path(path)
end

function M.show_current_package()
  local package = M.current_package()
  if not package then
    notify("no Riot package found for current buffer", vim.log.levels.WARN)
    return nil
  end

  notify(package.name)
  return package
end

M.diagnostics_namespace = diagnostics_namespace

return M
