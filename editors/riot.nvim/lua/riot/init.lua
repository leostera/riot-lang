local M = {}

local defaults = {
  notify = true,
  enable_lsp = true,
  riot_cmd = { "riot" },
  terminal_height = 14,
  logs_height = 16,
}

local diagnostics_namespace = vim.api.nvim_create_namespace("riot.nvim")

local state = {
  config = vim.deepcopy(defaults),
  logs = {},
  last_test_selector = nil,
  last_bench_selector = nil,
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

local function timestamp()
  return os.date("%H:%M:%S")
end

local function append_log(kind, message)
  for _, line in ipairs(vim.split(message or "", "\n", { trimempty = true })) do
    table.insert(state.logs, ("[%s] %s %s"):format(timestamp(), kind, line))
  end
end

local function buffer_lines_for_text(text)
  local lines = vim.split(text or "", "\n", { plain = true })

  if #lines > 0 and lines[#lines] == "" then
    table.remove(lines, #lines)
  end

  if #lines == 0 then
    return { "" }
  end

  return lines
end

local function open_scratch(title, lines, opts)
  opts = opts or {}

  vim.cmd("botright split")
  local win = vim.api.nvim_get_current_win()

  if opts.height then
    vim.api.nvim_win_set_height(win, opts.height)
  end

  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(win, bufnr)
  vim.bo[bufnr].bufhidden = "wipe"
  vim.bo[bufnr].buftype = "nofile"
  vim.bo[bufnr].buflisted = false
  vim.bo[bufnr].swapfile = false
  vim.bo[bufnr].modifiable = true
  vim.api.nvim_buf_set_name(bufnr, title)
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines or { "" })
  vim.bo[bufnr].modifiable = false

  if opts.filetype then
    vim.bo[bufnr].filetype = opts.filetype
  end

  return bufnr, win
end

local function show_text(title, text, opts)
  return open_scratch(title, buffer_lines_for_text(text), opts)
end

local function shell_join(command)
  return table.concat(vim.tbl_map(vim.fn.shellescape, command), " ")
end

local function display_command(command, cwd)
  if cwd and cwd ~= "" then
    return ("$ (cd %s && %s)"):format(vim.fn.shellescape(cwd), shell_join(command))
  end

  return "$ " .. shell_join(command)
end

local function riot_command(args)
  local command = vim.deepcopy(state.config.riot_cmd)
  vim.list_extend(command, args)
  return command
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

local function command_cwd_for_path(path)
  if type(path) == "string" and path ~= "" then
    return vim.fs.dirname(path)
  end

  return vim.uv.cwd()
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

local function path_directory(path)
  if type(path) ~= "string" or path == "" then
    return nil
  end

  if vim.fn.isdirectory(path) == 1 then
    return path
  end

  return vim.fs.dirname(path)
end

local function read_file(path)
  local lines = vim.fn.readfile(path)
  if vim.v.shell_error ~= 0 then
    return nil
  end

  return lines
end

local workspace_info_for_path
local current_package_for_path_from_info
local command_message

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
  local info = workspace_info_for_path and workspace_info_for_path(path)
  local info_package = current_package_for_path_from_info and current_package_for_path_from_info(info, path)

  if info_package then
    return info_package
  end

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

local function configure_riot_lsp_buffer(bufnr)
  if vim.api.nvim_buf_is_valid(bufnr) then
    vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
  end
end

local function client_supports_method(client, method)
  if type(client.supports_method) == "function" then
    return client:supports_method(method)
  end

  local capabilities = client.server_capabilities
  if type(capabilities) ~= "table" then
    return false
  end

  if method == "textDocument/completion" then
    return capabilities.completionProvider ~= nil
  end

  if method == "textDocument/inlayHint" then
    return capabilities.inlayHintProvider ~= nil
  end

  return false
end

local function attach_riot_lsp_buffer(client, bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end

  clear_diagnostics(bufnr)
  configure_riot_lsp_buffer(bufnr)

  if vim.lsp.inlay_hint ~= nil and client_supports_method(client, "textDocument/inlayHint") then
    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
  end
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

  configure_riot_lsp_buffer(bufnr)

  local root = workspace_root(path)
  if type(root) ~= "string" or root == "" then
    return false
  end

  if riot_lsp_active(bufnr) then
    clear_diagnostics(bufnr)
    for _, client in ipairs(riot_lsp_clients(bufnr)) do
      attach_riot_lsp_buffer(client, bufnr)
    end
    return true
  end

  local started = false
  vim.api.nvim_buf_call(bufnr, function()
    local client_id = vim.lsp.start({
      name = "riot-lsp",
      cmd = riot_lsp_command(),
      root_dir = root,
      single_file_support = true,
      on_attach = function(client, attached_bufnr)
        attach_riot_lsp_buffer(client, attached_bufnr)
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

  append_log("cmd", display_command(command, opts.cwd))

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

  append_log("exit", ("status=%s"):format(tostring(result.code)))

  if vim.trim(result.stderr or "") ~= "" then
    append_log("stderr", result.stderr)
  end

  if #events == 0 and vim.trim(result.stdout or "") ~= "" then
    append_log("stdout", result.stdout)
  end

  return result, events, stderr_lines
end

workspace_info_for_path = function(path)
  local cwd = path_directory(path) or command_cwd_for_path(path)
  local result, events = run_command(riot_command({ "info", "--json" }), { cwd = cwd })
  local event = events[1]

  if result.code ~= 0 or type(event) ~= "table" then
    return nil
  end

  if event.type ~= "workspace_info" then
    return nil
  end

  return event
end

current_package_for_path_from_info = function(info, path)
  if type(info) ~= "table" or type(info.packages) ~= "table" or type(path) ~= "string" then
    return nil
  end

  local target_path = normalized_path(path)
  if target_path == nil then
    return nil
  end

  for _, pkg in ipairs(info.packages) do
    local package_root = normalized_path(json_value(pkg.root))
    if package_root and vim.startswith(target_path, package_root) then
      return {
        name = json_value(pkg.name),
        manifest_path = json_value(pkg.manifest_path),
        root = package_root,
      }
    end
  end

  return nil
end

local function workspace_context(path)
  local info = workspace_info_for_path(path)
  local package = current_package_for_path_from_info(info, path) or package_for_path(path)

  return {
    info = info,
    package = package,
    cwd = type(info) == "table" and json_value(info.root) or command_cwd_for_path(path),
  }
end

local function command_output_text(result, event)
  local message = command_message(result, event, "")
  if message ~= "" then
    return message
  end

  local output = vim.trim(result.stdout or "")
  if output ~= "" then
    return output
  end

  return vim.trim(result.stderr or "")
end

local function select_item(items, opts, on_choice)
  opts = opts or {}

  if #items == 0 then
    if opts.empty_message then
      notify(opts.empty_message, vim.log.levels.WARN)
    end
    return
  end

  if #items == 1 and not opts.force_picker then
    on_choice(items[1])
    return
  end

  vim.ui.select(items, {
    prompt = opts.prompt or "Select Riot item",
    format_item = opts.format_item,
  }, on_choice)
end

local function open_terminal(command, opts)
  opts = opts or {}

  vim.cmd("botright split")
  vim.api.nvim_win_set_height(0, opts.height or state.config.terminal_height)

  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_win_set_buf(0, bufnr)
  vim.bo[bufnr].bufhidden = "hide"
  vim.bo[bufnr].buflisted = false
  vim.bo[bufnr].swapfile = false
  vim.bo[bufnr].filetype = "riotterm"
  vim.api.nvim_buf_set_name(
    bufnr,
    ("riot://%s/%d"):format((opts.title or "riot"):gsub("%s+", "-"):lower(), bufnr)
  )

  append_log("term", display_command(command, opts.cwd))

  vim.fn.termopen(command, {
    cwd = opts.cwd,
    on_exit = function(_, code)
      append_log("term", ("%s exited with %d"):format(opts.title or "Riot", code))

      if opts.on_exit then
        vim.schedule(function()
          opts.on_exit(code)
        end)
      end
    end,
  })

  vim.cmd("startinsert")
  return bufnr
end

local function open_file_in_split(path, opts)
  opts = opts or {}

  if type(path) ~= "string" or path == "" then
    return
  end

  vim.cmd("botright split")
  vim.api.nvim_win_set_height(0, opts.height or state.config.logs_height)
  vim.cmd("edit " .. vim.fn.fnameescape(path))
end

local function riot_lsp_server_log_path()
  if type(vim.env.RIOT_LSP_LOG_PATH) == "string" and vim.env.RIOT_LSP_LOG_PATH ~= "" then
    return vim.env.RIOT_LSP_LOG_PATH
  end

  return table.concat({ vim.fn.expand("~"), ".riot", "logs", "riot-lsp.log" }, "/")
end

local function nvim_lsp_log_path()
  if vim.lsp.log ~= nil and type(vim.lsp.log.get_filename) == "function" then
    return vim.lsp.log.get_filename()
  end

  if vim.lsp.get_log_path then
    return vim.lsp.get_log_path()
  end

  return nil
end

local function explain_command(source, code)
  if type(code) ~= "string" or code == "" then
    return nil
  end

  local normalized_source = (source or ""):lower()

  if code:match("^TYP%d+$") or normalized_source:find("typ", 1, true) then
    return riot_command({ "check", "--explain", code })
  end

  if code:match("^E%d+$") or normalized_source:find("fmt", 1, true) then
    return riot_command({ "fmt", "--explain", code })
  end

  return riot_command({ "fix", "--explain", code })
end

local function list_runnables(path, opts)
  opts = opts or {}
  local context = workspace_context(path)
  local command = riot_command({ "run", "--list", "--json" })
  local package_name = opts.package_name
    or (opts.current_package and context.package and context.package.name)

  if package_name then
    table.insert(command, "-p")
    table.insert(command, package_name)
  end

  local result, events = run_command(command, { cwd = context.cwd })
  local list_event = nil

  for _, event in ipairs(events) do
    if event.type == "RunList" then
      list_event = event
    end
  end

  if result.code ~= 0 or type(list_event) ~= "table" then
    return nil, command_output_text(result, list_event), context
  end

  local items = {}

  for _, binary in ipairs(json_value(list_event.binaries) or {}) do
    table.insert(items, {
      kind = json_value(binary.kind) or "binary",
      package = json_value(binary.package),
      name = json_value(binary.binary),
      path = json_value(binary.path),
      selector = json_value(binary.selector),
    })
  end

  return items, nil, context
end

local function list_test_cases(path, opts)
  opts = opts or {}
  local context = workspace_context(path)
  local command = riot_command({ "test", "--list", "--json" })
  local package_name = opts.package_name
    or (opts.current_package and context.package and context.package.name)

  if package_name then
    table.insert(command, "-p")
    table.insert(command, package_name)
  end

  local result, events = run_command(command, { cwd = context.cwd })
  if result.code ~= 0 then
    return nil, command_output_text(result), context
  end

  local suites = {}
  local ordered = {}

  for _, event in ipairs(events) do
    if event.type == "TestSuiteListed" then
      local absolute_path = normalized_path(json_value(event.path), context.cwd)
      local suite = {
        type = "suite",
        package = json_value(event.package),
        name = json_value(event.suite),
        selector = json_value(event.selector),
        path = absolute_path,
        cases = {},
      }
      suites[suite.selector] = suite
      table.insert(ordered, suite)
    elseif event.type == "TestCaseListed" then
      local suite_selector = ("%s:%s"):format(event.package, event.suite)
      local suite = suites[suite_selector]
      if suite then
        local case_json = json_value(event["case"]) or {}
        table.insert(suite.cases, {
          type = "case",
          package = json_value(event.package),
          suite = json_value(event.suite),
          name = json_value(event.name),
          selector = json_value(event.selector),
          index = json_value(case_json.index) or json_value(event.index),
          kind = json_value(case_json.type) or "test",
          reliability = json_value(case_json.reliability) or "stable",
          size = json_value(case_json.size) or "small",
          skip = json_value(case_json.skip) or false,
          path = suite.path,
        })
      end
    end
  end

  return ordered, nil, context
end

local function list_benchmarks(path, opts)
  opts = opts or {}
  local context = workspace_context(path)
  local command = riot_command({ "bench", "--list", "--json" })
  local package_name = opts.package_name
    or (opts.current_package and context.package and context.package.name)

  if package_name then
    table.insert(command, "-p")
    table.insert(command, package_name)
  end

  local result, events = run_command(command, { cwd = context.cwd })
  if result.code ~= 0 then
    return nil, command_output_text(result), context
  end

  local suites = {}
  local ordered = {}

  for _, event in ipairs(events) do
    if event.type == "BenchSuiteListed" then
      local absolute_path = normalized_path(json_value(event.path), context.cwd)
      local suite = {
        type = "suite",
        package = json_value(event.package),
        name = json_value(event.suite),
        selector = json_value(event.selector),
        path = absolute_path,
        items = {},
      }
      suites[suite.selector] = suite
      table.insert(ordered, suite)
    elseif event.type == "BenchItemListed" then
      local suite_selector = ("%s:%s"):format(event.package, event.suite)
      local suite = suites[suite_selector]
      if suite then
        local bench_json = json_value(event.benchmark) or {}
        table.insert(suite.items, {
          type = "item",
          package = json_value(event.package),
          suite = json_value(event.suite),
          name = json_value(event.name),
          selector = json_value(event.selector),
          kind = json_value(bench_json.kind) or "benchmark",
          iterations = json_value(bench_json.iterations),
          warmup = json_value(bench_json.warmup),
          skip = json_value(bench_json.skip) or false,
          path = suite.path,
        })
      end
    end
  end

  return ordered, nil, context
end

local function file_matches(path, candidate)
  local normalized_candidate = normalized_path(candidate)
  local normalized_target = normalized_path(path)

  return normalized_candidate ~= nil
    and normalized_target ~= nil
    and normalized_candidate == normalized_target
end

local function suites_for_current_file(path, suites)
  local matches = {}

  for _, suite in ipairs(suites or {}) do
    if file_matches(path, suite.path) then
      table.insert(matches, suite)
    end
  end

  return matches
end

local function nearest_named_entry(bufnr, patterns)
  local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

  for start = cursor_line, 1, -1 do
    local stop = math.min(start + 3, #lines)
    local block = table.concat(vim.list_slice(lines, start, stop), "\n")

    for _, pattern in ipairs(patterns) do
      local name = block:match(pattern)
      if name then
        return name
      end
    end
  end

  return nil
end

command_message = function(result, event, fallback)
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

  reload_buffer(bufnr)

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

local function current_context(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()

  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    return nil, workspace_context(vim.uv.cwd()), path
  end

  return path, workspace_context(path), nil
end

local function run_riot_terminal(args, opts)
  opts = opts or {}
  local path = opts.path
  local context = workspace_context(path or vim.uv.cwd())

  return open_terminal(riot_command(args), {
    cwd = opts.cwd or context.cwd,
    height = opts.height,
    title = opts.title,
    on_exit = opts.on_exit,
  })
end

local function diagnostic_at_cursor(bufnr)
  local lsp_diagnostics = riot_lsp_diagnostics_under_cursor(bufnr)
  if #lsp_diagnostics > 0 then
    return lsp_diagnostics[1]
  end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1] - 1
  local col = cursor[2]

  for _, diagnostic in ipairs(vim.diagnostic.get(bufnr, { lnum = line })) do
    if diagnostic_contains_cursor(diagnostic, line, col) then
      return diagnostic
    end
  end

  return nil
end

local function flatten_test_cases(suites)
  local items = {}

  for _, suite in ipairs(suites or {}) do
    for _, case_item in ipairs(suite.cases or {}) do
      table.insert(items, case_item)
    end
  end

  return items
end

local function flatten_bench_items(suites)
  local items = {}

  for _, suite in ipairs(suites or {}) do
    for _, item in ipairs(suite.items or {}) do
      table.insert(items, item)
    end
  end

  return items
end

local function run_test_selector(selector, path)
  state.last_test_selector = selector

  if selector then
    run_riot_terminal({ "test", selector }, {
      path = path,
      title = "Riot Test",
    })
  end
end

local function run_bench_selector(selector, path)
  state.last_bench_selector = selector

  if selector then
    run_riot_terminal({ "bench", selector }, {
      path = path,
      title = "Riot Bench",
    })
  end
end

local function runnable_picker_items(path, opts)
  local items, err = list_runnables(path, opts)
  if not items then
    notify(err or "failed to list Riot runnables", vim.log.levels.ERROR)
    return nil
  end

  return items
end

local function run_selected_runnable(kind)
  local path = current_buffer_path(vim.api.nvim_get_current_buf())
  local items = runnable_picker_items(path, { current_package = false })

  if not items then
    return
  end

  if kind then
    items = vim.tbl_filter(function(item)
      return item.kind == kind
    end, items)
  end

  select_item(items, {
    prompt = kind == "example" and "Riot example" or kind == "binary" and "Riot binary" or "Riot runnable",
    empty_message = "no Riot runnables found",
    format_item = function(item)
      return ("%s [%s] %s"):format(item.selector, item.kind, item.path or "")
    end,
  }, function(item)
    if item then
      run_riot_terminal({ "run", item.selector }, {
        path = path,
        title = "Riot Run",
      })
    end
  end)
end

local function explain_current_diagnostic()
  local bufnr = vim.api.nvim_get_current_buf()
  local diagnostic = diagnostic_at_cursor(bufnr)

  if not diagnostic then
    notify("no Riot diagnostic under the cursor", vim.log.levels.WARN)
    return false
  end

  local code = diagnostic.code
  if type(code) == "table" then
    code = code.value or code.target
  end

  if type(code) ~= "string" or code == "" then
    notify("current diagnostic has no explainable code", vim.log.levels.WARN)
    return false
  end

  local path, context, err = current_context(bufnr)
  if err then
    notify(err, vim.log.levels.WARN)
    return false
  end

  local command = explain_command(diagnostic.source, code)
  if not command then
    notify("no Riot explain command available for this diagnostic", vim.log.levels.WARN)
    return false
  end

  local result, events = run_command(command, { cwd = context.cwd })
  local output = command_output_text(result, events[1])

  if output == "" then
    notify("no explanation available", vim.log.levels.WARN)
    return false
  end

  show_text(("riot://explain/%s"):format(code), output, {
    height = state.config.logs_height,
    filetype = "markdown",
  })
  return result.code == 0
end

local function pick_test_from_file(bufnr)
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    notify("current buffer is not backed by a file", vim.log.levels.WARN)
    return
  end

  local suites, err = list_test_cases(path, { current_package = true })
  if not suites then
    notify(err or "failed to list Riot tests", vim.log.levels.ERROR)
    return
  end

  local matching_suites = suites_for_current_file(path, suites)
  if #matching_suites == 0 then
    notify("no Riot test suites found for current file", vim.log.levels.WARN)
    return
  end

  local cases = flatten_test_cases(matching_suites)
  select_item(cases, {
    prompt = "Riot test",
    empty_message = "no Riot tests found for current file",
    format_item = function(item)
      return ("%s :: %s"):format(item.package .. ":" .. item.suite, item.name)
    end,
  }, function(item)
    if item then
      run_test_selector(item.selector, path)
    end
  end)
end

local function pick_bench_for_context(bufnr, current_package_only)
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    notify("current buffer is not backed by a file", vim.log.levels.WARN)
    return
  end

  local suites, err = list_benchmarks(path, { current_package = current_package_only })
  if not suites then
    notify(err or "failed to list Riot benchmarks", vim.log.levels.ERROR)
    return
  end

  local items = flatten_bench_items(suites)
  select_item(items, {
    prompt = "Riot benchmark",
    empty_message = "no Riot benchmarks found",
    format_item = function(item)
      return ("%s [%s]"):format(item.selector, item.kind)
    end,
  }, function(item)
    if item then
      run_bench_selector(item.selector, path)
    end
  end)
end

local function nearest_test_selector(bufnr)
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    return nil, "current buffer is not backed by a file"
  end

  local suites, err = list_test_cases(path, { current_package = true })
  if not suites then
    return nil, err or "failed to list Riot tests"
  end

  local matching_suites = suites_for_current_file(path, suites)
  local name = nearest_named_entry(bufnr, {
    'Test%.case.-"([^"]+)"',
    'Test%.property.-"([^"]+)"',
    'Test%.skip.-"([^"]+)"',
    'Test%.todo.-"([^"]+)"',
  })

  if not name then
    return nil, "no nearby Riot test case found"
  end

  for _, suite in ipairs(matching_suites) do
    for _, case_item in ipairs(suite.cases) do
      if case_item.name == name then
        return case_item.selector, nil
      end
    end
  end

  return nil, ("no Riot test named '%s' found in current file"):format(name)
end

local function nearest_bench_selector(bufnr)
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    return nil, "current buffer is not backed by a file"
  end

  local suites, err = list_benchmarks(path, { current_package = true })
  if not suites then
    return nil, err or "failed to list Riot benchmarks"
  end

  local matching_suites = suites_for_current_file(path, suites)
  local name = nearest_named_entry(bufnr, {
    'Bench%.[%w_]+.-"([^"]+)"',
  })

  if not name then
    return nil, "no nearby Riot benchmark found"
  end

  for _, suite in ipairs(matching_suites) do
    for _, item in ipairs(suite.items) do
      if item.name == name then
        return item.selector, nil
      end
    end
  end

  return nil, ("no Riot benchmark named '%s' found in current file"):format(name)
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

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("riot.nvim.lsp.attach", { clear = true }),
    pattern = { "*.ml", "*.mli" },
    desc = "Configure buffers attached to riot-lsp",
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      if client and client.name == "riot-lsp" then
        attach_riot_lsp_buffer(client, args.buf)
      end
    end,
  })
end

function M.setup(opts)
  state.config = merge_config(opts)
  configure_lsp_autostart()
  configure_format_on_save()

  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(bufnr) then
      local path = current_buffer_path(bufnr)
      if type(path) == "string" and supported_file(path) then
        configure_riot_lsp_buffer(bufnr)
        for _, client in ipairs(riot_lsp_clients(bufnr)) do
          attach_riot_lsp_buffer(client, bufnr)
        end
      end
    end
  end
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

function M.fix_all()
  local bufnr = vim.api.nvim_get_current_buf()

  if start_riot_lsp(bufnr) then
    vim.lsp.buf.code_action({
      apply = true,
      context = {
        only = { "source.fixAll" },
      },
    })
    return true
  end

  local result = run_fix(bufnr)
  set_diagnostics(bufnr, result.items)
  if result.ok then
    reload_buffer(bufnr)
  end
  return result.ok
end

function M.explain_current_diagnostic()
  return explain_current_diagnostic()
end

function M.show_logs()
  local lines = #state.logs > 0 and state.logs or { "No Riot logs yet." }
  open_scratch("riot://logs", lines, { height = state.config.logs_height })
end

function M.show_lsp_logs()
  local server_log = riot_lsp_server_log_path()

  open_file_in_split(server_log, { height = state.config.logs_height })

  local client_log = nvim_lsp_log_path()
  if type(client_log) == "string" and client_log ~= "" and client_log ~= server_log then
    open_file_in_split(client_log, { height = state.config.logs_height })
  elseif client_log == nil then
    notify("this Neovim build does not expose an LSP log path", vim.log.levels.WARN)
  end
end

function M.start_lsp()
  local bufnr = vim.api.nvim_get_current_buf()
  if not start_riot_lsp(bufnr) then
    notify("failed to start riot-lsp for current buffer", vim.log.levels.WARN)
    return false
  end

  notify("riot-lsp started")
  return true
end

function M.stop_lsp()
  local clients = vim.lsp.get_clients({ name = "riot-lsp" })
  if #clients == 0 then
    notify("riot-lsp is not running", vim.log.levels.WARN)
    return false
  end

  vim.lsp.stop_client(vim.tbl_map(function(client)
    return client.id
  end, clients), true)
  notify("riot-lsp stopped")
  return true
end

function M.restart_lsp()
  M.stop_lsp()
  return M.start_lsp()
end

function M.show_lsp_info()
  local lines = {}
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ name = "riot-lsp" })
  local buffer_clients = riot_lsp_clients(bufnr)

  if #clients == 0 then
    table.insert(lines, "riot-lsp is not running")
  else
    for _, client in ipairs(clients) do
      table.insert(lines, ("Client %d"):format(client.id))
      table.insert(lines, ("  root: %s"):format(client.config.root_dir or "<none>"))
      table.insert(lines, ("  cmd: %s"):format(shell_join(client.config.cmd or {})))
    end
  end

  table.insert(lines, "")
  table.insert(lines, ("current buffer: %d"):format(bufnr))
  table.insert(lines, ("attached clients: %d"):format(#buffer_clients))
  table.insert(lines, ("omnifunc: %s"):format(vim.bo[bufnr].omnifunc ~= "" and vim.bo[bufnr].omnifunc or "<unset>"))

  table.insert(lines, "")
  table.insert(lines, ("riot-lsp log: %s"):format(riot_lsp_server_log_path()))
  table.insert(lines, ("nvim lsp log: %s"):format(nvim_lsp_log_path() or "<unavailable>"))

  open_scratch("riot://lsp-info", lines, { height = state.config.logs_height })
end

function M.build_current_target()
  local path, context = current_context()
  local args = { "build" }

  if context.package then
    table.insert(args, context.package.name)
  end

  run_riot_terminal(args, { path = path, title = "Riot Build" })
end

function M.check_current_target()
  local path, context = current_context()
  local args = { "check" }

  if type(path) == "string" and supported_file(path) then
    table.insert(args, path)
  elseif context.package then
    table.insert(args, "-p")
    table.insert(args, context.package.name)
  end

  run_riot_terminal(args, { path = path, title = "Riot Check" })
end

function M.test_workspace()
  local path = current_buffer_path(vim.api.nvim_get_current_buf())
  run_riot_terminal({ "test" }, { path = path, title = "Riot Test" })
end

function M.test_package()
  local path, context = current_context()
  if not context.package then
    notify("no Riot package found for current buffer", vim.log.levels.WARN)
    return false
  end

  run_riot_terminal({ "test", "-p", context.package.name }, {
    path = path,
    title = "Riot Test",
  })
  return true
end

function M.test_file()
  local bufnr = vim.api.nvim_get_current_buf()
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    notify("current buffer is not backed by a file", vim.log.levels.WARN)
    return false
  end

  local suites, err = list_test_cases(path, { current_package = true })
  if not suites then
    notify(err or "failed to list Riot tests", vim.log.levels.ERROR)
    return false
  end

  local matching_suites = suites_for_current_file(path, suites)
  select_item(matching_suites, {
    prompt = "Riot test suite",
    empty_message = "no Riot test suites found for current file",
    format_item = function(item)
      return ("%s (%s)"):format(item.selector, item.path or "")
    end,
  }, function(item)
    if item then
      run_test_selector(item.selector, path)
    end
  end)

  return true
end

function M.test_nearest()
  local bufnr = vim.api.nvim_get_current_buf()
  local path = current_buffer_path(bufnr)
  local selector, err = nearest_test_selector(bufnr)
  if not selector then
    notify(err or "failed to resolve Riot test", vim.log.levels.WARN)
    return false
  end

  run_test_selector(selector, path)
  return true
end

function M.test_current_target()
  local bufnr = vim.api.nvim_get_current_buf()
  local path = current_buffer_path(bufnr)

  if type(path) == "string" and path:match("/tests/") then
    return M.test_file()
  end

  return M.test_package() or M.test_workspace()
end

function M.bench_current_target()
  pick_bench_for_context(vim.api.nvim_get_current_buf(), true)
end

function M.bench_package()
  pick_bench_for_context(vim.api.nvim_get_current_buf(), true)
end

function M.bench_file()
  local bufnr = vim.api.nvim_get_current_buf()
  local path = current_buffer_path(bufnr)
  if type(path) ~= "string" then
    notify("current buffer is not backed by a file", vim.log.levels.WARN)
    return false
  end

  local suites, err = list_benchmarks(path, { current_package = true })
  if not suites then
    notify(err or "failed to list Riot benchmarks", vim.log.levels.ERROR)
    return false
  end

  local matching_suites = suites_for_current_file(path, suites)
  local items = flatten_bench_items(matching_suites)
  select_item(items, {
    prompt = "Riot benchmark",
    empty_message = "no Riot benchmarks found for current file",
    format_item = function(item)
      return ("%s [%s]"):format(item.selector, item.kind)
    end,
  }, function(item)
    if item then
      run_bench_selector(item.selector, path)
    end
  end)

  return true
end

function M.bench_nearest()
  local bufnr = vim.api.nvim_get_current_buf()
  local path = current_buffer_path(bufnr)
  local selector, err = nearest_bench_selector(bufnr)
  if not selector then
    notify(err or "failed to resolve Riot benchmark", vim.log.levels.WARN)
    return false
  end

  run_bench_selector(selector, path)
  return true
end

function M.bench_last()
  if not state.last_bench_selector then
    notify("no Riot benchmark has been run yet", vim.log.levels.WARN)
    return false
  end

  run_bench_selector(state.last_bench_selector, current_buffer_path(vim.api.nvim_get_current_buf()))
  return true
end

function M.run_runnable(kind)
  run_selected_runnable(kind)
end

function M.add_dependency(dependency)
  local dep = dependency
  if dep == nil or dep == "" then
    dep = vim.fn.input("riot add: ")
  end

  if dep == nil or dep == "" then
    return false
  end

  local path, context = current_context()
  run_riot_terminal({ "add", dep }, {
    cwd = context.package and context.package.root or context.cwd,
    path = path,
    title = "Riot Add",
  })
  return true
end

function M.remove_dependency(dependency)
  local dep = dependency
  if dep == nil or dep == "" then
    dep = vim.fn.input("riot rm: ")
  end

  if dep == nil or dep == "" then
    return false
  end

  local path, context = current_context()
  run_riot_terminal({ "rm", dep }, {
    cwd = context.package and context.package.root or context.cwd,
    path = path,
    title = "Riot Remove",
  })
  return true
end

M.diagnostics_namespace = diagnostics_namespace

return M
