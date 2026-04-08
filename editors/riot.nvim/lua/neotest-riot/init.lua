local nio = require("nio")
local Tree = require("neotest.types").Tree

---@class neotest.Adapter
local adapter = { name = "neotest-riot" }

local config = {
  riot_cmd = { "riot" },
}

local cache = {
  workspace_info = {},
  suite_lists = {},
}

local function normalize_riot_cmd(riot_cmd)
  if type(riot_cmd) == "string" and riot_cmd ~= "" then
    return { riot_cmd }
  end

  if type(riot_cmd) == "table" and #riot_cmd > 0 then
    return vim.deepcopy(riot_cmd)
  end

  return { "riot" }
end

local function json_value(value)
  if value == vim.NIL then
    return nil
  end

  return value
end

local function riot_command(args)
  local command = vim.deepcopy(config.riot_cmd)
  vim.list_extend(command, args)
  return command
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

local function run_json_command(command, cwd)
  local result = vim.system(command, {
    cwd = cwd,
    text = true,
  }):wait()

  local events = {}
  for _, line in ipairs(vim.split(result.stdout or "", "\n", { trimempty = true })) do
    local ok, decoded = pcall(vim.json.decode, line)
    if ok and type(decoded) == "table" then
      table.insert(events, decoded)
    end
  end

  return result, events
end

local function decode_jsonl(text)
  local events = {}

  for _, line in ipairs(vim.split(text or "", "\n", { trimempty = true })) do
    local ok, decoded = pcall(vim.json.decode, line)
    if ok and type(decoded) == "table" then
      table.insert(events, decoded)
    end
  end

  return events
end

local await_system = nio.wrap(function(command, cwd, cb)
  vim.system(command, {
    cwd = cwd,
    text = true,
  }, cb)
end, 3)

local function run_json_command_async(command, cwd)
  local result = await_system(command, cwd)
  return result, decode_jsonl(result.stdout)
end

local function cache_root_key(path)
  local root = adapter.root(path_directory(path) or vim.uv.cwd())
  return normalized_path(root or path_directory(path) or vim.uv.cwd())
end

local function workspace_info(path)
  local cache_key = cache_root_key(path)
  if cache_key and cache.workspace_info[cache_key] ~= nil then
    return cache.workspace_info[cache_key] or nil
  end

  local cwd = path_directory(path) or vim.uv.cwd()
  local result, events = run_json_command(riot_command({ "info", "--json" }), cwd)
  local event = events[1]

  if result.code ~= 0 or type(event) ~= "table" or event.type ~= "workspace_info" then
    if cache_key then
      cache.workspace_info[cache_key] = false
    end
    return nil
  end

  if cache_key then
    cache.workspace_info[cache_key] = event
  end

  return event
end

local function workspace_info_async(path)
  local cache_key = cache_root_key(path)
  if cache_key and cache.workspace_info[cache_key] ~= nil then
    return cache.workspace_info[cache_key] or nil
  end

  local cwd = path_directory(path) or vim.uv.cwd()
  local result, events = run_json_command_async(riot_command({ "info", "--json" }), cwd)
  local event = events[1]

  if result.code ~= 0 or type(event) ~= "table" or event.type ~= "workspace_info" then
    if cache_key then
      cache.workspace_info[cache_key] = false
    end
    return nil
  end

  if cache_key then
    cache.workspace_info[cache_key] = event
  end

  return event
end

local function suite_cache_key(info, pkg)
  local root = info and json_value(info.root)
  if type(root) ~= "string" or root == "" then
    return nil
  end

  return ("%s::%s"):format(root, pkg and pkg.name or "__workspace__")
end

local function suite_lists(info, pkg)
  local key = suite_cache_key(info, pkg)
  if key and cache.suite_lists[key] ~= nil then
    return cache.suite_lists[key] or {}
  end

  local command = riot_command({ "test", "--list", "--json" })
  if pkg then
    table.insert(command, "-p")
    table.insert(command, pkg.name)
  end

  local result, events = run_json_command(command, json_value(info.root))
  if result.code ~= 0 then
    if key then
      cache.suite_lists[key] = false
    end
    return {}
  end

  local grouped = {}
  local selector_map = {}

  for _, event in ipairs(events) do
    if event.type == "TestSuiteListed" then
      local absolute_path = normalized_path(json_value(event.path), json_value(info.root))
      if absolute_path then
        grouped[absolute_path] = grouped[absolute_path] or {}
        local suite = {
          package = json_value(event.package),
          name = json_value(event.suite),
          selector = json_value(event.selector),
          path = absolute_path,
          cases = {},
        }
        table.insert(grouped[absolute_path], suite)
        selector_map[suite.selector] = suite
      end
    elseif event.type == "TestCaseListed" then
      local suite_selector = ("%s:%s"):format(event.package, event.suite)
      local suite = selector_map[suite_selector]
      if suite then
        table.insert(suite.cases, {
          name = json_value(event.name),
          selector = json_value(event.selector),
        })
      end
    end
  end

  if key then
    cache.suite_lists[key] = grouped
  end

  return grouped
end

local function suite_lists_async(info, pkg)
  local key = suite_cache_key(info, pkg)
  if key and cache.suite_lists[key] ~= nil then
    return cache.suite_lists[key] or {}
  end

  local command = riot_command({ "test", "--list", "--json" })
  if pkg then
    table.insert(command, "-p")
    table.insert(command, pkg.name)
  end

  local result, events = run_json_command_async(command, json_value(info.root))
  if result.code ~= 0 then
    if key then
      cache.suite_lists[key] = false
    end
    return {}
  end

  local grouped = {}
  local selector_map = {}

  for _, event in ipairs(events) do
    if event.type == "TestSuiteListed" then
      local absolute_path = normalized_path(json_value(event.path), json_value(info.root))
      if absolute_path then
        grouped[absolute_path] = grouped[absolute_path] or {}
        local suite = {
          package = json_value(event.package),
          name = json_value(event.suite),
          selector = json_value(event.selector),
          path = absolute_path,
          cases = {},
        }
        table.insert(grouped[absolute_path], suite)
        selector_map[suite.selector] = suite
      end
    elseif event.type == "TestCaseListed" then
      local suite_selector = ("%s:%s"):format(event.package, event.suite)
      local suite = selector_map[suite_selector]
      if suite then
        table.insert(suite.cases, {
          name = json_value(event.name),
          selector = json_value(event.selector),
        })
      end
    end
  end

  if key then
    cache.suite_lists[key] = grouped
  end

  return grouped
end

local function current_package(info, path)
  if type(info) ~= "table" or type(info.packages) ~= "table" then
    return nil
  end

  local target_path = normalized_path(path)
  if not target_path then
    return nil
  end

  for _, pkg in ipairs(info.packages) do
    local root = normalized_path(json_value(pkg.root))
    if root and vim.startswith(target_path, root) then
      return {
        name = json_value(pkg.name),
        root = root,
      }
    end
  end

  return nil
end

local function read_lines(path)
  local ok, lines = pcall(vim.fn.readfile, path)
  if not ok then
    return {}
  end

  return lines
end

local function line_for_name(lines, marker, name)
  for index = 1, #lines do
    local stop = math.min(index + 3, #lines)
    local block = table.concat(vim.list_slice(lines, index, stop), "\n")
    if block:find(marker, 1, true) and block:find(name, 1, true) then
      return index - 1
    end
  end

  return 0
end

local function list_suites_for_file(file_path)
  local info = workspace_info(file_path)
  if not info then
    return {}, nil
  end

  local pkg = current_package(info, file_path)
  local grouped = suite_lists(info, pkg)
  return vim.deepcopy(grouped[normalized_path(file_path)] or {}), info
end

local function list_suites_for_file_async(file_path)
  local info = workspace_info_async(file_path)
  if not info then
    return {}, nil
  end

  local pkg = current_package(info, file_path)
  local grouped = suite_lists_async(info, pkg)
  return vim.deepcopy(grouped[normalized_path(file_path)] or {}), info
end

local function positions_for_file(file_path)
  local suites = list_suites_for_file(file_path)
  if #suites == 0 then
    return nil
  end

  local lines = read_lines(file_path)
  local last_line = math.max(#lines - 1, 0)
  local positions = {
    {
      id = file_path,
      name = vim.fs.basename(file_path),
      path = file_path,
      type = "file",
      range = { 0, 0, last_line, 0 },
    },
  }

  for _, suite in ipairs(suites) do
    local suite_line = 0
    if suite.cases[1] then
      suite_line = line_for_name(lines, "Test.", suite.cases[1].name)
    end

    table.insert(positions, {
      id = suite.selector,
      parent_id = file_path,
      name = suite.name,
      path = file_path,
      type = "namespace",
      range = { suite_line, 0, suite_line, 0 },
    })

    for _, case_item in ipairs(suite.cases) do
      local case_line = line_for_name(lines, "Test.", case_item.name)
      table.insert(positions, {
        id = case_item.selector,
        parent_id = suite.selector,
        name = case_item.name,
        path = file_path,
        type = "test",
        range = { case_line, 0, case_line, 0 },
      })
    end
  end

  return Tree.from_list(positions, function(pos)
    return pos.id
  end)
end

local function shell_join(command)
  return table.concat(vim.tbl_map(vim.fn.shellescape, command), " ")
end

local function command_for_selector(root, selector)
  return ("cd %s && %s"):format(vim.fn.shellescape(root), shell_join(riot_command({
    "test",
    "--json",
    selector,
  })))
end

local function output_text(result)
  if type(result.output) == "string" and vim.fn.filereadable(result.output) == 1 then
    return table.concat(vim.fn.readfile(result.output), "\n")
  end

  if type(result.output) == "string" then
    return result.output
  end

  return ""
end

local function parse_result_events(result)
  local events = {}
  for _, line in ipairs(vim.split(output_text(result), "\n", { trimempty = true })) do
    local ok, decoded = pcall(vim.json.decode, line)
    if ok and type(decoded) == "table" then
      table.insert(events, decoded)
    end
  end
  return events
end

local function result_status(status)
  if status == "passed" then
    return "passed"
  end

  if status == "skipped" then
    return "skipped"
  end

  return "failed"
end

function adapter.root(dir)
  local manifests = vim.fs.find("riot.toml", {
    path = dir,
    upward = true,
    type = "file",
  })

  if #manifests == 0 then
    return nil
  end

  return vim.fs.dirname(manifests[1])
end

function adapter.filter_dir(name)
  return name ~= ".git"
    and name ~= ".worktrees"
    and name ~= "_build"
    and name ~= "target"
end

function adapter.is_test_file(file_path)
  return file_path:match("%.ml$") ~= nil
    and (file_path:match("/tests/") ~= nil or file_path:match("_tests%.ml$") ~= nil)
end

---@async
function adapter.discover_positions(file_path)
  local suites = list_suites_for_file_async(file_path)
  if #suites == 0 then
    return nil
  end

  local lines = read_lines(file_path)
  local last_line = math.max(#lines - 1, 0)
  local positions = {
    {
      id = file_path,
      name = vim.fs.basename(file_path),
      path = file_path,
      type = "file",
      range = { 0, 0, last_line, 0 },
    },
  }

  for _, suite in ipairs(suites) do
    local suite_line = 0
    if suite.cases[1] then
      suite_line = line_for_name(lines, "Test.", suite.cases[1].name)
    end

    table.insert(positions, {
      id = suite.selector,
      parent_id = file_path,
      name = suite.name,
      path = file_path,
      type = "namespace",
      range = { suite_line, 0, suite_line, 0 },
    })

    for _, case_item in ipairs(suite.cases) do
      local case_line = line_for_name(lines, "Test.", case_item.name)
      table.insert(positions, {
        id = case_item.selector,
        parent_id = suite.selector,
        name = case_item.name,
        path = file_path,
        type = "test",
        range = { case_line, 0, case_line, 0 },
      })
    end
  end

  return Tree.from_list(positions, function(pos)
    return pos.id
  end)
end

function adapter.build_spec(args)
  local position = args.tree:data()
  local info = workspace_info(position.path)
  local root = info and json_value(info.root) or adapter.root(vim.fs.dirname(position.path))
  if not root then
    return nil
  end

  local positions = args.tree:to_list()

  if position.type == "test" or position.type == "namespace" then
    return {
      command = command_for_selector(root, position.id),
      context = {
        position_id = position.id,
        positions = positions,
      },
    }
  end

  if position.type == "file" then
    local specs = {}

    for _, suite in ipairs(list_suites_for_file(position.path)) do
      local suite_positions = {}
      for _, candidate in ipairs(positions) do
        if candidate.id == suite.selector or candidate.parent_id == suite.selector then
          table.insert(suite_positions, candidate)
        end
      end

      table.insert(specs, {
        command = command_for_selector(root, suite.selector),
        context = {
          position_id = suite.selector,
          positions = suite_positions,
        },
      })
    end

    return specs
  end

  return nil
end

function adapter.results(spec, result)
  local output = output_text(result)
  local events = parse_result_events(result)
  local mapped = {}
  local position_ids = {}
  local context = type(spec.context) == "table" and spec.context or {}

  for _, pos in ipairs(context.positions or {}) do
    position_ids[pos.id] = pos
  end

  for _, event in ipairs(events) do
    if event.type == "SuiteCompleted" then
      local suite_selector = ("%s:%s"):format(event.package, event.suite)
      local summary = json_value(event.summary) or {}
      local suite_failed = (json_value(summary.failed) or 0) > 0 or result.code ~= 0
      local suite_skipped = (json_value(summary.skipped) or 0) == (json_value(summary.total) or 0)

      mapped[suite_selector] = {
        status = suite_failed and "failed" or (suite_skipped and "skipped" or "passed"),
        output = result.output,
      }

      for _, test_event in ipairs(json_value(event.tests) or {}) do
        local selector = suite_selector .. ":" .. test_event.name
        mapped[selector] = {
          status = result_status(test_event.status),
          output = result.output,
          short = test_event.message,
          errors = test_event.message and { { message = test_event.message } } or nil,
        }
      end
    end
  end

  if vim.tbl_isempty(mapped) then
    local root_position = context.positions and context.positions[1]
    if root_position then
      mapped[root_position.id] = {
        status = result.code == 0 and "passed" or "failed",
        output = result.output,
        short = output ~= "" and output or nil,
      }
    elseif type(context.position_id) == "string" then
      mapped[context.position_id] = {
        status = result.code == 0 and "passed" or "failed",
        output = result.output,
        short = output ~= "" and output or nil,
      }
    end
  end

  return mapped
end

setmetatable(adapter, {
  __call = function(_, user_config)
    config = vim.tbl_deep_extend("force", config, user_config or {})
    config.riot_cmd = normalize_riot_cmd(config.riot_cmd)
    return adapter
  end,
})

return adapter
