if vim.g.loaded_riot_nvim == 1 then
  return
end

vim.g.loaded_riot_nvim = 1

local riot = require("riot")

local function command(name, method, opts)
  opts = opts or {}

  vim.api.nvim_create_user_command(name, function(ctx)
    riot[method](ctx.args)
  end, opts)
end

command("RiotFmt", "format_current_buffer", {
  desc = "Format the current file with riot fmt",
  nargs = 0,
})

command("RiotFix", "fix_current_diagnostic", {
  desc = "Apply Riot quick fixes for the diagnostic under the cursor",
  nargs = 0,
})

command("RiotFixAll", "fix_all", {
  desc = "Apply Riot fix-all actions for the current file",
  nargs = 0,
})

command("RiotExplain", "explain_current_diagnostic", {
  desc = "Explain the Riot diagnostic under the cursor",
  nargs = 0,
})

command("RiotPackage", "show_current_package", {
  desc = "Show the Riot package for the current file",
  nargs = 0,
})

command("RiotBuild", "build_current_target", {
  desc = "Build the current Riot package or workspace",
  nargs = 0,
})

command("RiotCheck", "check_current_target", {
  desc = "Typecheck the current file, package, or workspace",
  nargs = 0,
})

command("RiotRun", "run_runnable", {
  desc = "Pick and run a Riot runnable",
  nargs = 0,
})

vim.api.nvim_create_user_command("RiotRunBinary", function()
  riot.run_runnable("binary")
end, {
  desc = "Pick and run a Riot binary",
  nargs = 0,
})

vim.api.nvim_create_user_command("RiotRunExample", function()
  riot.run_runnable("example")
end, {
  desc = "Pick and run a Riot example",
  nargs = 0,
})

command("RiotTest", "test_current_target", {
  desc = "Run Riot tests for the current package or workspace",
  nargs = 0,
})

command("RiotTestWorkspace", "test_workspace", {
  desc = "Run Riot tests for the workspace",
  nargs = 0,
})

command("RiotTestPackage", "test_package", {
  desc = "Run Riot tests for the current package",
  nargs = 0,
})

command("RiotTestFile", "test_file", {
  desc = "Pick and run a Riot test suite from the current file",
  nargs = 0,
})

command("RiotTestNearest", "test_nearest", {
  desc = "Run the nearest Riot test in the current file",
  nargs = 0,
})

command("RiotBench", "bench_current_target", {
  desc = "Pick and run a Riot benchmark from the current package",
  nargs = 0,
})

command("RiotBenchPackage", "bench_package", {
  desc = "Pick and run a Riot benchmark from the current package",
  nargs = 0,
})

command("RiotBenchFile", "bench_file", {
  desc = "Pick and run a Riot benchmark from the current file",
  nargs = 0,
})

command("RiotBenchNearest", "bench_nearest", {
  desc = "Run the nearest Riot benchmark in the current file",
  nargs = 0,
})

command("RiotBenchLast", "bench_last", {
  desc = "Re-run the last Riot benchmark selector",
  nargs = 0,
})

command("RiotAdd", "add_dependency", {
  desc = "Add a Riot dependency to the current package",
  nargs = "?",
})

command("RiotRemove", "remove_dependency", {
  desc = "Remove a Riot dependency from the current package",
  nargs = "?",
})

command("RiotLogs", "show_logs", {
  desc = "Show riot.nvim logs",
  nargs = 0,
})

command("RiotLspLogs", "show_lsp_logs", {
  desc = "Open the Neovim LSP log",
  nargs = 0,
})

command("RiotLspStart", "start_lsp", {
  desc = "Start riot-lsp for the current buffer",
  nargs = 0,
})

command("RiotLspStop", "stop_lsp", {
  desc = "Stop all riot-lsp clients",
  nargs = 0,
})

command("RiotLspRestart", "restart_lsp", {
  desc = "Restart riot-lsp for the current buffer",
  nargs = 0,
})

command("RiotLspInfo", "show_lsp_info", {
  desc = "Show riot-lsp client information",
  nargs = 0,
})

riot.setup()
