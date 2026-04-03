if vim.g.loaded_riot_nvim == 1 then
  return
end

vim.g.loaded_riot_nvim = 1

vim.api.nvim_create_user_command("RiotFmt", function()
  require("riot").format_current_buffer()
end, {
  desc = "Format the current file with riot fmt",
})

vim.api.nvim_create_user_command("RiotPackage", function()
  require("riot").show_current_package()
end, {
  desc = "Show the Riot package for the current file",
})

vim.api.nvim_create_user_command("RiotFix", function()
  require("riot").fix_current_diagnostic()
end, {
  desc = "Apply Riot quick fixes for the diagnostic under the cursor",
})

require("riot").setup()
