vim.bo.commentstring = "// %s"

if vim.bo.syntax == "" or vim.bo.syntax == "ocaml" then
  vim.bo.syntax = "tinyml"
end
