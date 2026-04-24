open Std

let materialize = fun (config: Context.t) ->
  let content = {|[workspace]
name = "|} ^ config.workspace_name ^ {|"
members = [
  "packages/|} ^ config.package_name ^ {|",
]

[dependencies]
# Shared external dependencies

[profile.debug]
kind = "native"
|}
  in
  Writer.write_file config ~relative_path:"riot.toml" ~content ~executable:false
