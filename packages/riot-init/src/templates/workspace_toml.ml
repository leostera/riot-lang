open Std

let materialize = fun (config: Template_config.t) ->
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
  Template_writer.write_file config ~relative_path:"riot.toml" ~content ~executable:false
