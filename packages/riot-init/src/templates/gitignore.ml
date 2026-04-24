open Std

let materialize = fun (config: Template_config.t) ->
  Template_writer.write_file config ~relative_path:".gitignore"
    ~content:{|# Riot build artifacts
_build
|}
    ~executable:false
