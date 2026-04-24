open Std

let materialize = fun (config: Template_config.t) ->
  Template_writer.write_file
    config
    ~relative_path:".riot/config.toml"
    ~content:Template_assets.workspace_riot_config_toml
    ~executable:false
