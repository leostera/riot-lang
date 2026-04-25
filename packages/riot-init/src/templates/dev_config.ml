open Std

let materialize = fun (config: Context.t) -> Writer.write_file config ~relative_path:"config/dev.toml" ~content:(Template_assets.dev_config_toml ~workspace_name:config.workspace_name) ~executable:false
