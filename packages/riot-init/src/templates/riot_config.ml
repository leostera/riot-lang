open Std

let materialize = fun (config: Context.t) -> Writer.write_file config ~relative_path:".riot/config.toml" ~content:Template_assets.workspace_riot_config_toml ~executable:false
