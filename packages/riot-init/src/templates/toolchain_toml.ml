open Std

let materialize = fun (config: Template_config.t) ->
  Template_writer.write_file config ~relative_path:"ocaml-toolchain.toml"
    ~content:{|[toolchain]
version = "5.5.0-riot.4"
|}
    ~executable:false
