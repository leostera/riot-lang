open Std

let materialize = fun (config: Context.t) ->
  Writer.write_file
    config
    ~relative_path:"ocaml-toolchain.toml"
    ~content:{|[toolchain]
version = "5.5.0-riot.4"
|}
    ~executable:false
