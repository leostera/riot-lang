open Std

let materialize = fun (config: Context.t) -> Writer.write_file config ~relative_path:".gitignore" ~content:{|# Riot build artifacts
_build
|} ~executable:false
