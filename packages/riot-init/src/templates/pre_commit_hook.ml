open Std

let materialize = fun (config: Context.t) -> Writer.write_file config ~relative_path:".githooks/pre-commit" ~content:Template_assets.pre_commit_hook ~executable:true
