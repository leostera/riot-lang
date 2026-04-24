open Std

let materialize = fun (config: Template_config.t) ->
  Template_writer.write_file
    config
    ~relative_path:".githooks/pre-commit"
    ~content:Template_assets.pre_commit_hook
    ~executable:true
