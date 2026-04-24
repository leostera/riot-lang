open Std
open Std.Result.Syntax

let materialize = fun (config: Template_config.t) ->
  Template_assets.riot_skill_files |> List.fold_left ~init:(Ok ())
    ~fn:(fun acc (file: Template_assets.file) ->
      let* () = acc in
      Template_writer.write_file
        config
        ~relative_path:file.relative_path
        ~content:file.content
        ~executable:file.executable)
