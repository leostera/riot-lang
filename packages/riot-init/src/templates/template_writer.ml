open Std
open Std.Result.Syntax

let emit_created = fun (config: Template_config.t) path ->
  Riot_init_types.emit ~on_event:config.on_event (ScaffoldCreated { path })

let write_file = fun ?(emit = true) (config: Template_config.t) ~relative_path ~content ~executable ->
  let path = Path.(config.target_dir / Path.v relative_path) in
  let* () =
    match Path.parent path with
    | None -> Ok ()
    | Some parent -> Fs.create_dir_all parent
    |> Result.map_err ~fn:(fun err -> "Failed to create template directory: " ^ IO.error_message err)
  in
  let* () = Fs.write content path
  |> Result.map_err
    ~fn:(fun err -> "Failed to create " ^ relative_path ^ ": " ^ IO.error_message err) in
  let* () =
    if executable then
      Fs.set_permissions path Fs.Permissions.executable
      |> Result.map_err
        ~fn:(fun err -> "Failed to make " ^ relative_path ^ " executable: " ^ IO.error_message err)
    else
      Ok ()
  in
  if emit then
    emit_created config relative_path;
  Ok ()
