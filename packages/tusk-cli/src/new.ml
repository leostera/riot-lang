open Std
open Tusk_server

let command =
  let open ArgParser in
  let open Arg in
  command "new"
  |> about "Create a new package"
  |> args
       [
         positional "path" |> help "Path for new package";
         flag "lib" |> long "lib" |> help "Create a library package (default)";
         flag "bin" |> long "bin" |> help "Create a binary package";
       ]

let run matches =
  let open ArgParser in
  let path = get_one matches "path" |> Option.expect ~msg:"path required" in
  let is_library = if get_flag matches "bin" then false else true in

  let path_obj = match Path.of_string path with
    | Ok p -> p
    | Error _ -> Path.v path
  in
  let name = Path.basename path_obj in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Tusk_model.Workspace_manager.scan cwd
    |> Result.expect ~msg:"Failed to scan workspace"
  in

  match Tusk_server.Server_manager.ensure_running ~workspace with
  | Ok client -> (
      match Tusk_client.new_package client ~path ~name ~is_library with
      | Ok (created_path, created_name) ->
          println "Package '%s' created at '%s'" created_name created_path;
          Ok ()
      | Error e -> Error (Failure ("Package creation failed: " ^ e)))
  | Error _e -> Error (Failure "Failed to connect to server")
