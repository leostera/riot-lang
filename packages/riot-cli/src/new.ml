open Std
open Riot_build

let command =
  let open ArgParser in
    let open Arg in command "new"
    |> about "Create a new package"
    |> args
      [
        positional "path" |> help "Path for new package";
        flag "lib" |> long "lib" |> help "Create a library package (default)";
        flag "bin" |> long "bin" |> help "Create a binary package";
      ]

let run = fun matches ->
  let open ArgParser in
    let path = get_one matches "path" |> Option.expect ~msg:"path required" in
    let is_library =
      if get_flag matches "bin" then
        false
      else
        true
    in
    let path_obj =
      match Path.of_string path with
      | Ok p -> p
      | Error _ -> Path.v path
    in
    let name = Path.basename path_obj in
    let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
    let (workspace, _load_errors) = Riot_model.Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
    match Client.connect_local ~workspace () with
    | Ok client -> (
        let package_kind =
          if is_library then
            "library"
          else
            "binary"
        in
        println ("Creating new " ^ package_kind ^ " '" ^ name ^ "' in '" ^ path ^ "'");
        match Client.new_package client ~path ~name ~is_library with
        | Ok (created_path, created_name) ->
            println
              (String.capitalize_ascii package_kind
              ^ " '"
              ^ created_name
              ^ "' created at '"
              ^ created_path
              ^ "'");
            Ok ()
        | Error e -> Error (Failure ("Package creation failed: " ^ e))
      )
    | Error _e -> Error (Failure "Failed to start local riot session")
