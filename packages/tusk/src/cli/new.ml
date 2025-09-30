open Std
open Server

(** Execute the new command *)
let run args =
  if List.length args < 1 then (
    Printf.eprintf "Error: Package path required\n";
    Printf.eprintf "Usage: tusk new <path> [--lib|--bin]\n";
    Error (Failure "Missing package path"))
  else
    let path = List.nth args 0 in
    let is_library =
      if List.length args > 1 then
        match List.nth args 1 with
        | "--bin" -> false
        | "--lib" -> true
        | _ -> true (* default to library *)
      else true
    in

    (* Extract package name from path *)
    let name = Filename.basename path in

    (* Use server to create the package *)
    let cwd =
      Std.Env.current_dir ()
      |> Std.Result.expect ~msg:"Failed to get current directory"
    in
    let workspace =
      Core.Workspace_manager.scan cwd
      |> Std.Result.expect ~msg:"Failed to scan workspace"
    in

    (* Ensure server is running and create package via client *)
    match Server.Server_manager.ensure_running ~workspace with
    | Ok client -> (
        match
          Tusk_jsonrpc.Client.new_package client ~path ~name ~is_library
        with
        | Ok (created_path, created_name) ->
            Printf.printf "Package '%s' created at '%s'\n%!" created_name
              created_path;
            Ok ()
        | Error e -> Error (Failure ("Package creation failed: " ^ e)))
    | Error _e -> Error (Failure "Failed to connect to server")
