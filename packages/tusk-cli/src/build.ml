open Std
open Std.Collections
open Tusk_model
open Tusk_server

let command =
  let open ArgParser in
  let open Arg in
  command "build" |> about "Build packages"
  |> args
       [
         option "package" |> short 'p' |> long "package"
         |> help "Build only the specified package";
       ]

let build_command package_opt =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd
    |> Result.expect
         ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in

  let client =
    Tusk_server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  let request =
    match package_opt with
    | Some pkg -> Tusk_client.BuildPackage pkg
    | None -> Tusk_client.BuildAll
  in
  let displayed_packages = HashSet.create () in
  let result =
    Tusk_client.build_streaming client request (fun event ->
        match event with
        | Tusk_client.BuildStarted session_id -> ()
        | Tusk_client.BuildEvent event ->
            let msg = Event_formatter.format ~displayed_packages event in
            if msg <> "" then println "%s" msg
        | Tusk_client.BuildCompleted _ -> ()
        | Tusk_client.BuildFailed _ -> ())
    |> Result.expect ~msg:"Build failed"
  in
  Tusk_client.close client;

  match result with
  | Tusk_client.BuildCompleted _ -> Ok ()
  | Tusk_client.BuildFailed { errors; stats; _ } ->
      (* Get names of failed packages *)
      let failed_packages =
        List.filter_map
          (fun (r : Tusk_protocol.WireProtocol.build_result) ->
            match r.status with
            | Tusk_protocol.WireProtocol.Failed _ -> Some r.package.name
            | _ -> None)
          errors
      in

      (* Don't print redundant error - failures were already shown during build *)
      Error (Failure "Build failed")
  | Tusk_client.BuildStarted _ | Tusk_client.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  build_command package_opt
