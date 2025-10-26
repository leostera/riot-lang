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
  let displayed_packages = HashMap.create () in
  let result =
    Tusk_client.build_streaming client request (fun event ->
        match event with
        | Tusk_client.BuildStarted session_id -> ()
        | Tusk_client.BuildEvent _event -> ()
        | Tusk_client.BuildCompleted _ -> ()
        | Tusk_client.BuildFailed _ -> ())
    |> Result.expect ~msg:"Build failed"
  in
  Tusk_client.close client;

  match result with
  | Tusk_client.BuildCompleted _ -> Ok ()
  | Tusk_client.BuildFailed { errors; stats; _ } ->
      (match errors with
      | [] ->
          println "error: build failed: %d packages failed"
            stats.packages_failed
      | errs ->
          println "error: build failed: %d packages failed" (List.length errs);
          List.iter
            (fun (r : Tusk_protocol.WireProtocol.build_result) ->
              match r.status with
              | Tusk_protocol.WireProtocol.Failed err ->
                  let error_msg =
                    match err with
                    | Tusk_protocol.WireProtocol.PlanningFailed planning_err ->
                        Tusk_planner.Planning_error.to_string planning_err
                    | Tusk_protocol.WireProtocol.ExecutionFailed { message } ->
                        message
                    | Tusk_protocol.WireProtocol.ActionFailed action_err ->
                        Tusk_executor.Package_builder.package_error_to_string
                          (Tusk_executor.Package_builder.ActionFailed action_err)
                  in
                  println "  %s: %s" r.package.name error_msg
              | _ -> ())
            errs);
      Error (Failure "Build failed")
  | Tusk_client.BuildStarted _ | Tusk_client.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  build_command package_opt
