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
  let (workspace, _load_errors) =
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

  (* Track build stats as events arrive *)
  let start_time = Time.Instant.now () in
  let built_count = ref 0 in
  let cached_count = ref 0 in
  let failed_count = ref 0 in
  let skipped_count = ref 0 in

  let result =
    Tusk_client.build_streaming client request (fun event ->
        match event with
        | Tusk_client.BuildStarted session_id -> ()
        | Tusk_client.BuildEvent event ->
            (* Track stats from events *)
            (match event with
            | Tusk_executor.Telemetry_events.BuildCompleted
                { status = `Fresh; _ } ->
                built_count := !built_count + 1
            | Tusk_executor.Telemetry_events.BuildCompleted
                { status = `Cached; _ } ->
                cached_count := !cached_count + 1
            | Tusk_executor.Telemetry_events.BuildFailed _ ->
                failed_count := !failed_count + 1
            | Tusk_executor.Telemetry_events.BuildSkipped _ ->
                skipped_count := !skipped_count + 1
            | _ -> ());

            let msg = Event_formatter.format ~displayed_packages event in
            if msg <> "" then println "%s" msg
        | Tusk_client.BuildCompleted _ -> ()
        | Tusk_client.BuildFailed { errors; _ } ->
            (* Track failed packages *)
            failed_count := !failed_count + List.length errors;
            (* Display error details from failed build *)
            List.iter (fun (error : Tusk_protocol.WireProtocol.build_result) ->
              match error.status with
              | Tusk_protocol.WireProtocol.Failed (Tusk_protocol.WireProtocol.ExecutionFailed { message }) ->
                  println "";
                  println "\027[1;31mError\027[0m: %s" message
              | Tusk_protocol.WireProtocol.Failed (Tusk_protocol.WireProtocol.PlanningFailed _) ->
                  println "";
                  println "\027[1;31mError\027[0m: Planning failed for %s" error.package.name
              | Tusk_protocol.WireProtocol.Failed (Tusk_protocol.WireProtocol.ActionExecutionFailed { message }) ->
                  println "";
                  println "\027[1;31mError\027[0m: Action execution failed for %s: %s" error.package.name message
              | Tusk_protocol.WireProtocol.Failed (Tusk_protocol.WireProtocol.ActionOutputsNotCreated { missing }) ->
                  println "";
                  println "\027[1;31mError\027[0m: Action outputs not created for %s" error.package.name
              | Tusk_protocol.WireProtocol.Failed (Tusk_protocol.WireProtocol.ActionDependenciesFailed _) ->
                  println "";
                  println "\027[1;31mError\027[0m: Action dependencies failed for %s" error.package.name
              | _ -> ()
            ) errors
        | Tusk_client.PlanningFailed { reason; _ } ->
            (* Planning failed before build started - this is a fatal error *)
            println "";
            println "\027[1;31mPlanning Failed\027[0m: %s" reason;
            failed_count := !failed_count + 1
        | Tusk_client.CycleDetected { cycle_nodes; _ } ->
            println "      \027[1;31mError\027[0m: Cyclic dependency detected:";
            println "         %s" (String.concat " ->\n         " cycle_nodes))
  in
  
  let final_event = match result with
  | Error err ->
      Tusk_client.close client;
      (match err with
      | Tusk_client.JsonrpcError json_err ->
          println "\027[1;31mError\027[0m: %s" (Tusk_client.jsonrpc_error_to_string json_err);
          exit 1
      | Tusk_client.PackageNotFound { package_name; available_packages } ->
          println "\027[1;31mError\027[0m: Package '%s' not found" package_name;
          println "";
          println "Available packages:";
          List.iter (fun pkg -> println "  • %s" pkg) available_packages;
          exit 1
      | Tusk_client.UnexpectedEvent { reason; _ } ->
          println "\027[1;31mError\027[0m: %s" reason;
          exit 1)
  | Ok event ->
      Tusk_client.close client;
      event
  in

  (* Print final summary line *)
  let duration =
    Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ())
  in
  let duration_secs = Time.Duration.to_secs_float duration in

  if !failed_count = 0 && !skipped_count = 0 then
    println "    \027[1;32mFinished\027[0m in %.2fs (%d built, %d cached)"
      duration_secs !built_count !cached_count
  else if !failed_count > 0 then
    println
      "    \027[1;31mFinished\027[0m in %.2fs (%d built, %d cached, %d failed, \
       %d skipped)"
      duration_secs !built_count !cached_count !failed_count !skipped_count
  else
    println
      "    \027[1;33mFinished\027[0m in %.2fs (%d built, %d cached, %d skipped)"
      duration_secs !built_count !cached_count !skipped_count;

  match final_event with
  | Tusk_client.BuildCompleted _ -> Ok ()
  | Tusk_client.BuildFailed _ -> Error (Failure "Build failed")
  | Tusk_client.PlanningFailed _ -> Error (Failure "Planning failed")
  | Tusk_client.CycleDetected _ -> Error (Failure "Cyclic dependency detected")
  | Tusk_client.BuildStarted _ | Tusk_client.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  build_command package_opt
