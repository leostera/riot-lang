open Std
open Std.Collections
open Tusk_model
open Tusk_server

(** Helper functions for target resolution *)

let ensure_toolchains_for_targets workspace targets =
  let config = Toolchain_config.from_workspace workspace in
  
  (* Check which toolchains are missing *)
  let missing = List.filter (fun target ->
    match Tusk_toolchain.check_toolchain_status ~version:config.version ~target with
    | Tusk_toolchain.NotInstalled _ | Tusk_toolchain.Incomplete _ -> true
    | Tusk_toolchain.Installed _ -> false
  ) targets in
  
  if List.length missing > 0 then (
    println "";
    println ("📥 Installing " ^ Int.to_string (List.length missing) ^ " missing toolchain(s)...");
    println "";
    
    let host = Tusk_toolchain.get_host_triple () in
    List.iter (fun target ->
      match Tusk_toolchain.download_and_install_toolchain config.version ~host ~target with
      | Ok () -> println ("  ✓ " ^ target)
      | Error msg -> 
          println ("  ✗ " ^ target ^ ": " ^ msg);
          println "";
          println ("❌ Failed to install toolchain for " ^ target);
          exit 1
    ) missing;
    
    println ""
  )

let get_configured_targets workspace =
  let config = Toolchain_config.from_workspace workspace in
  match config.targets with
  | [] -> [Tusk_toolchain.get_host_triple ()]
  | targets -> targets

let resolve_target_pattern workspace pattern =
  let configured = get_configured_targets workspace in
  let host = Tusk_toolchain.get_host_triple () in
  
  match String.lowercase_ascii pattern with
  | "host" | "native" -> Ok [host]
  | "all" -> Ok configured
  | exact when List.mem exact configured -> Ok [exact]
  | pattern ->
      (* Substring matching *)
      let matches = List.filter (fun t ->
        String.contains t pattern
      ) configured in
      if List.length matches = 0 then
        Error ("No targets match pattern '" ^ pattern ^ "'.\n\
                Available targets: " ^ String.concat ", " configured)
      else
        Ok matches

let resolve_targets workspace matches =
  let all_targets = ArgParser.get_flag matches "all-targets" in
  let target_pattern = ArgParser.get_one matches "target" in
  
  if all_targets then
    Ok (get_configured_targets workspace)
  else match target_pattern with
  | Some pattern ->
      resolve_target_pattern workspace pattern
  | None ->
      (* Default to host *)
      Ok [Tusk_toolchain.get_host_triple ()]

let command =
  let open ArgParser in
  let open Arg in
  command "build" |> about "Build packages"
  |> args
       [
         positional "package" |> required false
         |> help "Package to build (or omit to build all packages)";
         option "target"
         |> short 'x'
         |> long "target"
         |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
         flag "all-targets"
         |> help "Build for all configured targets";
       ]

let build_command package_opt target_arch =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd
    |> Result.expect
         ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in

  let client =
    Local_session.connect_local ~workspace
    |> Result.expect ~msg:"Failed to start local tusk session"
  in

  let request =
    match package_opt with
    | Some pkg -> Local_session.BuildPackage pkg
    | None -> Local_session.BuildAll
  in
  let displayed_packages = HashSet.create () in

  (* Track build stats as events arrive *)
  let start_time = Time.Instant.now () in
  let built_count = ref 0 in
  let cached_count = ref 0 in
  let failed_count = ref 0 in
  let skipped_count = ref 0 in

  let result =
    Local_session.build_streaming client request ?target_arch (fun event ->
        match event with
        | Local_session.BuildStarted session_id -> ()
        | Local_session.BuildEvent event ->
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
            if msg != "" then println msg
        | Local_session.BuildCompleted _ -> ()
        | Local_session.BuildFailed { errors; _ } ->
            (* Track failed packages *)
            failed_count := !failed_count + List.length errors;
            (* Display error details from failed build *)
            List.iter (fun (error : Tusk_executor.Package_builder.build_result) ->
              match error.status with
              | Tusk_executor.Package_builder.Failed (Tusk_executor.Package_builder.ExecutionFailed { message }) ->
                  println "";
                  println ("\027[1;31mError\027[0m: " ^ message)
              | Tusk_executor.Package_builder.Failed (Tusk_executor.Package_builder.PlanningFailed _) ->
                  println "";
                  println ("\027[1;31mError\027[0m: Planning failed for " ^ error.package.name)
              | Tusk_executor.Package_builder.Failed (Tusk_executor.Package_builder.ActionExecutionFailed { message }) ->
                  println "";
                  println ("\027[1;31mError\027[0m: Action execution failed for " ^ error.package.name ^ ": " ^ message)
              | Tusk_executor.Package_builder.Failed (Tusk_executor.Package_builder.ActionOutputsNotCreated Module.{ missing }) ->
                  println "";
                  println ("\027[1;31mError\027[0m: Action outputs not created for " ^ error.package.name)
              | Tusk_executor.Package_builder.Failed (Tusk_executor.Package_builder.ActionDependenciesFailed _) ->
                  println "";
                  println ("\027[1;31mError\027[0m: Action dependencies failed for " ^ error.package.name)
              | _ -> ()
            ) errors
        | Local_session.PlanningFailed { reason; _ } ->
            (* Planning failed before build started - this is a fatal error *)
            println "";
            println ("\027[1;31mPlanning Failed\027[0m: " ^ reason);
            failed_count := !failed_count + 1
        | Local_session.CycleDetected { cycle_nodes; _ } ->
            println "      \027[1;31mError\027[0m: Cyclic dependency detected:";
            println ("         " ^ String.concat " ->\n         " cycle_nodes))
  in
  
  let final_event = match result with
  | Error err ->
      Local_session.close client;
      (match err with
      | Local_session.PackageNotFound { package_name; available_packages } ->
          println ("\027[1;31mError\027[0m: Package '" ^ package_name ^ "' not found");
          println "";
          println "Available packages:";
          List.iter (fun pkg -> println ("  • " ^ pkg)) available_packages;
          exit 1
      | Local_session.BuildAlreadyRunning { lock_path } ->
          println ("\027[1;31mError\027[0m: another tusk build is already running");
          println ("Lock file: " ^ Path.to_string lock_path);
          println "Wait for the current build to finish and try again.";
          exit 1
      | Local_session.UnexpectedEvent Module.{ reason } ->
          println ("\027[1;31mError\027[0m: " ^ reason);
          exit 1)
  | Ok event ->
      Local_session.close client;
      event
  in

  (* Print final summary line *)
  let duration =
    Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ())
  in
  let duration_secs = Time.Duration.to_secs_float duration in

  let formatted_duration = Time.Duration.to_secs_string ~precision:2 duration in
  let total_count = !built_count + !cached_count in
  
  if !failed_count = 0 && !skipped_count = 0 then
    println ("    \027[1;32mFinished\027[0m in " ^ formatted_duration ^ "s (" ^ 
      Int.to_string total_count ^ " built)")
  else if !failed_count > 0 then
    println
      ("    \027[1;31mFinished\027[0m in " ^ formatted_duration ^ "s ("
      ^ Int.to_string total_count ^ " built, "
      ^ Int.to_string !failed_count ^ " failed, "
      ^ Int.to_string !skipped_count ^ " skipped)")
  else
    println
      ("    \027[1;33mFinished\027[0m in " ^ formatted_duration ^ "s ("
      ^ Int.to_string total_count ^ " built, "
      ^ Int.to_string !skipped_count ^ " skipped)");

  match final_event with
  | Local_session.BuildCompleted _ -> Ok ()
  | Local_session.BuildFailed _ -> Error (Failure "Build failed")
  | Local_session.PlanningFailed _ -> Error (Failure "Planning failed")
  | Local_session.CycleDetected _ -> Error (Failure "Cyclic dependency detected")
  | Local_session.BuildStarted _ | Local_session.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  
  (* Get workspace to resolve targets *)
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd
    |> Result.expect ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in
  
  (* Resolve target(s) from flags *)
  let targets = match resolve_targets workspace matches with
    | Ok targets -> targets
    | Error msg ->
        println ("❌ " ^ msg);
        exit 1
  in
  
  (* For now, only support single target - multi-target requires executor changes *)
  if List.length targets > 1 then (
    println "";
    println "❌ Multiple targets matched. Please specify a single target.";
    println "";
    println "Matched targets:";
    List.iter (fun t -> println ("  • " ^ t)) targets;
    println "";
    println "Use one of these flags to build for a specific target:";
    List.iter (fun t -> println ("  tusk build -x " ^ t)) targets;
    println "";
    Error (Failure "Multiple targets matched")
  ) else if List.length targets = 1 then (
    let target = List.hd targets in
    let host = Tusk_toolchain.get_host_triple () in
    
    (* Ensure toolchain is installed (auto-install if missing) *)
    ensure_toolchains_for_targets workspace [target];
    
    (* Validate toolchain for target exists *)
    let config = Toolchain_config.from_workspace workspace in
    (match Tusk_toolchain.init_for_target ~config ~target with
    | Ok _ -> ()
    | Error msg ->
        println ("❌ Failed to initialize toolchain for " ^ target);
        println msg;
        exit 1);
    
    (* Determine if we're cross-compiling *)
    let target_arch = if target = host then None else Some target in
    
    (match target_arch with
    | Some arch -> println ("🔨 Cross-compiling for " ^ arch)
    | None -> ());
    
    build_command package_opt target_arch
  ) else (
    println "❌ No targets specified";
    Error (Failure "No targets")
  )
