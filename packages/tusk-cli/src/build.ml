open Std
open Std.Collections
open Tusk_model
open Tusk_server

type build_scope =
  Runtime
  | Dev

let out = eprintln

type output_mode =
  | Human
  | Json

let write_json_event = fun (json: Data.Json.t) ->
  print (Data.Json.to_string json);
  print "\n"

let telemetry_event_to_json = fun event ->
  match Tusk_executor.Telemetry_events.to_json event with
  | Some json -> json
  | None -> Data.Json.Null

let build_stats_to_json = fun (stats: Local_session.build_stats) ->
  Data.Json.Object [
    ("duration_ms", Data.Json.Int stats.duration_ms);
    ("packages_built", Data.Json.Int stats.packages_built);
    ("packages_failed", Data.Json.Int stats.packages_failed);
    ("total_modules", Data.Json.Int stats.total_modules);
    ("cache_hits", Data.Json.Int stats.cache_hits);
    ("cache_misses", Data.Json.Int stats.cache_misses);
  ]

let package_results_to_json = fun (results: Tusk_executor.Package_builder.build_result list) ->
  Data.Json.Array (List.map Tusk_executor.Package_builder.build_result_to_json results)

let build_failed_event_to_json = fun session_id failed_at errors ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildFailed");
    ("session_id", Data.Json.String (Session_id.to_string session_id));
    ("failed_at", Data.Json.String (Datetime.to_iso8601 failed_at));
    ("errors", package_results_to_json errors);
  ]

let build_completed_event_to_json = fun session_id completed_at stats results ->
  Data.Json.Object [
    ("type", Data.Json.String "BuildCompleted");
    ("session_id", Data.Json.String (Session_id.to_string session_id));
    ("completed_at", Data.Json.String (Datetime.to_iso8601 completed_at));
    ("stats", build_stats_to_json stats);
    ("results", package_results_to_json results);
  ]

let planning_failed_event_to_json = fun session_id failed_at reason ->
  Data.Json.Object [
    ("type", Data.Json.String "PlanningFailed");
    ("session_id", Data.Json.String (Session_id.to_string session_id));
    ("failed_at", Data.Json.String (Datetime.to_iso8601 failed_at));
    ("reason", Data.Json.String reason);
  ]

let cycle_detected_event_to_json = fun session_id detected_at cycle_nodes ->
  Data.Json.Object [
    ("type", Data.Json.String "CycleDetected");
    ("session_id", Data.Json.String (Session_id.to_string session_id));
    ("detected_at", Data.Json.String (Datetime.to_iso8601 detected_at));
    ("cycle_nodes", Data.Json.Array (List.map Data.Json.string cycle_nodes));
  ]

let command_error_event_to_json = fun kind details ->
  Data.Json.Object (("type", Data.Json.String kind) :: details)

let pm_package_source_label = function
  | Some version -> version
  | None -> "src"

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Tusk_model.Event.RegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates registry in
          Some ("    \027[1;32mUpdating\027[0m " ^ registry ^ " index")
        )
  | Tusk_model.Event.PackageResolvedForBuild { package; version; _ } ->
      Some ("    \027[1;32mResolved\027[0m "
      ^ package
      ^ " ("
      ^ pm_package_source_label version
      ^ ")")
  | Tusk_model.Event.PackageDownloadStarted { package; version; _ } ->
      Some (" \027[1;32mDownloading\027[0m "
      ^ package
      ^ " ("
      ^ version
      ^ ")")
  | Tusk_model.Event.PackageDownloadQueued { package; version; _ } ->
      Some ("      \027[1;33mQueued\027[0m "
      ^ package
      ^ " ("
      ^ version
      ^ ")")
  | Tusk_model.Event.PackageDownloadSkipped { package; version; _ } ->
      Some ("    \027[1;33mUsing\027[0m "
      ^ package
      ^ " ("
      ^ version
      ^ ")")
  | Tusk_model.Event.DependencyResolutionStarted _
  | Tusk_model.Event.DependencyResolutionRefreshingLock _
  | Tusk_model.Event.DependencyResolutionFailed _
  | Tusk_model.Event.DependencyUniverseBuilding _
  | Tusk_model.Event.DependencyUniverseBuilt _
  | Tusk_model.Event.PackageMetadataFetchStarted _
  | Tusk_model.Event.PackageMetadataFetchFinished _
  | Tusk_model.Event.PackageMetadataFetchFailed _
  | Tusk_model.Event.LockfileReadStarted _
  | Tusk_model.Event.LockfileReadFinished _
  | Tusk_model.Event.LockfileReadFailed _
  | Tusk_model.Event.LockfileWriteStarted _
  | Tusk_model.Event.LockfileWriteFinished _
  | Tusk_model.Event.LockfileWriteFailed _
  | Tusk_model.Event.DependencyResolutionFinished _
  | Tusk_model.Event.DependencyResolutionUsingExistingLock _
  | Tusk_model.Event.DependencyResolutionUnlocking _ ->
      None
  | kind -> Some (Tusk_model.Event.display kind)

let write_pm_event = fun ~mode ~session_id ~seen_registry_updates kind ->
  match mode with
  | Json -> Tusk_model.Event.create ~session_id ~level:Tusk_model.Event.Info kind
  |> Tusk_model.Event.to_json
  |> write_json_event
  | Human -> (
      match format_pm_event ~seen_registry_updates kind with
      | Some message -> out message
      | None -> ()
    )

let write_command_error = fun ~mode kind details human_message ->
  match mode with
  | Json -> write_json_event (command_error_event_to_json kind details)
  | Human -> out ("\027[1;31mError\027[0m: " ^ human_message)

(** Helper functions for target resolution *)
let ensure_toolchains_for_targets = fun workspace targets ->
  let config = Toolchain_config.from_workspace workspace in
  (* Check which toolchains are missing *)
  let missing =
    List.filter
      (fun target ->
        match Tusk_toolchain.check_toolchain_status ~version:config.version ~target with
        | Tusk_toolchain.NotInstalled _
        | Tusk_toolchain.Incomplete _ -> true
        | Tusk_toolchain.Installed _ -> false)
      targets
  in
  if List.length missing > 0 then
    (
      out "";
      out ("📥 Installing " ^ Int.to_string (List.length missing) ^ " missing toolchain(s)...");
      out "";
      let host = Tusk_toolchain.get_host_triple () in
      List.iter
        (fun target ->
          match Tusk_toolchain.download_and_install_toolchain config.version ~host ~target with
          | Ok () -> out ("  ✓ " ^ target)
          | Error msg ->
              out ("  ✗ " ^ target ^ ": " ^ msg);
              out "";
              out ("❌ Failed to install toolchain for " ^ target);
              exit 1)
        missing;
      out ""
    )

let get_configured_targets = fun workspace ->
  let config = Toolchain_config.from_workspace workspace in
  match config.targets with
  | [] -> [ Tusk_toolchain.get_host_triple () ]
  | targets -> targets

let resolve_target_pattern = fun workspace pattern ->
  let configured = get_configured_targets workspace in
  let host = Tusk_toolchain.get_host_triple () in
  match String.lowercase_ascii pattern with
  | "host"
  | "native" ->
      Ok [ host ]
  | "all" ->
      Ok configured
  | exact when List.mem exact configured ->
      Ok [ exact ]
  | pattern ->
      (* Substring matching *)
      let matches =
        List.filter
          (fun t ->
            String.contains t pattern)
          configured
      in
      if List.length matches = 0 then
        Error (
          "No targets match pattern '" ^ pattern ^ "'.\n\
                Available targets: " ^ String.concat ", " configured
        )
      else
        Ok matches

let resolve_targets = fun workspace matches ->
  let all_targets = ArgParser.get_flag matches "all-targets" in
  let target_pattern = ArgParser.get_one matches "target" in
  if all_targets then
    Ok (get_configured_targets workspace)
  else
    match target_pattern with
    | Some pattern -> resolve_target_pattern workspace pattern
    | None ->
        (* Default to host *)
        Ok [ Tusk_toolchain.get_host_triple () ]

let command =
  let open ArgParser in
    let open Arg in command "build"
    |> about "Build packages"
    |> args
      [
        positional "package" |> required false |> multiple |> help "Packages to build (or omit to build all packages)";
        option "target" |> short 'x' |> long "target" |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
        flag "all-targets" |> help "Build for all configured targets";
        flag "json" |> long "json" |> help "Emit machine-readable JSONL events";
      ]

let run_build_request = fun ~workspace ~load_errors ?(scope = Runtime) ?(mode = Human) ?(show_finished_summary =
  true) request target_arch ->
  let startup_session_id = Session_id.make () in
  let seen_registry_updates = HashSet.create () in
  match Local_session.connect_local
    ~emit:(write_pm_event ~mode ~session_id:startup_session_id ~seen_registry_updates)
    ~workspace
    ~load_errors
    () with
  | Error (Local_session.StartupFailed { error }) ->
      let reason = Tusk_server.error_message error in
      write_command_error
        ~mode
        "SessionStartFailed"
        [ ("reason", Data.Json.String reason) ]
        reason;
      exit 1
  | Error err ->
      let reason = Local_session.error_message err in
      write_command_error
        ~mode
        "SessionStartFailed"
        [ ("reason", Data.Json.String reason) ]
        reason;
      exit 1
  | Ok client ->
      let displayed_packages = HashSet.create () in
      (* Track build stats as events arrive *)
      let start_time = Time.Instant.now () in
      let built_count = ref 0 in
      let cached_count = ref 0 in
      let failed_count = ref 0 in
      let skipped_count = ref 0 in
      let result =
        Local_session.build_streaming client request
          ~scope:((
            match scope with
            | Runtime -> Local_session.Runtime
            | Dev -> Local_session.Dev
          ))
          ?target_arch
          (fun event ->
            match mode with
            | Json -> (
                match event with
                | Local_session.BuildStarted _ ->
                    ()
                | Local_session.BuildEvent event -> (
                    match telemetry_event_to_json event with
                    | Data.Json.Null -> ()
                    | json -> write_json_event json
                  )
                | Local_session.BuildCompleted { session_id; completed_at; stats; results } ->
                    build_completed_event_to_json session_id completed_at stats results |> write_json_event
                | Local_session.BuildFailed {
                  session_id;
                  failed_at;
                  stats=_;
                  built=_;
                  errors
                } ->
                    build_failed_event_to_json session_id failed_at errors |> write_json_event
                | Local_session.PlanningFailed { session_id; failed_at; reason } ->
                    planning_failed_event_to_json session_id failed_at reason |> write_json_event
                | Local_session.CycleDetected { session_id; detected_at; cycle_nodes } ->
                    cycle_detected_event_to_json session_id detected_at cycle_nodes |> write_json_event
              )
            | Human -> (
                match event with
                | Local_session.BuildStarted _ ->
                    ()
                | Local_session.BuildEvent event ->
                    (
                      match event with
                      | Tusk_executor.Telemetry_events.BuildCompleted { status=`Fresh; _ } -> built_count := !built_count + 1
                      | Tusk_executor.Telemetry_events.BuildCompleted { status=`Cached; _ } -> cached_count := !cached_count + 1
                      | Tusk_executor.Telemetry_events.BuildFailed _ -> failed_count := !failed_count + 1
                      | Tusk_executor.Telemetry_events.BuildSkipped _ -> skipped_count := !skipped_count + 1
                      | _ -> ()
                    );
                    let msg = Event_formatter.format ~displayed_packages event in
                    if msg != "" then
                      out msg
                | Local_session.BuildCompleted _ ->
                    ()
                | Local_session.BuildFailed _ ->
                    ()
                | Local_session.PlanningFailed { reason; _ } ->
                    out "";
                    out ("\027[1;31mPlanning Failed\027[0m: " ^ reason);
                    failed_count := !failed_count + 1
                | Local_session.CycleDetected { cycle_nodes; _ } ->
                    out "      \027[1;31mError\027[0m: Cyclic dependency detected:";
                    out ("         " ^ String.concat " ->\n         " cycle_nodes)
              ))
      in
      let final_event =
        match result with
        | Error err ->
            Local_session.close client;
            (
              match err with
              | Local_session.PackageNotFound { package_name; available_packages } ->
                  if mode = Json then
                    write_json_event
                      (command_error_event_to_json
                        "PackageNotFound"
                        [
                          ("package_name", Data.Json.String package_name);
                          ("available_packages", Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) available_packages));
                        ]);
                  out ("\027[1;31mError\027[0m: Package '" ^ package_name ^ "' not found");
                  out "";
                  out "Available packages:";
                  List.iter (fun pkg -> out ("  • " ^ pkg)) available_packages;
                  exit 1
              | Local_session.PackagesNotFound { package_names; available_packages } ->
                  if mode = Json then
                    write_json_event
                      (command_error_event_to_json
                        "PackagesNotFound"
                        [
                          ("package_names", Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) package_names));
                          ("available_packages", Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) available_packages));
                        ]);
                  out ("\027[1;31mError\027[0m: Packages not found: " ^ String.concat ", " package_names);
                  out "";
                  out "Available packages:";
                  List.iter (fun pkg -> out ("  • " ^ pkg)) available_packages;
                  exit 1
              | Local_session.BuildAlreadyRunning { lock_path } ->
                  if mode = Json then
                    write_json_event
                      (command_error_event_to_json
                        "BuildAlreadyRunning"
                        [ ("lock_path", Data.Json.String (Path.to_string lock_path)) ]);
                  out "\027[1;31mError\027[0m: another tusk build is already running";
                  out ("Lock file: " ^ Path.to_string lock_path);
                  out "Wait for the current build to finish and try again.";
                  exit 1
              | Local_session.UnexpectedEvent Module.{ reason } ->
                  if mode = Json then
                    write_json_event
                      (command_error_event_to_json
                        "UnexpectedEvent"
                        [ ("reason", Data.Json.String reason) ]);
                  out ("\027[1;31mError\027[0m: " ^ reason);
                  exit 1
              | Local_session.StartupFailed { error } ->
                  let reason = Tusk_server.error_message error in
                  if mode = Json then
                    write_json_event
                      (command_error_event_to_json
                        "SessionStartFailed"
                        [ ("reason", Data.Json.String reason) ]);
                  out ("\027[1;31mError\027[0m: " ^ reason);
                  exit 1
            )
        | Ok event ->
            Local_session.close client;
            event
      in
      if show_finished_summary then
        (
          match mode with
          | Json -> ()
          | Human ->
              let duration = Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ()) in
              let formatted_duration = Time.Duration.to_secs_string ~precision:2 duration in
              let total_count = !built_count + !cached_count in
              if !failed_count = 0 && !skipped_count = 0 then
                out
                  ("    \027[1;32mFinished\027[0m in "
                  ^ formatted_duration
                  ^ "s ("
                  ^ Int.to_string total_count
                  ^ " built)")
              else if !failed_count > 0 then
                out
                  ("    \027[1;31mFinished\027[0m in "
                  ^ formatted_duration
                  ^ "s ("
                  ^ Int.to_string total_count
                  ^ " built, "
                  ^ Int.to_string !failed_count
                  ^ " failed, "
                  ^ Int.to_string !skipped_count
                  ^ " skipped)")
              else
                out
                  ("    \027[1;33mFinished\027[0m in "
                  ^ formatted_duration
                  ^ "s ("
                  ^ Int.to_string total_count
                  ^ " built, "
                  ^ Int.to_string !skipped_count
                  ^ " skipped)")
        );
      match final_event with
      | Local_session.BuildCompleted _ -> Ok ()
      | Local_session.BuildFailed _ -> Error (Failure "Build failed")
      | Local_session.PlanningFailed _ -> Error (Failure "Planning failed")
      | Local_session.CycleDetected _ -> Error (Failure "Cyclic dependency detected")
      | Local_session.BuildStarted _
      | Local_session.BuildEvent _ -> Error (Failure "Unexpected response from server")

let build_command = fun ?workspace ?load_errors ?(scope = Runtime) ?(mode = Human) ?(show_finished_summary = true) package_opt target_arch ->
  let (workspace, load_errors) =
    match (workspace, load_errors) with
    | Some workspace, Some load_errors -> (workspace, load_errors)
    | _ ->
        let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
        Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in
  let request =
    match package_opt with
    | Some pkg -> Local_session.BuildPackage pkg
    | None -> Local_session.BuildAll
  in
  run_build_request ~workspace ~load_errors ~scope ~mode ~show_finished_summary request target_arch

let build_packages_command = fun ~workspace ~load_errors ?(scope = Runtime) ?(mode = Human) ?(show_finished_summary =
  true) package_names target_arch ->
  let request =
    match package_names with
    | [] -> Local_session.BuildAll
    | [ package_name ] -> Local_session.BuildPackage package_name
    | packages -> Local_session.BuildPackages packages
  in
  run_build_request ~workspace ~load_errors ~scope ~mode ~show_finished_summary request target_arch

let run = fun ~workspace ~load_errors matches ->
  let open ArgParser in
    let package_names = get_many matches "package" in
    let mode =
      if get_flag matches "json" then
        Json
      else
        Human
    in
    (* Resolve target(s) from flags *)
    let targets =
      match resolve_targets workspace matches with
      | Ok targets -> targets
      | Error msg ->
          out ("❌ " ^ msg);
          exit 1
    in
    (* For now, only support single target - multi-target requires executor changes *)
    if List.length targets > 1 then
      (
        out "";
        out "❌ Multiple targets matched. Please specify a single target.";
        out "";
        out "Matched targets:";
        List.iter (fun t -> out ("  • " ^ t)) targets;
        out "";
        out "Use one of these flags to build for a specific target:";
        List.iter (fun t -> out ("  tusk build -x " ^ t)) targets;
        out "";
        Error (Failure "Multiple targets matched")
      )
    else if List.length targets = 1 then
      (
        let target = List.hd targets in
        let host = Tusk_toolchain.get_host_triple () in
        (* Ensure toolchain is installed (auto-install if missing) *)
        ensure_toolchains_for_targets workspace [ target ];
        (* Validate toolchain for target exists *)
        let config = Toolchain_config.from_workspace workspace in
        (
          match Tusk_toolchain.init_for_target ~config ~target with
          | Ok _ -> ()
          | Error msg ->
              out ("❌ Failed to initialize toolchain for " ^ target);
              out msg;
              exit 1
        );
        (* Determine if we're cross-compiling *)
        let target_arch =
          if target = host then
            None
          else
            Some target
        in
        (
          match target_arch with
          | Some arch -> out ("🔨 Cross-compiling for " ^ arch)
          | None -> ()
        );
        build_packages_command ~workspace ~load_errors ~mode package_names target_arch
      )
    else (
      out "❌ No targets specified";
      Error (Failure "No targets")
    )
