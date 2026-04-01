open Std
open Std.Collections
open Tusk_model
open Tusk_build

type build_scope = Tusk_build.build_scope =
  Runtime
  | Dev

let out = eprintln

type output_mode =
  | Human
  | Json

type build_progress = {
  mutable built_count: int;
  mutable cached_count: int;
  mutable failed_count: int;
  mutable skipped_count: int;
}

type request = {
  build_request: Tusk_build.build_request;
  output_mode: output_mode;
  show_finished_summary: bool;
}

let build_trace_enabled = fun () ->
  match Env.var String ~name:"TUSK_BUILD_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_build = fun message ->
  if build_trace_enabled () then
    eprintln ("[tusk-build] " ^ message)
  else
    ()

let build_request_label = fun (request: Tusk_build.build_request) ->
  let packages =
    match request.packages with
    | [] -> "all"
    | packages -> String.concat "," packages
  in
  let targets =
    match request.targets with
    | Tusk_build.Host -> "host"
    | Tusk_build.All -> "all"
    | Tusk_build.Pattern pattern -> pattern
  in
  "Build(" ^ packages ^ "; targets=" ^ targets ^ ")"

let streaming_event_label = function
  | Client.BuildStarted _ -> "BuildStarted"
  | Client.BuildEvent _ -> "BuildEvent"
  | Client.BuildCompleted _ -> "BuildCompleted"
  | Client.BuildFailed _ -> "BuildFailed"
  | Client.PlanningFailed _ -> "PlanningFailed"
  | Client.CycleDetected _ -> "CycleDetected"

let write_json_event = fun (json: Data.Json.t) ->
  print (Data.Json.to_string json);
  print "\n"

let write_build_event_json = fun event ->
  match Tusk_build.Event.to_json event with
  | Some json -> write_json_event json
  | None -> ()

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
  | Tusk_model.Event.PackageResolvedForBuild { package; version; workspace; _ } ->
      if workspace then
        None
      else
        Some ("    \027[1;32mResolved\027[0m " ^ package ^ " (" ^ pm_package_source_label version ^ ")")
  | Tusk_model.Event.PackageDownloadStarted { package; version; _ } ->
      Some ("    \027[1;32mFetching\027[0m " ^ package ^ " " ^ version)
  | Tusk_model.Event.PackageDownloadQueued { package; version; _ } ->
      Some ("      \027[1;33mQueued\027[0m " ^ package ^ " (" ^ version ^ ")")
  | Tusk_model.Event.PackageMaterializationStarted { package; version; _ } ->
      Some ("    \027[1;32mFetching\027[0m " ^ package ^ " " ^ version)
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
  | Tusk_model.Event.DependencyResolutionUnlocking _
  | Tusk_model.Event.PackageManifestFetchStarted _
  | Tusk_model.Event.PackageManifestFetchFinished _
  | Tusk_model.Event.PackageManifestFetchFailed _
  | Tusk_model.Event.PackageDownloadSkipped _
  | Tusk_model.Event.PackageMaterializationFinished _
  | Tusk_model.Event.PackageMaterializationFailed _ ->
      None
  | kind ->
      Some (Tusk_model.Event.display kind)

let write_pm_event = fun ~mode ~seen_registry_updates event ->
  match mode with
  | Json -> write_build_event_json (Tusk_build.Pm event)
  | Human -> (
      match format_pm_event ~seen_registry_updates event.kind with
      | Some message -> out message
      | None -> ()
    )

let write_command_error = fun ~mode kind details human_message ->
  match mode with
  | Json -> write_json_event (command_error_event_to_json kind details)
  | Human -> out ("\027[1;31mError\027[0m: " ^ human_message)

let command =
  let open ArgParser in
    let open Arg in command "build"
    |> about "Build packages"
    |> args
      [
        positional "package" |> required false |> multiple |> help "Packages to build (or omit to build all packages)";
        option "target" |> short 'x' |> long "target" |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
        flag "all-targets" |> help "Build for all configured targets";
        flag "release" |> long "release" |> help "Use the release build profile";
        flag "json" |> long "json" |> help "Emit machine-readable JSONL events";
      ]

let target_request_of_matches = fun matches ->
  if ArgParser.get_flag matches "all-targets" then
    Tusk_build.All
  else
    match ArgParser.get_one matches "target" with
    | Some pattern -> Tusk_build.Pattern pattern
    | None -> Tusk_build.Host

let output_mode_of_matches = fun matches ->
  if ArgParser.get_flag matches "json" then
    Json
  else
    Human

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let make_request = fun ~workspace ?(scope = Runtime) ?(profile = "debug") ?(mode = Human) ?(show_finished_summary = true) ~packages ~targets () ->
  {
    build_request =
      Tusk_build.{
        workspace;
        packages;
        targets;
        scope;
        profile;
      };
    output_mode = mode;
    show_finished_summary;
  }

let request_of_matches = fun ~workspace matches ->
  make_request
    ~workspace
    ~profile:(profile_of_matches matches)
    ~mode:(output_mode_of_matches matches)
    ~packages:(ArgParser.get_many matches "package")
    ~targets:(target_request_of_matches matches)
    ()

let write_building_target_event = fun ~mode ~target ~host ->
  match mode with
  | Json -> write_build_event_json (Tusk_build.BuildingTarget { target; host })
  | Human ->
      if not host then
        out ("🔨 Cross-compiling for " ^ target)

let write_streaming_event = fun ~mode ~displayed_packages ~progress event ->
  trace_build ("streaming event: " ^ streaming_event_label event);
  match mode with
  | Json -> ()
  | Human -> (
      match event with
      | Client.BuildStarted _ ->
          ()
      | Client.BuildEvent event ->
          (
            match event with
            | Tusk_executor.Telemetry_events.BuildCompleted { status=`Fresh; _ } -> progress.built_count <- progress.built_count
            + 1
            | Tusk_executor.Telemetry_events.BuildCompleted { status=`Cached; _ } -> progress.cached_count <- progress.cached_count
            + 1
            | Tusk_executor.Telemetry_events.BuildFailed _ -> progress.failed_count <- progress.failed_count
            + 1
            | Tusk_executor.Telemetry_events.BuildSkipped _ -> progress.skipped_count <- progress.skipped_count
            + 1
            | _ -> ()
          );
          let msg = Event_formatter.format ~displayed_packages event in
          if msg != "" then
            out msg
      | Client.BuildCompleted _ ->
          ()
      | Client.BuildFailed _ ->
          ()
      | Client.PlanningFailed { reason; _ } ->
          out "";
          out ("\027[1;31mPlanning Failed\027[0m: " ^ reason);
          progress.failed_count <- progress.failed_count + 1
      | Client.CycleDetected { cycle_nodes; _ } ->
          out "      \027[1;31mError\027[0m: Cyclic dependency detected:";
          out ("         " ^ String.concat " ->\n         " cycle_nodes)
    )

let write_build_error = fun ~mode err ->
  match err with
  | Tusk_build.NoTargetsMatched { pattern; available_targets } ->
      write_command_error
        ~mode
        "NoTargetsMatched"
        [
          ("pattern", Data.Json.String pattern);
          (
            "available_targets",
            Data.Json.Array (List.map (fun target -> Data.Json.String target) available_targets)
          );
        ]
        (Tusk_build.build_error_message err)
  | Tusk_build.ToolchainInstallFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInstallFailed"
        [ ("target", Data.Json.String target); ("reason", Data.Json.String error) ]
        (Tusk_build.build_error_message err)
  | Tusk_build.ToolchainInitializationFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInitializationFailed"
        [ ("target", Data.Json.String target); ("reason", Data.Json.String error) ]
        (Tusk_build.build_error_message err)
  | Tusk_build.ClientError client_error -> (
      match client_error with
      | Client.PackageNotFound { package_name; available_packages } ->
          if mode = Json then
            write_json_event
              (command_error_event_to_json
                "PackageNotFound"
                [
                  ("package_name", Data.Json.String package_name);
                  (
                    "available_packages",
                    Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) available_packages)
                  );
                ])
          else (
            out ("\027[1;31mError\027[0m: Package '" ^ package_name ^ "' not found");
            out "";
            out "Available packages:";
            List.iter (fun pkg -> out ("  • " ^ pkg)) available_packages
          )
      | Client.PackagesNotFound { package_names; available_packages } ->
          if mode = Json then
            write_json_event
              (command_error_event_to_json
                "PackagesNotFound"
                [
                  (
                    "package_names",
                    Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) package_names)
                  );
                  (
                    "available_packages",
                    Data.Json.Array (List.map (fun pkg -> Data.Json.String pkg) available_packages)
                  );
                ])
          else (
            out ("\027[1;31mError\027[0m: Packages not found: " ^ String.concat ", " package_names);
            out "";
            out "Available packages:";
            List.iter (fun pkg -> out ("  • " ^ pkg)) available_packages
          )
      | Client.BuildAlreadyRunning { lock_path } ->
          write_command_error
            ~mode
            "BuildAlreadyRunning"
            [ ("lock_path", Data.Json.String (Path.to_string lock_path)) ]
            ("another tusk build is already running\nLock file: " ^ Path.to_string lock_path ^ "\nWait for the current build to finish and try again.")
      | Client.BuildFailed { errors } ->
          write_command_error
            ~mode
            "BuildFailed"
            [
              (
                "errors",
                Data.Json.Array (List.map Tusk_executor.Package_builder.build_result_to_json errors)
              );
            ]
            (Client.error_message client_error)
      | Client.PlanningFailed { reason } ->
          write_command_error
            ~mode
            "PlanningFailed"
            [ ("reason", Data.Json.String reason) ]
            (Client.error_message client_error)
      | Client.CycleDetected { cycle_nodes } ->
          write_command_error
            ~mode
            "CycleDetected"
            [ ("cycle_nodes", Data.Json.Array (List.map Data.Json.string cycle_nodes)) ]
            (Client.error_message client_error)
      | Client.UnexpectedEvent { reason } ->
          write_command_error ~mode "UnexpectedEvent" [ ("reason", Data.Json.String reason) ] reason
      | Client.StartupFailed { error } ->
          let reason = Tusk_build.error_message error in
          write_command_error ~mode "SessionStartFailed" [ ("reason", Data.Json.String reason) ] reason
    )

let build_error_already_reported = fun (err: Tusk_build.build_error) ->
  match err with
  | Tusk_build.ClientError (Client.BuildFailed _)
  | Tusk_build.ClientError (Client.PlanningFailed _)
  | Tusk_build.ClientError (Client.CycleDetected _) -> true
  | _ -> false

let run_request = fun (request: request) ->
  trace_build
    (
      "run_request request="
      ^ build_request_label request.build_request
      ^ " scope="
      ^ match request.build_request.scope with
      | Runtime -> "runtime"
      | Dev ->
          "dev" ^ " mode=" ^ match request.output_mode with
          | Human -> "human"
          | Json -> "json"
    );
  let seen_registry_updates = HashSet.create () in
  let displayed_packages = HashSet.create () in
  let start_time = Time.Instant.now () in
  let progress = { built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let attempted_build = ref false in
  let result =
    Tusk_build.build
      ~on_event:(
        function
        | Tusk_build.Pm kind ->
            write_pm_event ~mode:request.output_mode ~seen_registry_updates kind
        | Tusk_build.BuildingTarget { target; host } ->
            attempted_build := true;
            write_building_target_event ~mode:request.output_mode ~target ~host
        | Tusk_build.Streaming event ->
            attempted_build := true;
            write_streaming_event ~mode:request.output_mode ~displayed_packages ~progress event;
            if request.output_mode = Json then
              write_build_event_json (Tusk_build.Streaming event)
      )
      request.build_request
  in
  if request.show_finished_summary && !attempted_build then
    (
      match request.output_mode with
      | Json -> ()
      | Human ->
          let duration = Time.Instant.duration_since ~earlier:start_time (Time.Instant.now ()) in
          let formatted_duration = Time.Duration.to_secs_string ~precision:2 duration in
          let total_count = progress.built_count + progress.cached_count in
          if progress.failed_count = 0 && progress.skipped_count = 0 then
            out
              ("    \027[1;32mFinished\027[0m in "
              ^ formatted_duration
              ^ "s ("
              ^ Int.to_string total_count
              ^ " built)")
          else if progress.failed_count > 0 then
            out
              ("    \027[1;31mFinished\027[0m in "
              ^ formatted_duration
              ^ "s ("
              ^ Int.to_string total_count
              ^ " built, "
              ^ Int.to_string progress.failed_count
              ^ " failed, "
              ^ Int.to_string progress.skipped_count
              ^ " skipped)")
          else
            out
              ("    \027[1;33mFinished\027[0m in "
              ^ formatted_duration
              ^ "s ("
              ^ Int.to_string total_count
              ^ " built, "
              ^ Int.to_string progress.skipped_count
              ^ " skipped)")
    );
  match result with
  | Ok () -> Ok ()
  | Error err ->
      if not (build_error_already_reported err) then
        write_build_error ~mode:request.output_mode err;
      Error (Failure (Tusk_build.build_error_message err))

let print_workspace_load_errors = fun errors ->
  List.iter
    (fun err -> out ("\027[1;31mError\027[0m: " ^ Workspace_manager.load_error_to_string err))
    errors

let load_workspace_strict = fun cwd ->
  match Workspace_manager.scan cwd with
  | Error err ->
      Error (Failure err)
  | Ok (_workspace, load_errors) when List.length load_errors > 0 ->
      print_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Ok (workspace, _) ->
      Ok workspace

let build_command = fun ?workspace ?(scope = Runtime) ?(profile = "debug") ?(mode = Human) ?(show_finished_summary = true) package_opt target_arch ->
  let workspace =
    match workspace with
    | Some workspace -> Ok workspace
    | None ->
        let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
        load_workspace_strict cwd
  in
  match workspace with
  | Error _ as err -> err
  | Ok workspace ->
      run_request
        (
          make_request ~workspace ~scope ~profile ~mode ~show_finished_summary ~packages:((package_opt
          |> Option.to_list))
            ~targets:((
              match target_arch with
              | Some target -> Tusk_build.Pattern target
              | None -> Tusk_build.Host
            ))
            ()
        )

let build_packages_command = fun ~workspace ?(scope = Runtime) ?(mode = Human) ?(show_finished_summary = true) package_names target_arch ->
  run_request
    (
      make_request ~workspace ~scope ~mode ~show_finished_summary ~packages:package_names
        ~targets:((
          match target_arch with
          | Some target -> Tusk_build.Pattern target
          | None -> Tusk_build.Host
        ))
        ()
    )

let run = fun ~workspace matches -> run_request (request_of_matches ~workspace matches)
