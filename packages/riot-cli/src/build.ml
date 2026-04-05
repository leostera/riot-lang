open Std
open Std.Collections
open Riot_model
open Riot_build

type build_scope = Riot_build.build_scope =
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
  build_request: Riot_build.build_request;
  workspace_manager: Workspace_manager.t option;
  output_mode: output_mode;
  show_finished_summary: bool;
  prepared: bool;
}

let build_trace_enabled = fun () ->
  match Env.var String ~name:"RIOT_BUILD_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_build = fun message ->
  if build_trace_enabled () then
    eprintln ("[riot-build] " ^ message)
  else
    ()

let build_request_label = fun (request: Riot_build.build_request) ->
  let packages =
    match request.packages with
    | [] -> "all"
    | packages -> String.concat "," packages
  in
  let targets =
    match request.targets with
    | Riot_build.Host -> "host"
    | Riot_build.All -> "all"
    | Riot_build.Pattern pattern -> pattern
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
  match Riot_build.Event.to_json event with
  | Some json -> write_json_event json
  | None -> ()

let command_error_event_to_json = fun kind details ->
  Data.Json.Object (("type", Data.Json.String kind) :: details)

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Riot_model.Event.RegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates registry in
          Some ("    \027[1;32mUpdating\027[0m " ^ registry ^ " index")
        )
  | Riot_model.Event.PackageResolvedForBuild _ ->
      None
  | Riot_model.Event.PackageDownloadStarted { package; version; _ } ->
      Some ("    \027[1;32mFetching\027[0m " ^ package ^ " " ^ version)
  | Riot_model.Event.PackageDownloadQueued { package; version; _ } ->
      Some ("      \027[1;33mQueued\027[0m " ^ package ^ " (" ^ version ^ ")")
  | Riot_model.Event.DependencyResolutionStarted _
  | Riot_model.Event.DependencyResolutionRefreshingLock _
  | Riot_model.Event.DependencyResolutionFailed _
  | Riot_model.Event.DependencyUniverseBuilding _
  | Riot_model.Event.DependencyUniverseBuilt _
  | Riot_model.Event.PackageMetadataFetchStarted _
  | Riot_model.Event.PackageMetadataFetchFinished _
  | Riot_model.Event.PackageMetadataFetchFailed _
  | Riot_model.Event.SourceDependencyMaterializationFinished _
  | Riot_model.Event.LockfileReadStarted _
  | Riot_model.Event.LockfileReadFinished _
  | Riot_model.Event.LockfileReadFailed _
  | Riot_model.Event.LockfileWriteStarted _
  | Riot_model.Event.LockfileWriteFinished _
  | Riot_model.Event.LockfileWriteFailed _
  | Riot_model.Event.DependencyResolutionFinished _
  | Riot_model.Event.DependencyResolutionUsingExistingLock _
  | Riot_model.Event.DependencyResolutionUnlocking _
  | Riot_model.Event.PackageManifestFetchStarted _
  | Riot_model.Event.PackageManifestFetchFinished _
  | Riot_model.Event.PackageManifestFetchFailed _
  | Riot_model.Event.PackageDownloadSkipped _
  | Riot_model.Event.PackageMaterializationStarted _
  | Riot_model.Event.PackageMaterializationFinished _
  | Riot_model.Event.PackageMaterializationFailed _ ->
      None
  | Riot_model.Event.SourceDependencyMaterializationStarted { source_locator; ref_ } ->
      Some (
        "    \027[1;34mCloning\027[0m " ^ (
          match ref_ with
          | Some ref_ -> source_locator ^ "#" ^ ref_
          | None -> source_locator
        )
      )
  | Riot_model.Event.DependencyManifestUpdated { path; section; operation; dependency } ->
      let verb =
        match operation with
        | `Add -> "Added"
        | `Remove -> "Removed"
      in
      Some ("    \027[1;32m" ^ verb ^ "\027[0m " ^ dependency ^ " (" ^ section ^ ") in " ^ path)
  | Riot_model.Event.PackageVersionLocked { package; version } ->
      Some ("    \027[1;32mLocked\027[0m " ^ package ^ " (" ^ version ^ ")")
  | Riot_model.Event.PackageVersionsUnchanged _ ->
      Some "    Dependencies are already up to date"
  | Riot_model.Event.PackageVersionUpdated { package; from_version; to_version } ->
      Some ("    \027[1;32mUpdated\027[0m " ^ package ^ " (" ^ from_version ^ " -> " ^ to_version ^ ")")
  | kind ->
      Some (Riot_model.Event.display kind)

let write_pm_event = fun ~mode ~seen_registry_updates event ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Pm event)
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
    Riot_build.All
  else
    match ArgParser.get_one matches "target" with
    | Some pattern -> Riot_build.Pattern pattern
    | None -> Riot_build.Host

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

let make_request = fun ~workspace ?workspace_manager ?(scope = Runtime) ?(profile = "debug") ?(mode = Human) ?(show_finished_summary = true) ?(prepared = false) ~packages ~targets () ->
  {
    build_request =
      Riot_build.{
        workspace;
        packages;
        targets;
        scope;
        profile;
      };
    workspace_manager;
    output_mode = mode;
    show_finished_summary;
    prepared;
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
  | Json -> write_build_event_json (Riot_build.BuildingTarget { target; host })
  | Human ->
      if not host then
        out ("🔨 Cross-compiling for " ^ target)

let scaled_size_string = fun bytes divisor suffix ->
  let whole = Int64.div bytes divisor in
  let remainder = Int64.rem bytes divisor in
  let fraction = Int64.div (Int64.mul remainder 10L) divisor in
  Int64.to_string whole ^ "." ^ Int64.to_string fraction ^ " " ^ suffix

let size_to_string = fun size_bytes ->
  let kib = 1_024L in
  let mib = Int64.mul kib 1_024L in
  let gib = Int64.mul mib 1_024L in
  let tib = Int64.mul gib 1_024L in
  if Int64.compare size_bytes tib >= 0 then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib >= 0 then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib >= 0 then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib >= 0 then
    scaled_size_string size_bytes kib "KiB"
  else
    Int64.to_string size_bytes ^ " B"

let format_cache_gc_cleanup = fun (summary: Riot_store.Cache_gc.summary) ->
  Int.to_string summary.deleted_entries
  ^ " cache entries and "
  ^ Int.to_string summary.deleted_generations
  ^ " generations ("
  ^ size_to_string summary.size_before_bytes
  ^ " -> "
  ^ size_to_string summary.size_after_bytes
  ^ ")"

let write_cache_gc_event = fun ~mode event ->
  match mode with
  | Json -> write_json_event (Riot_store.Cache_gc.event_to_json event)
  | Human -> (
      match event with
      | Riot_store.Cache_gc.GcStarted _ -> ()
      | Riot_store.Cache_gc.GcSkipped { trigger=Post_build; _ } -> ()
      | Riot_store.Cache_gc.GcSkipped { summary; _ } -> out
        ("    Cache is already within policy (" ^ size_to_string summary.size_after_bytes ^ ")")
      | Riot_store.Cache_gc.GcCompleted { summary; _ } -> out
        ("    \027[1;32mCleaning\027[0m " ^ format_cache_gc_cleanup summary)
      | Riot_store.Cache_gc.GcFailed { error; _ } -> out
        ("\027[1;31mError\027[0m: cache GC failed: " ^ error)
      | Riot_store.Cache_gc.ForceCleanStarted _ -> ()
      | Riot_store.Cache_gc.ForceCleanCompleted { build_root } -> out
        ("    \027[1;32mCleaning\027[0m removed build root " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.ForceCleanFailed { build_root; error } -> out
        ("\027[1;31mError\027[0m: failed to remove build root "
        ^ Path.to_string build_root
        ^ ": "
        ^ error)
    )

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
            | Riot_executor.Telemetry_events.BuildCompleted { status=`Fresh; _ } -> progress.built_count <- progress.built_count
            + 1
            | Riot_executor.Telemetry_events.BuildCompleted { status=`Cached; _ } -> progress.cached_count <- progress.cached_count
            + 1
            | Riot_executor.Telemetry_events.BuildFailed _ -> progress.failed_count <- progress.failed_count
            + 1
            | Riot_executor.Telemetry_events.BuildSkipped _ -> progress.skipped_count <- progress.skipped_count
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
  | Riot_build.NoTargetsMatched { pattern; available_targets } ->
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
        (Riot_build.build_error_message err)
  | Riot_build.ToolchainInstallFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInstallFailed"
        [ ("target", Data.Json.String target); ("reason", Data.Json.String error) ]
        (Riot_build.build_error_message err)
  | Riot_build.ToolchainInitializationFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInitializationFailed"
        [ ("target", Data.Json.String target); ("reason", Data.Json.String error) ]
        (Riot_build.build_error_message err)
  | Riot_build.ClientError client_error -> (
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
            ("another riot build is already running\nLock file: " ^ Path.to_string lock_path ^ "\nWait for the current build to finish and try again.")
      | Client.BuildFailed { errors } ->
          write_command_error
            ~mode
            "BuildFailed"
            [
              (
                "errors",
                Data.Json.Array (List.map Riot_executor.Package_builder.build_result_to_json errors)
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
          let reason = Riot_build.error_message error in
          write_command_error ~mode "SessionStartFailed" [ ("reason", Data.Json.String reason) ] reason
    )

let build_error_already_reported = fun (err: Riot_build.build_error) ->
  match err with
  | Riot_build.ClientError (Client.BuildFailed _)
  | Riot_build.ClientError (Client.PlanningFailed _)
  | Riot_build.ClientError (Client.CycleDetected _) -> true
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
    (
      if request.prepared then
        Riot_build.build_prepared
      else
        Riot_build.build
    ) ?workspace_manager:request.workspace_manager
      ~on_event:(
        function
        | Riot_build.Pm kind ->
            write_pm_event ~mode:request.output_mode ~seen_registry_updates kind
        | Riot_build.BuildingTarget { target; host } ->
            attempted_build := true;
            write_building_target_event ~mode:request.output_mode ~target ~host
        | Riot_build.CacheGc event ->
            attempted_build := true;
            write_cache_gc_event ~mode:request.output_mode event
        | Riot_build.Streaming event ->
            attempted_build := true;
            write_streaming_event ~mode:request.output_mode ~displayed_packages ~progress event;
            if request.output_mode = Json then
              write_build_event_json (Riot_build.Streaming event)
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
  | Ok _ -> Ok ()
  | Error err ->
      if not (build_error_already_reported err) then
        write_build_error ~mode:request.output_mode err;
      Error (Failure (Riot_build.build_error_message err))

let print_workspace_load_errors = fun errors ->
  List.iter
    (fun err -> out ("\027[1;31mError\027[0m: " ^ Workspace_manager.load_error_to_string err))
    errors

type loaded_workspace = {
  workspace: Workspace.t;
  workspace_manager: Workspace_manager.t;
}

let load_workspace_strict = fun cwd ->
  let workspace_manager = Workspace_manager.create () in
  match Workspace_manager.scan workspace_manager cwd with
  | Error err ->
      Error (Failure err)
  | Ok (_workspace, load_errors) when List.length load_errors > 0 ->
      print_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Ok (workspace, _) ->
      Ok { workspace; workspace_manager }

let build_command = fun ?workspace ?(prepared = false) ?(scope = Runtime) ?(profile = "debug") ?(mode = Human) ?(show_finished_summary = true) package_opt target_arch ->
  let loaded_workspace =
    match workspace with
    | Some workspace -> Ok { workspace; workspace_manager = Workspace_manager.create () }
    | None ->
        let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
        load_workspace_strict cwd
  in
  match loaded_workspace with
  | Error _ as err -> err
  | Ok { workspace; workspace_manager } ->
      run_request
        (
          make_request ~workspace ~workspace_manager ~scope ~profile ~mode ~show_finished_summary ~prepared ~packages:((package_opt
          |> Option.to_list))
            ~targets:((
              match target_arch with
              | Some target -> Riot_build.Pattern target
              | None -> Riot_build.Host
            ))
            ()
        )

let build_packages_command = fun ~workspace ?(scope = Runtime) ?(mode = Human) ?(show_finished_summary = true) package_names target_arch ->
  run_request
    (
      make_request ~workspace ~scope ~mode ~show_finished_summary ~packages:package_names
        ~targets:((
          match target_arch with
          | Some target -> Riot_build.Pattern target
          | None -> Riot_build.Host
        ))
        ()
    )

let run = fun ~workspace matches -> run_request (request_of_matches ~workspace matches)
