open Std
open Std.Collections
open Std.Result.Syntax
open Riot_model
open Riot_build
module Build_telemetry = Riot_build.Internal.Telemetry_events

type build_scope = Riot_build.Request.scope =
  Runtime
  | Dev

type dev_artifacts = Riot_build.Request.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}

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
  workspace: Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
  output_mode: output_mode;
  show_finished_summary: bool;
}

let build_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_BUILD_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_build = fun message ->
  let _ = message in
  ()

let trace_build_probe = fun ~started_at message ->
  let _ = started_at in
  let _ = message in
  ()

let build_request_label = fun (request: request) ->
  let packages =
    match request.packages with
    | [] -> "all"
    | packages -> packages |> List.map ~fn:Riot_model.Package_name.to_string |> String.concat ","
  in
  let targets =
    match request.targets with
    | Riot_model.Target.Host -> "host"
    | Riot_model.Target.All -> "all"
    | Riot_model.Target.Pattern pattern -> pattern
    | Riot_model.Target.Exact targets -> Riot_model.Target.Set.to_list targets
    |> List.map ~fn:Riot_model.Target.to_string
    |> String.concat ","
  in
  "Build(" ^ packages ^ "; targets=" ^ targets ^ "; profile=" ^ request.profile.name ^ ")"

let json_clock_origin = ref None

let reset_json_clock = fun ~started_at -> json_clock_origin := Some started_at

let event_elapsed_us = fun () ->
  match !json_clock_origin with
  | Some origin -> Time.Instant.elapsed origin |> Time.Duration.to_micros
  | None ->
      let origin = Time.Instant.now () in
      json_clock_origin := Some origin;
      0

let stamp_json_event = fun (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      let fields =
        if Option.is_some
            (
              List.find fields
                ~fn:(fun (name, _) ->
                  String.equal name "emitted_at_us")
            ) then
          fields
        else
          fields @ [ ("emitted_at_us", Data.Json.Int (event_elapsed_us ())) ]
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun (json: Data.Json.t) ->
  println (Data.Json.to_string (stamp_json_event json))

let write_build_event_json = fun event ->
  match Riot_build.Event.to_json event with
  | Some json -> write_json_event json
  | None -> ()

let display_package_name = fun (package: Riot_model.Package.t) ->
  let name = Riot_model.Package_name.to_string package.name in
  if Riot_model.Package.is_workspace_member package then
    name
  else
    match package.publish.version with
    | Some version -> name ^ " (" ^ Std.Version.to_string version ^ ")"
    | None -> name

let labeled_multiline_lines = fun ~label value ->
  match String.split value ~by:"\n" with
  | [] -> [ label ^ ":" ]
  | first :: rest ->
      (label ^ ": " ^ first) :: List.map rest
        ~fn:(fun line ->
          if String.equal line "" then
            ""
          else
            "  " ^ line)

let planning_error_lines = function
  | Riot_planner.Planning_error.CyclicDependency { cycle } ->
      [ "planner error: cyclic dependency detected"; "cycle: " ^ String.concat " -> " cycle; ]
  | Riot_planner.Planning_error.ScanFailed { path; reason } ->
      [ "planner error: failed to scan package sources"; "path: " ^ Path.to_string path; ]
      @ labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.DependencyAnalysisFailed { reason } ->
      "planner error: dependency analysis failed" :: labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.GraphBuildFailed { reason } ->
      "planner error: failed to build module graph" :: labeled_multiline_lines ~label:"reason" reason
  | Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module;

  } ->
      [
        "planner error: target reaches a library-internal module";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "help: use " ^ public_module ^ "." ^ requested_module ^ " instead";
      ]
  | Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module;

  } ->
      let public_leaf = internal_module
      |> String.split ~by:"__"
      |> List.reverse
      |> List.head
      |> Option.unwrap_or ~default:requested_module in
      [
        "planner error: target reaches a namespaced library-internal module";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "internal module: " ^ internal_module;
        "public module: " ^ public_module;
        "help: use " ^ public_module ^ "." ^ public_leaf ^ " instead";
      ]
  | Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
    target_name;
    source;
    requested_module;
    other_target_name;
    other_target_module;
    public_module;

  } ->
      [
        "planner error: target reaches another target root";
        "target: " ^ target_name;
        "source: " ^ Path.to_string source;
        "requested module: " ^ requested_module;
        "other target: " ^ other_target_name;
        "other target module: " ^ other_target_module;
        "public module: " ^ public_module;
        "help: move shared code behind " ^ public_module ^ " or a shared helper module";
      ]
  | Riot_planner.Planning_error.Exception { exn } ->
      "planner error: unexpected exception"
      :: labeled_multiline_lines ~label:"reason" (Kernel.Exception.to_string exn)

let workspace_load_error_line = function
  | Riot_model.Workspace_manager.PackageNotFound { package; path; dependant=None } -> "missing package: "
  ^ package
  ^ " ("
  ^ path
  ^ ")"
  | Riot_model.Workspace_manager.PackageNotFound { package; path; dependant=Some dependant } -> "missing package: "
  ^ package
  ^ " (required by "
  ^ dependant
  ^ ", "
  ^ path
  ^ ")"
  | Riot_model.Workspace_manager.PackageTomlReadFailed { package; path } -> "failed to read package toml: "
  ^ package
  ^ " ("
  ^ path
  ^ ")"
  | Riot_model.Workspace_manager.PackageTomlParseFailed { package; path } -> "failed to parse package toml: "
  ^ package
  ^ " ("
  ^ path
  ^ ")"
  | Riot_model.Workspace_manager.PackageFromTomlFailed { package; path; error } -> "failed to load package manifest: "
  ^ package
  ^ " ("
  ^ path
  ^ "): "
  ^ Riot_model.Package_manifest.error_message error

let workspace_planning_error_lines = function
  | Riot_planner.Workspace_planner.PackageNotFound { name; available } -> [
    "planner error: package not found";
    "package: " ^ Riot_model.Package_name.to_string name;
    "available packages: "
    ^ String.concat ", " (List.map available ~fn:Riot_model.Package_name.to_string);
  ]
  | Riot_planner.Workspace_planner.PackagesNotFound { names; available } -> [
    "planner error: packages not found";
    "packages: " ^ String.concat ", " (List.map names ~fn:Riot_model.Package_name.to_string);
    "available packages: "
    ^ String.concat ", " (List.map available ~fn:Riot_model.Package_name.to_string);
  ]
  | Riot_planner.Workspace_planner.CycleDetected { cycle } -> [
    "planner error: package cycle detected";
    "cycle: " ^ String.concat " -> " cycle;
  ]
  | Riot_planner.Workspace_planner.MissingDependencies { missing } -> "planner error: missing dependencies"
  :: List.map
    missing
    ~fn:(fun (dep: Riot_planner.Package_graph.missing_dependency) ->
      "missing: " ^ dep.package ^ " -> " ^ dep.dependency)
  | Riot_planner.Workspace_planner.PackageLoadFailed { errors } -> "planner error: failed to load workspace packages"
  :: List.map errors ~fn:workspace_load_error_line

let out_prefixed_payload = fun ~prefix payload ->
  match String.split payload ~by:"\n" with
  | [] -> ()
  | first_line :: rest ->
      out (prefix ^ first_line);
      rest |> List.for_each ~fn:out

let telemetry_package_error_message = function
  | Build_telemetry.PlanningFailed planning_error -> Riot_planner.Planning_error.to_string planning_error
  | Build_telemetry.ExecutionFailed { message }
  | Build_telemetry.ActionExecutionFailed { message } -> message
  | Build_telemetry.ActionOutputsNotCreated { missing } -> "missing outputs: "
  ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | Build_telemetry.ActionDependenciesFailed { failed } -> "failed dependencies: "
  ^ String.concat ", " (List.map failed ~fn:Graph.SimpleGraph.Node_id.to_string)

let write_build_telemetry_event = fun ~mode event ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Telemetry event)
  | Human -> (
      match event with
      | Build_telemetry.CompilationStarted { package; _ } ->
          out ("    \027[1;32mBuilding\027[0m " ^ display_package_name package)
      | Build_telemetry.BuildCompleted { package; status=`Fresh; _ } ->
          out ("       \027[1;32mBuilt\027[0m " ^ display_package_name package)
      | Build_telemetry.BuildCompleted { package; status=`Cached; _ } ->
          out ("      \027[1;34mCached\027[0m " ^ display_package_name package)
      | Build_telemetry.BuildSkipped { package; reason; _ } ->
          out ("      \027[1;33mSkipped\027[0m " ^ display_package_name package ^ ": " ^ reason)
      | Build_telemetry.BuildFailed { package; error=PlanningFailed planning_error; _ } ->
          out ("      \027[1;31mFailed\027[0m " ^ display_package_name package);
          planning_error_lines planning_error
          |> List.for_each ~fn:(fun line -> out ("        " ^ line))
      | Build_telemetry.BuildFailed { package; error; _ } ->
          out
            ("      \027[1;31mFailed\027[0m "
            ^ display_package_name package
            ^ ": "
            ^ telemetry_package_error_message error)
      | Build_telemetry.PackageOcamlcWarnings { package; messages; _ } ->
          messages
          |> List.for_each
            ~fn:(fun message ->
              out_prefixed_payload
                ~prefix:("     \027[1;33mWarning\027[0m " ^ display_package_name package ^ ": ")
                message)
      | Build_telemetry.BuildStarted _
      | Build_telemetry.WorkspacePlanStarted _
      | Build_telemetry.WorkspacePlanCompleted _
      | Build_telemetry.WorkspaceManifestFilterCompleted _
      | Build_telemetry.WorkspaceGraphCreated _
      | Build_telemetry.WorkspaceTargetGraphFiltered _
      | Build_telemetry.WorkspaceTopologicalSortCompleted _
      | Build_telemetry.PlanningWorkspaceStarted _
      | Build_telemetry.PlanningWorkspaceCompleted _
      | Build_telemetry.PackagePlanningResult _
      | Build_telemetry.PackagePlanningBreakdown _
      | Build_telemetry.ActionStarted _
      | Build_telemetry.ActionCommandStarted _
      | Build_telemetry.ActionCompleted _
      | Build_telemetry.ActionFailed _
      | Build_telemetry.CacheHit _
      | Build_telemetry.CacheMiss _
      | Build_telemetry.WorkspaceStarted _
      | Build_telemetry.WorkspaceCompleted _ ->
          ()
      | _ ->
          ()
    )

let write_build_phase_event = fun ~mode phase ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Phase phase)
  | Human -> (
      match phase with
      | Riot_build.Event.TargetsResolved { target_count } ->
          out ("    Resolved " ^ Int.to_string target_count ^ " target(s)")
      | Riot_build.Event.ToolchainsEnsured { target_count } ->
          out ("    Ensured toolchains for " ^ Int.to_string target_count ^ " target(s)")
      | Riot_build.Event.ToolchainsValidated { target_count } ->
          out ("    Validated toolchains for " ^ Int.to_string target_count ^ " target(s)")
      | Riot_build.Event.RuntimeStarting ->
          out "    Starting build runtime"
      | Riot_build.Event.RuntimeStarted ->
          out "    Build runtime ready"
      | Riot_build.Event.BuildLockWaiting _ ->
          out "    Build lock is taken, waiting..."
      | Riot_build.Event.PackagePlanningStarted { package_count; _ } ->
          out ("    Planning " ^ Int.to_string package_count ^ " package(s)")
      | Riot_build.Event.PackagePlanningFinished {
        execution_required_count;
        cached_count;
        skipped_count;
        failed_count;
        error_count;
        _
      } ->
          let parts = [
            Some (Int.to_string execution_required_count ^ " to build");
            Some (Int.to_string cached_count ^ " cached");
            Some (Int.to_string skipped_count ^ " skipped");
            Some (Int.to_string failed_count ^ " failed");
            Some (Int.to_string error_count ^ " errored");
          ]
          |> List.filter_map ~fn:(fun value -> value) in
          out ("    Planned packages (" ^ String.concat ", " parts ^ ")")
      | Riot_build.Event.PackageExecutionStarted { package_count; _ } ->
          out ("    Executing " ^ Int.to_string package_count ^ " package(s)")
      | Riot_build.Event.PackageExecutionFinished { built_count; failed_count; error_count; _ } ->
          out
            ("    Package execution finished ("
            ^ Int.to_string built_count
            ^ " built, "
            ^ Int.to_string failed_count
            ^ " failed, "
            ^ Int.to_string error_count
            ^ " errored)")
      | Riot_build.Event.TargetBuildStarted _
      | Riot_build.Event.TargetBuildFinished _
      | Riot_build.Event.CacheGenerationRecordingStarted _
      | Riot_build.Event.CacheGenerationRecorded _
      | Riot_build.Event.ReturningResults _ ->
          ()
    )

let command_error_event_to_json = fun kind details ->
  Data.Json.Object (("type", Data.Json.String kind) :: details)

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Riot_model.Event.RegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates ~value:registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates ~value:registry in
          Some ("    \027[1;32mUpdating\027[0m " ^ registry ^ " index")
        )
  | Riot_model.Event.PackageResolvedForBuild _ ->
      None
  | Riot_model.Event.PackageDownloadStarted { package; version; _ } ->
      Some ("    \027[1;32mFetching\027[0m " ^ Riot_model.Package_name.to_string package ^ " " ^ version)
  | Riot_model.Event.PackageDownloadQueued { package; version; _ } ->
      Some ("      \027[1;33mQueued\027[0m "
      ^ Riot_model.Package_name.to_string package
      ^ " ("
      ^ version
      ^ ")")
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
        "  \027[1;34mInstalling\027[0m " ^ (
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
      Some ("      \027[1;32mLocked\027[0m "
      ^ Riot_model.Package_name.to_string package
      ^ " ("
      ^ version
      ^ ")")
  | Riot_model.Event.PackageVersionsUnchanged _ ->
      Some "    Dependencies are already up to date"
  | Riot_model.Event.PackageVersionUpdated { package; from_version; to_version } ->
      Some ("    \027[1;32mUpdated\027[0m "
      ^ Riot_model.Package_name.to_string package
      ^ " ("
      ^ from_version
      ^ " -> "
      ^ to_version
      ^ ")")
  | kind ->
      Some (Riot_model.Event.display kind)

let write_pm_event = fun ~mode ~seen_registry_updates event ->
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.Pm event)
  | Human -> (
      match format_pm_event ~seen_registry_updates event.kind with
      | Some message -> out message
      | None -> ()
    )

let write_command_error = fun ~mode kind details human_message ->
  match mode with
  | Json -> write_json_event (command_error_event_to_json kind details)
  | Human -> out ("\027[1;31mError\027[0m: " ^ human_message)

let build_failure_detail_lines = fun (failure: Riot_build.Build_result.failure) ->
  let package_name = Riot_model.Package_name.to_string failure.package_name in
  match failure.reason with
  | Riot_build.Build_result.PackagePlanningFailed planning_error -> ("package: " ^ package_name)
  :: List.map (planning_error_lines planning_error) ~fn:(fun line -> "  " ^ line)
  | _ -> [ package_name ^ ": " ^ failure.message ]

let write_build_failed_error = fun ~mode errors ->
  match mode with
  | Json -> write_json_event
    (command_error_event_to_json
      "BuildFailed"
      [ ("errors", Data.Json.Array (List.map errors ~fn:Riot_build.Build_result.failure_to_json)); ])
  | Human ->
      out "\027[1;31mError\027[0m: build failed";
      errors
      |> List.for_each
        ~fn:(fun failure ->
          build_failure_detail_lines failure |> List.for_each ~fn:(fun line -> out ("  " ^ line)))

let command =
  let open ArgParser in
    let open Arg in
      command "build" |> about "Build packages" |> args
        [
          option "package" |> short 'p' |> long "package" |> multiple |> help "Build a specific package. Repeat to build multiple packages; omit to build all packages.";
          option "target" |> short 'x' |> long "target" |> help "Target architecture (exact triple, pattern like 'linux'/'aarch64', or 'all')";
          flag "all-targets" |> long "all-targets" |> help "Build for all configured targets";
          flag "tests" |> long "tests" |> help "Also compile test binaries";
          flag "examples" |> long "examples" |> help "Also compile example binaries";
          flag "benches" |> long "benches" |> help "Also compile benchmark binaries";
          flag "all" |> long "all" |> help "Also compile tests, examples, and benchmark binaries";
          flag "release" |> long "release" |> help "Use the release build profile";
          option "jobs" |> short 'j' |> long "jobs" |> help "Limit parallel workers";
          flag "json" |> long "json" |> help "Emit machine-readable JSONL events";
        ]

let target_request_of_matches = fun matches ->
  if ArgParser.get_flag matches "all-targets" then
    Riot_model.Target.All
  else
    match ArgParser.get_one matches "target" with
    | Some value -> Riot_model.Target.parse value
    | None -> Riot_model.Target.Host

let output_mode_of_matches = fun matches ->
  if ArgParser.get_flag matches "json" then
    Json
  else
    Human

let dev_artifacts_of_matches = fun matches ->
  if ArgParser.get_flag matches "all" then
    { tests = true; examples = true; benches = true }
  else
    {
      tests = ArgParser.get_flag matches "tests";
      examples = ArgParser.get_flag matches "examples";
      benches = ArgParser.get_flag matches "benches"
    }

let scope_of_dev_artifacts = fun (dev_artifacts: dev_artifacts) ->
  if dev_artifacts.tests || dev_artifacts.examples || dev_artifacts.benches then
    Dev
  else
    Runtime

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    Riot_model.Profile.release
  else
    Riot_model.Profile.debug

let requested_parallelism_of_matches = fun matches ->
  match ArgParser.get_one matches "jobs" with
  | None -> Ok None
  | Some value -> (
      match Int.parse value with
      | Some workers -> Ok (Some workers)
      | None -> Error (Failure ("invalid --jobs value: " ^ value))
    )

let parse_package_names = fun package_names ->
  let rec loop acc = function
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error -> Error (Failure ("invalid package name '"
        ^ package_name
        ^ "': "
        ^ Riot_model.Package_name.error_message error))
      )
  in
  loop [] package_names

let make_request = fun ~workspace ?(scope = Runtime) ?(dev_artifacts = {
  tests = true;
  examples = true;
  benches = true
}) ?(profile = Riot_model.Profile.debug) ?(mode = Human) ?(show_finished_summary = true) ?(requested_parallelism = None) ~packages ~targets () ->
  {
    workspace;
    packages;
    targets;
    scope;
    dev_artifacts;
    profile;
    requested_parallelism;
    output_mode = mode;
    show_finished_summary;
  }

let request_of_matches = fun ~workspace matches ->
  match parse_package_names (ArgParser.get_many matches "package") with
  | Error _ as err -> err
  | Ok packages -> (
      match requested_parallelism_of_matches matches with
      | Error _ as err -> err
      | Ok requested_parallelism ->
          let dev_artifacts = dev_artifacts_of_matches matches in
          Ok (make_request
            ~workspace
            ~scope:(scope_of_dev_artifacts dev_artifacts)
            ~dev_artifacts
            ~profile:(profile_of_matches matches)
            ~mode:(output_mode_of_matches matches)
            ~requested_parallelism
            ~packages
            ~targets:(target_request_of_matches matches)
            ())
    )

let write_building_target_event = fun ~mode ~target ~host ->
  let target_name = Riot_model.Target.to_string target in
  match mode with
  | Json -> write_build_event_json (Riot_build.Event.BuildingTarget { target; host })
  | Human ->
      if not host then
        out ("🔨 Cross-compiling for " ^ target_name)

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
  if Int64.compare size_bytes tib != Order.LT then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib != Order.LT then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib != Order.LT then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib != Order.LT then
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
      | Riot_store.Cache_gc.GcStarted { trigger=Manual } -> out "    Running tracked cache GC (build root kept; use --force to remove it)"
      | Riot_store.Cache_gc.GcStarted { trigger=Post_build } -> ()
      | Riot_store.Cache_gc.GcSkipped { trigger=Post_build; _ } -> ()
      | Riot_store.Cache_gc.GcSkipped { summary; _ } -> out
        ("    Cache GC skipped: tracked cache is already within policy ("
        ^ size_to_string summary.size_after_bytes
        ^ "). Build root kept; use --force to remove it.")
      | Riot_store.Cache_gc.GcCompleted { summary; _ } -> out
        ("    \027[1;32mCleaned\027[0m tracked cache: " ^ format_cache_gc_cleanup summary ^ ". Build root kept.")
      | Riot_store.Cache_gc.GcFailed { error; _ } -> out
        ("\027[1;31mError\027[0m: cache GC failed: " ^ error)
      | Riot_store.Cache_gc.ForceCleanStarted { build_root } -> out
        ("    Removing build root " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.ForceCleanCompleted { build_root } -> out
        ("    \027[1;32mRemoved\027[0m build root " ^ Path.to_string build_root)
      | Riot_store.Cache_gc.ForceCleanFailed { build_root; error } -> out
        ("\027[1;31mError\027[0m: failed to remove build root "
        ^ Path.to_string build_root
        ^ ": "
        ^ error)
    )

let write_build_event = fun ~mode ~seen_registry_updates event ->
  match event with
  | Riot_build.Event.Pm event -> write_pm_event ~mode ~seen_registry_updates event
  | Riot_build.Event.BuildingTarget { target; host } -> write_building_target_event ~mode ~target ~host
  | Riot_build.Event.CacheGc event -> write_cache_gc_event ~mode event
  | Riot_build.Event.Telemetry event -> write_build_telemetry_event ~mode event
  | Riot_build.Event.Phase phase -> write_build_phase_event ~mode phase

let write_package_not_found_error = fun ~mode ~package_name ~available_packages ->
  let package_name = Riot_model.Package_name.to_string package_name in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  if mode = Json then
    write_json_event
      (command_error_event_to_json
        "PackageNotFound"
        [
          ("package_name", Data.Json.String package_name);
          (
            "available_packages",
            Data.Json.Array (List.map available_packages ~fn:(fun pkg -> Data.Json.String pkg))
          );
        ])
  else (
    out ("\027[1;31mError\027[0m: Package '" ^ package_name ^ "' not found");
    out "";
    out "Available packages:";
    List.for_each available_packages ~fn:(fun pkg -> out ("  • " ^ pkg))
  )

let write_packages_not_found_error = fun ~mode ~package_names ~available_packages ->
  let package_names = List.map package_names ~fn:Riot_model.Package_name.to_string in
  let available_packages = List.map available_packages ~fn:Riot_model.Package_name.to_string in
  if mode = Json then
    write_json_event
      (command_error_event_to_json
        "PackagesNotFound"
        [
          (
            "package_names",
            Data.Json.Array (List.map package_names ~fn:(fun pkg -> Data.Json.String pkg))
          );
          (
            "available_packages",
            Data.Json.Array (List.map available_packages ~fn:(fun pkg -> Data.Json.String pkg))
          );
        ])
  else (
    out ("\027[1;31mError\027[0m: Packages not found: " ^ String.concat ", " package_names);
    out "";
    out "Available packages:";
    List.for_each available_packages ~fn:(fun pkg -> out ("  • " ^ pkg))
  )

let write_build_error = fun ~mode err ->
  match err with
  | Riot_build.TargetSelectionFailed { pattern; available_targets } ->
      write_command_error
        ~mode
        "NoTargetsMatched"
        [
          ("pattern", Data.Json.String pattern);
          (
            "available_targets",
            Data.Json.Array (List.map
              available_targets
              ~fn:(fun target -> Data.Json.String (Riot_model.Target.to_string target)))
          );
        ]
        (Riot_build.error_message err)
  | Riot_build.PackageNotFound { package_name; available_packages } ->
      write_package_not_found_error ~mode ~package_name ~available_packages
  | Riot_build.PackagesNotFound { package_names; available_packages } ->
      write_packages_not_found_error ~mode ~package_names ~available_packages
  | Riot_build.ToolchainInstallFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInstallFailed"
        [
          ("target", Data.Json.String (Riot_model.Target.to_string target));
          ("reason", Data.Json.String (Riot_build.toolchain_install_error_message error));
        ]
        (Riot_build.error_message err)
  | Riot_build.ToolchainInitializationFailed { target; error } ->
      write_command_error
        ~mode
        "ToolchainInitializationFailed"
        [
          ("target", Data.Json.String (Riot_model.Target.to_string target));
          ("reason", Data.Json.String (Riot_build.toolchain_initialization_error_message error));
        ]
        (Riot_build.error_message err)
  | Riot_build.BuildFailed { errors } ->
      write_build_failed_error ~mode errors
  | Riot_build.PlanningFailed planning_error ->
      if mode = Json then
        write_json_event
          (
            command_error_event_to_json "PlanningFailed"
              [ (
                  "error",
                  Riot_planner.Workspace_planner.(match planning_error with
                  | PackageNotFound { name; available } -> Data.Json.obj
                    [
                      ("type", Data.Json.string "package_not_found");
                      ("name", Data.Json.string (Riot_model.Package_name.to_string name));
                      (
                        "available",
                        Data.Json.array
                          (List.map
                            available
                            ~fn:(fun pkg -> Data.Json.string (Riot_model.Package_name.to_string pkg)))
                      );
                    ]
                  | PackagesNotFound { names; available } -> Data.Json.obj
                    [
                      ("type", Data.Json.string "packages_not_found");
                      (
                        "names",
                        Data.Json.array
                          (List.map
                            names
                            ~fn:(fun pkg -> Data.Json.string (Riot_model.Package_name.to_string pkg)))
                      );
                      (
                        "available",
                        Data.Json.array
                          (List.map
                            available
                            ~fn:(fun pkg -> Data.Json.string (Riot_model.Package_name.to_string pkg)))
                      );
                    ]
                  | CycleDetected { cycle } -> Data.Json.obj
                    [
                      ("type", Data.Json.string "cycle_detected");
                      ("cycle", Data.Json.array (List.map cycle ~fn:Data.Json.string));
                    ]
                  | MissingDependencies { missing } -> Data.Json.obj
                    [
                      ("type", Data.Json.string "missing_dependencies");
                      (
                        "missing",
                        Data.Json.array
                          (List.map
                            missing
                            ~fn:(fun dep ->
                              Data.Json.obj
                                [
                                  ("package", Data.Json.string dep.package);
                                  ("dependency", Data.Json.string dep.dependency);
                                ]))
                      );
                    ]
                  | PackageLoadFailed { errors } -> Data.Json.obj
                    [
                      ("type", Data.Json.string "package_load_failed");
                      (
                        "errors",
                        Data.Json.array
                          (List.map
                            errors
                            ~fn:(fun error -> Data.Json.string (workspace_load_error_line error)))
                      );
                    ])
                ) ]
          )
      else (
        out "\027[1;31mError\027[0m: planning failed";
        workspace_planning_error_lines planning_error
        |> List.for_each ~fn:(fun line -> out ("  " ^ line))
      )
  | Riot_build.CycleDetected { cycle_nodes } ->
      write_command_error
        ~mode
        "CycleDetected"
        [ ("cycle_nodes", Data.Json.Array (List.map cycle_nodes ~fn:Data.Json.string)) ]
        (Riot_build.error_message err)
  | Riot_build.BuildAlreadyRunning { lock_path } ->
      write_command_error
        ~mode
        "BuildAlreadyRunning"
        [ ("lock_path", Data.Json.String (Path.to_string lock_path)) ]
        (Riot_build.error_message err)
  | Riot_build.InvalidRequestedParallelism value ->
      write_command_error
        ~mode
        "InvalidRequestedParallelism"
        [ ("value", Data.Json.Int value) ]
        (Riot_build.error_message err)
  | Riot_build.UnexpectedError { reason } ->
      write_command_error ~mode "UnexpectedError" [ ("reason", Data.Json.String reason) ] reason

let record_output_progress = fun progress output ->
  Riot_build.Build_result.packages output |> List.for_each
    ~fn:(fun package_output ->
      match Riot_build.Build_result.package_status package_output with
      | Riot_build.Build_result.Built _ -> progress.built_count <- progress.built_count + 1
      | Riot_build.Build_result.Cached _ -> progress.cached_count <- progress.cached_count + 1
      | Riot_build.Build_result.Skipped _ -> progress.skipped_count <- progress.skipped_count + 1
      | Riot_build.Build_result.Failed _ -> progress.failed_count <- progress.failed_count + 1)

let run_request = fun (request: request) ->
  trace_build
    (
      "run_request request=" ^ build_request_label request ^ " scope=" ^ match request.scope with
      | Runtime -> "runtime"
      | Dev ->
          "dev" ^ " mode=" ^ match request.output_mode with
          | Human -> "human"
          | Json -> "json"
    );
  let seen_registry_updates = HashSet.create () in
  let start_time = Time.Instant.now () in
  reset_json_clock ~started_at:start_time;
  let progress = { built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let attempted_build = ref false in
  let pm_session_id = Riot_model.Session_id.make () in
  let emit_pm_kind kind = write_pm_event
    ~mode:request.output_mode
    ~seen_registry_updates
    (Riot_model.Event.create ~session_id:pm_session_id ~level:Riot_model.Event.Info kind) in
  let on_build_event event =
    match event with
    | Riot_build.Event.Pm kind -> emit_pm_kind kind.kind
    | _ ->
        attempted_build := true;
        write_build_event ~mode:request.output_mode ~seen_registry_updates event
  in
  let result =
    Riot_build.build
      ~on_event:on_build_event
      (Riot_build.Request.make
        ~workspace:request.workspace
        ~packages:request.packages
        ~targets:request.targets
        ~scope:request.scope
        ~dev_artifacts:request.dev_artifacts
        ~profile:request.profile
        ~requested_parallelism:request.requested_parallelism
        ())
    |> Result.map
      ~fn:(fun output ->
        record_output_progress progress output;
        ())
    |> Result.map_err
      ~fn:(fun err ->
        write_build_error ~mode:request.output_mode err;
        Failure (Riot_build.error_message err))
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
  if request.show_finished_summary && !attempted_build then
    trace_build_probe ~started_at:start_time "summary-finished";
  match result with
  | Ok _ ->
      trace_build_probe ~started_at:start_time "run-request-return-ok";
      Ok ()
  | Error err ->
      trace_build_probe
        ~started_at:start_time
        ("run-request-return-error reason=" ^ Kernel.Exception.to_string err);
      Error err

let print_workspace_load_errors = fun errors ->
  List.for_each
    errors
    ~fn:(fun err -> out ("\027[1;31mError\027[0m: " ^ Workspace_manager.load_error_to_string err))

type loaded_workspace = {
  workspace: Workspace.t;
}

let load_workspace_strict = fun cwd ->
  let workspace_manager = Workspace_manager.create () in
  let* registry = Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
  |> Result.map_err ~fn:(fun err -> Failure (Pkgs_ml.Registry_cache.create_error_message err)) in
  match Workspace_manager.scan workspace_manager cwd with
  | Error err ->
      Error (Failure (Workspace_manager.scan_error_message err))
  | Ok (_workspace, load_errors) when List.length load_errors > 0 ->
      print_workspace_load_errors load_errors;
      Error (Failure "Workspace load failed")
  | Ok (workspace, _) ->
      let* workspace = Riot_deps.ensure_workspace
        ~workspace_manager
        ~mode:Riot_deps.Dep_solver.Refresh
        ~registry
        ~workspace
        ()
      |> Result.map_err ~fn:(fun err -> Failure (Riot_model.Pm_error.message err)) in
      Ok { workspace }

let build_command = fun ~workspace ?(scope = Runtime) ?(dev_artifacts = {
  tests = true;
  examples = true;
  benches = true
}) ?(profile = "debug") ?(mode = Human) ?(show_finished_summary = true) ?(requested_parallelism = None) package_opt target_arch ->
  let packages = package_opt |> Option.to_list in
  run_request
    (
      make_request ~workspace ~scope ~dev_artifacts
        ~profile:(
          match profile with
          | "release" -> Riot_model.Profile.release
          | _ -> Riot_model.Profile.debug
        )
        ~mode
        ~show_finished_summary
        ~requested_parallelism
        ~packages
        ~targets:(
          match target_arch with
          | Some target -> Riot_model.Target.parse target
          | None -> Riot_model.Target.Host
        )
        ()
    )

let build_packages_command = fun ~workspace ?(scope = Runtime) ?(dev_artifacts = {
  tests = true;
  examples = true;
  benches = true
}) ?(mode = Human) ?(show_finished_summary = true) ?(requested_parallelism = None) package_names target_arch ->
  match parse_package_names package_names with
  | Error _ as err -> err
  | Ok packages ->
      run_request
        (
          make_request ~workspace ~scope ~dev_artifacts ~mode ~show_finished_summary ~requested_parallelism ~packages
            ~targets:(
              match target_arch with
              | Some target -> Riot_model.Target.parse target
              | None -> Riot_model.Target.Host
            )
            ()
        )

let run = fun ~workspace matches ->
  match request_of_matches ~workspace matches with
  | Error _ as err -> err
  | Ok request -> run_request request
