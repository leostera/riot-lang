open Std
open Std.Result.Syntax

type build_event =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of Event.runtime_phase

type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | InvalidRequestedParallelism of int
  | PackageNotFound of {
      package_name: Riot_model.Package_name.t;
      available_packages: Riot_model.Package_name.t list
    }
  | PackagesNotFound of {
      package_names: Riot_model.Package_name.t list;
      available_packages: Riot_model.Package_name.t list
    }
  | BuildFailed of { errors: Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | UnexpectedError of { reason: string }

type build_context = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: Build_spec.scope;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  allow_partial_failures: bool;
  record_cache_generation: bool;
  on_event: build_event -> unit;
}

let no_event: build_event -> unit = fun _ -> ()

let emit_runtime_phase = fun context phase -> context.on_event (Phase phase)

let emit_targets_resolved = fun context targets ->
  emit_runtime_phase context (Event.TargetsResolved { target_count = List.length targets })

let emit_toolchains_ensured = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsEnsured { target_count = List.length targets })

let emit_toolchains_validated = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsValidated { target_count = List.length targets })

let emit_runtime_starting = fun context -> emit_runtime_phase context Event.RuntimeStarting

let emit_runtime_started = fun context -> emit_runtime_phase context Event.RuntimeStarted

let emit_target_build_started = fun context target ->
  let host = Riot_model.Target.equal target context.host in
  context.on_event (BuildingTarget { target; host });
  emit_runtime_phase context (Event.TargetBuildStarted { target; host })

let emit_target_build_finished = fun context ~target ~result_count ~had_partial_failure ->
  emit_runtime_phase
    context
    (Event.TargetBuildFinished { target; result_count; had_partial_failure })

let emit_cache_generation_recording_started = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase context (Event.CacheGenerationRecordingStarted { lane_count; new_entry_count })

let emit_cache_generation_recorded = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase context (Event.CacheGenerationRecorded { lane_count; new_entry_count })

let emit_returning_results = fun context ~result_count ~had_partial_failure ->
  emit_runtime_phase context (Event.ReturningResults { result_count; had_partial_failure })

let error_message = function
  | ToolchainInstallFailed { target; error } -> "Failed to install toolchain for "
  ^ Riot_model.Target.to_string target
  ^ ": "
  ^ error
  | ToolchainInitializationFailed { target; error } -> "Failed to initialize toolchain for "
  ^ Riot_model.Target.to_string target
  ^ ": "
  ^ error
  | InvalidRequestedParallelism value ->
      "invalid requested parallelism (" ^ Int.to_string value ^ "): jobs must be >= 1"
  | PackageNotFound { package_name; available_packages } ->
      "Package '" ^ Riot_model.Package_name.to_string package_name
      ^ "' not found. Available packages: "
      ^ String.concat ", " (List.map available_packages ~fn:Riot_model.Package_name.to_string)
  | PackagesNotFound { package_names; available_packages } ->
      "Packages not found: "
      ^ String.concat ", " (List.map package_names ~fn:Riot_model.Package_name.to_string)
      ^ ". Available packages: "
      ^ String.concat ", " (List.map available_packages ~fn:Riot_model.Package_name.to_string)
  | BuildFailed { errors } -> (
      let failures = Build_result.failures_of_build_results errors in
      match failures with
      | [] -> "build failed"
      | [ failure ] -> Build_result.failure_message failure
      | _ ->
          "build failed:\n"
          ^ String.concat "\n" (List.map failures ~fn:Build_result.failure_message)
    )
  | PlanningFailed { reason } -> "planning failed: " ^ reason
  | CycleDetected { cycle_nodes } ->
      "cyclic dependency detected: " ^ String.concat " -> " cycle_nodes
  | BuildAlreadyRunning { lock_path } ->
      "another riot build is already running (" ^ Path.to_string lock_path ^ ")"
  | UnexpectedError { reason } -> reason

let make_context = fun ~allow_partial_failures ?(record_cache_generation = true) ?(on_event = no_event) spec ->
  let open Std.Result.Syntax in
  let session_id = Riot_model.Session_id.make () in
  let workspace = Build_spec.workspace spec in
  let host = Riot_model.Target.current in
  let toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace.root in
  let* parallelism =
    match Build_spec.requested_parallelism spec with
    | Some requested when requested < 1 -> Error (InvalidRequestedParallelism requested)
    | Some requested -> Ok (Int.min Thread.available_parallelism requested)
    | None -> Ok Thread.available_parallelism
  in
  Ok {
    session_id;
    workspace;
    package_names = Build_spec.package_names spec;
    targets = Build_spec.targets spec;
    scope = Build_spec.scope spec;
    profile = Build_spec.profile spec;
    host;
    toolchain_config;
    parallelism = Int.max 1 parallelism;
    allow_partial_failures;
    record_cache_generation;
    on_event;
  }

let ensure_toolchains_for_targets = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let missing =
    List.filter targets
      ~fn:(fun target ->
        match Riot_toolchain.check_toolchain_status ~version:context.toolchain_config.version ~target with
        | Riot_toolchain.NotInstalled _
        | Riot_toolchain.Incomplete _ -> true
        | Riot_toolchain.Installed _ -> false)
  in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.download_and_install_toolchain
          context.toolchain_config.version
          ~host:context.host
          ~target with
        | Ok () -> loop rest
        | Error error -> Error (ToolchainInstallFailed { target; error })
      )
  in
  let* () = loop missing in
  emit_toolchains_ensured context targets;
  Ok ()

let validate_target_toolchains = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.init_for_target ~config:context.toolchain_config ~target with
        | Ok _ -> loop rest
        | Error error -> Error (ToolchainInitializationFailed { target; error })
      )
  in
  let* () = loop targets in
  emit_toolchains_validated context targets;
  Ok ()

let sort_uniq_strings = fun values ->
  let rec dedupe acc = function
    | [] -> List.reverse acc
    | [ value ] -> List.reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        if String.equal left right then
          dedupe acc rest
        else
          dedupe (left :: acc) rest
  in
  values |> List.sort ~compare:String.compare |> dedupe []

let referenced_hashes_of_artifact = fun (artifact: Riot_store.Artifact.t) ->
  Std.Crypto.Digest.hex artifact.hash
  :: List.map
    artifact.exports
    ~fn:(fun (entry: Riot_store.Manifest.export_entry) -> entry.action_hash)
  |> sort_uniq_strings

let generation_lane_of_results = fun ~profile ~target results ->
  let hashes =
    List.flat_map results
      ~fn:(fun (result: Package_builder.build_result) ->
        match result.status with
        | Package_builder.Built artifact
        | Package_builder.Cached artifact -> referenced_hashes_of_artifact artifact
        | Package_builder.Skipped _
        | Package_builder.Failed _ -> [])
    |> sort_uniq_strings
  in
  Riot_store.Cache_gc.{ profile; target; hashes }

let new_entries_of_results = fun ~profile ~target results ->
  List.filter_map results
    ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Built artifact -> Some Riot_store.Cache_gc.{
        profile;
        target;
        hash = Std.Crypto.Digest.hex artifact.Riot_store.Artifact.hash
      }
      | Package_builder.Cached _
      | Package_builder.Skipped _
      | Package_builder.Failed _ -> None)

let record_successful_build_cache_generation = fun context lane_results ->
  let lanes =
    List.map
      lane_results
      ~fn:(fun (target, results) ->
        generation_lane_of_results ~profile:context.profile.name ~target results)
  in
  let new_entries = List.map
    lane_results
    ~fn:(fun (target, results) -> new_entries_of_results ~profile:context.profile.name ~target results)
  |> List.concat in
  match Riot_store.Cache_gc.record_successful_build ~workspace:context.workspace ~lanes ~new_entries with
  | Ok _ -> Ok ()
  | Error _ -> Ok ()

let new_entry_count_of_lane_results = fun context lane_results ->
  List.map
    lane_results
    ~fn:(fun (target, results) -> new_entries_of_results ~profile:context.profile.name ~target results)
  |> List.concat
  |> List.length

let record_cache_generation_if_needed = fun context lane_results had_partial_failure ->
  let new_entry_count = new_entry_count_of_lane_results context lane_results in
  if context.record_cache_generation && not had_partial_failure then
    (
      let lane_count = List.length lane_results in
      let emit_cache_generation_events = new_entry_count > 0 in
      if emit_cache_generation_events then
        emit_cache_generation_recording_started context ~lane_count ~new_entry_count;
      let* () = record_successful_build_cache_generation context lane_results in
      if emit_cache_generation_events then
        emit_cache_generation_recorded context ~lane_count ~new_entry_count;
      Ok ()
    )
  else
    Ok ()

let failed_results = fun results ->
  List.filter results
    ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Failed _ -> true
      | Package_builder.Built _
      | Package_builder.Cached _
      | Package_builder.Skipped _ -> false)

let prepare_lane = fun context ~toolchain target ->
  Build_lane.prepare
    ~workspace:context.workspace
    ~package_names:context.package_names
    ~scope:context.scope
    ~profile:context.profile
    ~session_id:context.session_id
    ~host:context.host
    ~target
    ~toolchain
    ~toolchain_config:context.toolchain_config
    ~parallelism:context.parallelism

let map_lane_error = fun error ->
  UnexpectedError { reason = error }

let execute_lane = fun context lane ->
  emit_target_build_started context (Build_lane.target lane);
  match Build_lane.execute lane with
  | Ok outcome -> Ok (outcome, [])
  | Error error -> Error (map_lane_error error)

let run_lanes = fun context ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list context.targets
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let rec prepare_lanes = function
    | [] -> Ok []
    | target :: rest ->
        let* lane =
          prepare_lane context ~toolchain target
          |> Result.map_err ~fn:map_lane_error
        in
        let* lanes = prepare_lanes rest in
        Ok (lane :: lanes)
  in
  let* lanes = prepare_lanes targets in
  let lanes = List.reverse lanes in
    let results =
      Build_scheduler.run
        ~concurrency:context.parallelism
        ~tasks:lanes
        ~fn:(fun lane -> execute_lane context lane)
  in
  let results =
    List.map results ~fn:(fun (lane, outcome) -> (lane, outcome))
  in
  let has_failures =
    List.exists (fun (_, outcome) ->
      match outcome with
      | Error _ -> true
      | Ok (lane_outcome: Build_lane.outcome) -> lane_outcome.had_partial_failure)
      results
  in
  let all_errors =
    List.filter_map results
      ~fn:(fun (_, outcome) ->
        match outcome with
        | Error err -> Some err
        | Ok _ -> None)
  in
  let lane_results =
    List.filter_map results
      ~fn:(fun (lane, outcome) ->
        match outcome with
        | Ok (lane_outcome: Build_lane.outcome) -> Some (lane_outcome.target, lane_outcome.results)
        | Error _ -> None)
  in
  let () =
    List.for_each results ~fn:(fun (lane, outcome) ->
      let had_partial_failure =
        match outcome with
        | Ok (lane_outcome: Build_lane.outcome) -> lane_outcome.had_partial_failure
        | Error _ -> true
      in
      let result_count =
        match outcome with
        | Ok (lane_outcome: Build_lane.outcome) -> List.length lane_outcome.results
        | Error _ -> 0
      in
      emit_target_build_finished
        context
        ~target:(Build_lane.target lane)
        ~result_count
        ~had_partial_failure)
  in
  if List.length all_errors > 0 then
    match List.head all_errors with
    | Some err -> Error err
    | None -> Ok (lane_results, has_failures)
  else
    if has_failures && not context.allow_partial_failures then
      let failures =
        lane_results
        |> List.map ~fn:(fun (_, results) -> failed_results results)
        |> List.concat
      in
      Error (BuildFailed { errors = failures })
    else
      Ok (lane_results, has_failures)

let do_build = fun context ->
  let targets = Riot_model.Target.Set.to_list context.targets in
  emit_targets_resolved context targets;
  let* () = ensure_toolchains_for_targets context context.targets in
  let* () = validate_target_toolchains context context.targets in
  emit_runtime_starting context;
  let* toolchain = Riot_toolchain.init ~config:context.toolchain_config
  |> Result.map_err ~fn:(fun error -> ToolchainInitializationFailed { target = context.host; error }) in
  emit_runtime_started context;
  let* (lane_results, had_partial_failure) = run_lanes context ~toolchain in
  let all_results =
    List.map ~fn:(fun (_, results) -> results) lane_results
    |> List.concat
  in
  let* () = record_cache_generation_if_needed context lane_results had_partial_failure in
  emit_returning_results context ~result_count:(List.length all_results) ~had_partial_failure;
  Ok all_results

let execute = fun ?(allow_partial_failures = false) ?(record_cache_generation = true) ?(on_event = no_event) spec ->
  let* context = make_context ~allow_partial_failures ~record_cache_generation ~on_event spec in
  do_build context
