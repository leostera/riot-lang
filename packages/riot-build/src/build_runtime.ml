open Std
open Std.Result.Syntax

type toolchain_install_error =
  | ToolchainDownloadFailed of { message: string }

let toolchain_install_error_message = fun (ToolchainDownloadFailed { message }) -> message

type toolchain_initialization_error =
  | ToolchainInitFailed of { message: string }

let toolchain_initialization_error_message = fun (ToolchainInitFailed { message }) -> message

let toolchain_install_error = fun message -> ToolchainDownloadFailed { message }

let toolchain_initialization_error = fun message -> ToolchainInitFailed { message }

type build_error =
  | ToolchainInstallFailed of {
      target: Riot_model.Target.t;
      error: toolchain_install_error;
    }
  | ToolchainInitializationFailed of {
      target: Riot_model.Target.t;
      error: toolchain_initialization_error;
    }
  | BuildFailed of {
      errors: Package_builder.build_result list;
    }
  | PlanningFailed of Riot_planner.Workspace_planner.plan_error
  | UnexpectedError of { reason: string }

type build_context = {
  build: Build_context.t;
  resolved: Resolved_build.t;
  allow_partial_failures: bool;
  record_cache_generation: bool;
}

let telemetry_handler_id = fun context ->
  "riot-build:" ^ Riot_model.Session_id.to_string context.build.Build_context.session_id

let with_telemetry_bridge = fun context fn ->
  let _ = Std.Telemetry.start () in
  let handler_id = telemetry_handler_id context in
  Std.Telemetry.attach handler_id (Build_context.forward_telemetry_event context.build);
  try
    let result = fn () in
    Build_context.flush_events context.build;
    Std.Telemetry.detach handler_id;
    result
  with
  | exn ->
      Build_context.flush_events context.build;
      Std.Telemetry.detach handler_id;
      raise exn

let emit_runtime_phase = fun context phase -> Build_context.emit_phase context.build phase

let emit_targets_resolved = fun context targets ->
  emit_runtime_phase
    context
    (Event.TargetsResolved { target_count = List.length targets })

let emit_toolchains_ensured = fun context targets ->
  emit_runtime_phase
    context
    (Event.ToolchainsEnsured { target_count = List.length targets })

let emit_toolchains_validated = fun context targets ->
  emit_runtime_phase
    context
    (Event.ToolchainsValidated { target_count = List.length targets })

let emit_runtime_starting = fun context -> emit_runtime_phase context Event.RuntimeStarting

let emit_runtime_started = fun context -> emit_runtime_phase context Event.RuntimeStarted

let emit_target_build_started = fun context target ->
  let host = Riot_model.Target.equal target context.build.host in
  Build_context.emit_building_target context.build ~target ~host;
  emit_runtime_phase context (Event.TargetBuildStarted { target; host })

let emit_target_build_finished = fun context ~target ~result_count ~had_partial_failure ->
  emit_runtime_phase
    context
    (Event.TargetBuildFinished { target; result_count; had_partial_failure })

let emit_cache_generation_recording_started = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase
    context
    (Event.CacheGenerationRecordingStarted { lane_count; new_entry_count })

let emit_cache_generation_recorded = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase
    context
    (Event.CacheGenerationRecorded { lane_count; new_entry_count })

let emit_returning_results = fun context ~result_count ~had_partial_failure ->
  emit_runtime_phase
    context
    (Event.ReturningResults { result_count; had_partial_failure })

let error_message = fun __tmp1 ->
  match __tmp1 with
  | ToolchainInstallFailed { target; error } ->
      "Failed to install toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ toolchain_install_error_message error
  | ToolchainInitializationFailed { target; error } ->
      "Failed to initialize toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ toolchain_initialization_error_message error
  | BuildFailed { errors } -> (
      let failures = Build_result.failures_of_build_results errors in
      match failures with
      | [] -> "build failed"
      | [ failure ] -> Build_result.failure_message failure
      | _ ->
          "build failed:\n"
          ^ String.concat "\n" (List.map failures ~fn:Build_result.failure_message)
    )
  | PlanningFailed error -> Build_lane.error_message (Build_lane.PlanningFailed error)
  | UnexpectedError { reason } -> reason

let make_context = fun ~allow_partial_failures ?(record_cache_generation = true) build spec ->
  Ok {
    build;
    resolved = spec;
    allow_partial_failures;
    record_cache_generation;
  }

let ensure_toolchains_for_targets = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let missing =
    List.filter
      targets
      ~fn:(fun target ->
        match Riot_toolchain.check_toolchain_status
          ~version:context.build.toolchain_config.version
          ~target with
        | Riot_toolchain.NotInstalled _
        | Riot_toolchain.Incomplete _ -> true
        | Riot_toolchain.Installed _ -> false)
  in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.download_and_install_toolchain
          context.build.toolchain_config.version
          ~host:context.build.host
          ~target with
        | Ok () -> loop rest
        | Error message ->
            Error (ToolchainInstallFailed { target; error = toolchain_install_error message })
      )
  in
  let* () = loop missing in
  emit_toolchains_ensured context targets;
  Ok ()

let validate_target_toolchains = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.init_for_target ~config:context.build.toolchain_config ~target with
        | Ok _ -> loop rest
        | Error message ->
            Error (ToolchainInitializationFailed {
              target;
              error = toolchain_initialization_error message;
            })
      )
  in
  let* () = loop targets in
  emit_toolchains_validated context targets;
  Ok ()

let sort_uniq_strings = fun values ->
  let rec dedupe acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | [ value ] -> List.reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        if String.equal left right then
          dedupe acc rest
        else
          dedupe (left :: acc) rest
  in
  values
  |> List.sort ~compare:String.compare
  |> dedupe []

let referenced_hashes_of_artifact = fun (artifact: Riot_store.Artifact.t) ->
  Std.Crypto.Digest.hex artifact.input_hash
  :: List.map
    artifact.exports
    ~fn:(fun (entry: Riot_store.Manifest.export_entry) -> entry.action_hash)
  |> sort_uniq_strings

let generation_lane_of_results = fun ~profile ~target results ->
  let hashes =
    List.flat_map
      results
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
  List.filter_map
    results
    ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Built artifact ->
          Some Riot_store.Cache_gc.{
            profile;
            target;
            hash = Std.Crypto.Digest.hex artifact.Riot_store.Artifact.input_hash;
          }
      | Package_builder.Cached _
      | Package_builder.Skipped _
      | Package_builder.Failed _ -> None)

let generation_lane_of_result = fun context (lane_result: Lane_result.t) ->
  generation_lane_of_results
    ~profile:context.build.profile.name
    ~target:(Lane_result.target lane_result)
    (Lane_result.results lane_result)

let new_entries_of_lane_result = fun context (lane_result: Lane_result.t) ->
  new_entries_of_results
    ~profile:context.build.profile.name
    ~target:(Lane_result.target lane_result)
    (Lane_result.results lane_result)

let record_successful_build_cache_generation = fun context lane_results ->
  let lanes = List.map lane_results ~fn:(generation_lane_of_result context) in
  let new_entries =
    List.map lane_results ~fn:(new_entries_of_lane_result context)
    |> List.concat
  in
  match Riot_store.Cache_gc.record_successful_build
    ~workspace:context.build.workspace
    ~lanes
    ~new_entries with
  | Ok _ -> Ok ()
  | Error _ -> Ok ()

let new_entry_count_of_lane_results = fun context lane_results ->
  List.map lane_results ~fn:(new_entries_of_lane_result context)
  |> List.concat
  |> List.length

let record_cache_generation_if_needed = fun context lane_results had_partial_failure ->
  let new_entry_count = new_entry_count_of_lane_results context lane_results in
  if context.record_cache_generation && not had_partial_failure then (
    let lane_count = List.length lane_results in
    let emit_cache_generation_events = new_entry_count > 0 in
    if emit_cache_generation_events then
      emit_cache_generation_recording_started context ~lane_count ~new_entry_count;
    let* () = record_successful_build_cache_generation context lane_results in
    if emit_cache_generation_events then
      emit_cache_generation_recorded context ~lane_count ~new_entry_count;
    Ok ()
  ) else
    Ok ()

let failed_results = fun results ->
  List.filter
    results
    ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Failed _ -> true
      | Package_builder.Built _
      | Package_builder.Cached _
      | Package_builder.Skipped _ -> false)

let map_lane_error = fun error -> UnexpectedError { reason = error.Build_work.reason }

let map_prepare_error = fun __tmp1 ->
  match __tmp1 with
  | Build_lane.PlanningFailed error -> PlanningFailed error
  | Build_lane.Failure reason -> UnexpectedError { reason }

let run_lanes = fun context ~toolchain ->
  let* lanes =
    Build_work.prepare_lanes context.build context.resolved ~toolchain
    |> Result.map_err ~fn:map_prepare_error
  in
  List.for_each lanes ~fn:(fun lane -> emit_target_build_started context (Build_lane.target lane));
  let results = Build_work.run context.build lanes in
  let summary = Build_work.summarize results in
  let all_errors = List.map summary.errors ~fn:map_lane_error in
  let lane_results = summary.lane_results in
  let () =
    List.for_each
      summary.completions
      ~fn:(fun (completion: Build_work.completion) ->
        emit_target_build_finished
          context
          ~target:completion.target
          ~result_count:completion.result_count
          ~had_partial_failure:completion.had_partial_failure)
  in
  if List.length all_errors > 0 then
    match List.head all_errors with
    | Some err -> Error err
    | None -> Ok (lane_results, summary.had_failure)
  else if summary.had_failure && not context.allow_partial_failures then
    let failures =
      lane_results
      |> List.map ~fn:(fun lane_result -> failed_results (Lane_result.results lane_result))
      |> List.concat
    in
    Error (BuildFailed { errors = failures })
  else
    Ok (lane_results, summary.had_failure)

let do_build = fun context ->
  let targets = Riot_model.Target.Set.to_list (Resolved_build.targets context.resolved) in
  emit_targets_resolved context targets;
  let* () = ensure_toolchains_for_targets context (Resolved_build.targets context.resolved) in
  let* () = validate_target_toolchains context (Resolved_build.targets context.resolved) in
  emit_runtime_starting context;
  let* toolchain =
    Riot_toolchain.init ~config:context.build.toolchain_config
    |> Result.map_err
      ~fn:(fun message ->
        ToolchainInitializationFailed {
          target = context.build.host;
          error = toolchain_initialization_error message;
        })
  in
  emit_runtime_started context;
  let* (lane_results, had_partial_failure) = run_lanes context ~toolchain in
  let all_results =
    List.map ~fn:Lane_result.results lane_results
    |> List.concat
  in
  let* () = record_cache_generation_if_needed context lane_results had_partial_failure in
  emit_returning_results context ~result_count:(List.length all_results) ~had_partial_failure;
  Ok all_results

let execute = fun ?(allow_partial_failures = false) ?(record_cache_generation = true) build spec ->
  let* context = make_context ~allow_partial_failures ~record_cache_generation build spec in
  with_telemetry_bridge context (fun () -> do_build context)
