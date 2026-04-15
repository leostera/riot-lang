open Std
open Std.Result.Syntax

type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildFailed of { errors: Package_builder.build_result list }
  | UnexpectedError of { reason: string }

type build_context = {
  build: Build_context.t;
  resolved: Resolved_build.t;
  allow_partial_failures: bool;
  record_cache_generation: bool;
}

let emit_runtime_phase = fun context phase -> context.build.on_event (Event.Phase phase)

let emit_targets_resolved = fun context targets ->
  emit_runtime_phase context (Event.TargetsResolved { target_count = List.length targets })

let emit_toolchains_ensured = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsEnsured { target_count = List.length targets })

let emit_toolchains_validated = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsValidated { target_count = List.length targets })

let emit_runtime_starting = fun context -> emit_runtime_phase context Event.RuntimeStarting

let emit_runtime_started = fun context -> emit_runtime_phase context Event.RuntimeStarted

let emit_target_build_started = fun context target ->
  let host = Riot_model.Target.equal target context.build.host in
  context.build.on_event (Event.BuildingTarget { target; host });
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
  | BuildFailed { errors } -> (
      let failures = Build_result.failures_of_build_results errors in
      match failures with
      | [] -> "build failed"
      | [ failure ] -> Build_result.failure_message failure
      | _ ->
          "build failed:\n"
          ^ String.concat "\n" (List.map failures ~fn:Build_result.failure_message)
    )
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
    List.filter targets
      ~fn:(fun target ->
        match Riot_toolchain.check_toolchain_status ~version:context.build.toolchain_config.version ~target with
        | Riot_toolchain.NotInstalled _
        | Riot_toolchain.Incomplete _ -> true
        | Riot_toolchain.Installed _ -> false)
  in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match Riot_toolchain.download_and_install_toolchain
          context.build.toolchain_config.version
          ~host:context.build.host
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
        match Riot_toolchain.init_for_target ~config:context.build.toolchain_config ~target with
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
  let lanes =
    List.map
      lane_results
      ~fn:(generation_lane_of_result context)
  in
  let new_entries = List.map
    lane_results
    ~fn:(new_entries_of_lane_result context)
  |> List.concat in
  match Riot_store.Cache_gc.record_successful_build ~workspace:context.build.workspace ~lanes ~new_entries with
  | Ok _ -> Ok ()
  | Error _ -> Ok ()

let new_entry_count_of_lane_results = fun context lane_results ->
  List.map
    lane_results
    ~fn:(new_entries_of_lane_result context)
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
    context.build
    context.resolved
    ~target
    ~toolchain

let map_lane_error = fun error ->
  UnexpectedError { reason = error }

let execute_lane = fun context lane ->
  match Build_lane.execute lane with
  | Ok outcome -> Ok (outcome, [])
  | Error error -> Error (map_lane_error error)

let run_lanes = fun context ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list (Resolved_build.targets context.resolved)
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let release_lanes = fun lanes -> List.for_each lanes ~fn:Build_lane.release in
  let rec prepare_lanes prepared = function
    | [] -> Ok (List.reverse prepared)
    | target :: rest ->
        match prepare_lane context ~toolchain target |> Result.map_err ~fn:map_lane_error with
        | Ok lane -> prepare_lanes (lane :: prepared) rest
        | Error _ as error ->
            release_lanes prepared;
            error
  in
  let* lanes = prepare_lanes [] targets in
  List.for_each lanes ~fn:(fun lane -> emit_target_build_started context (Build_lane.target lane));
    let results =
      Build_scheduler.run
        ~concurrency:context.build.parallelism
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
      | Ok (lane_outcome: Lane_result.t) -> lane_outcome.had_partial_failure)
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
        | Ok (lane_outcome: Lane_result.t) -> Some lane_outcome
        | Error _ -> None)
  in
  let () =
    List.for_each results ~fn:(fun (lane, outcome) ->
      let had_partial_failure =
        match outcome with
        | Ok (lane_outcome: Lane_result.t) -> lane_outcome.had_partial_failure
        | Error _ -> true
      in
      let result_count =
        match outcome with
        | Ok (lane_outcome: Lane_result.t) -> List.length lane_outcome.results
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
        |> List.map ~fn:(fun lane_result -> failed_results (Lane_result.results lane_result))
        |> List.concat
      in
      Error (BuildFailed { errors = failures })
    else
      Ok (lane_results, has_failures)

let do_build = fun context ->
  let targets = Riot_model.Target.Set.to_list (Resolved_build.targets context.resolved) in
  emit_targets_resolved context targets;
  let* () = ensure_toolchains_for_targets context (Resolved_build.targets context.resolved) in
  let* () = validate_target_toolchains context (Resolved_build.targets context.resolved) in
  emit_runtime_starting context;
  let* toolchain = Riot_toolchain.init ~config:context.build.toolchain_config
  |> Result.map_err ~fn:(fun error -> ToolchainInitializationFailed { target = context.build.host; error }) in
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
  do_build context
