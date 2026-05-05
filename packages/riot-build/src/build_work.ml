open Std
open Std.Result.Syntax

type plan_package = {
  lane: Build_lane.locked Build_lane.t;
  unit_key: Riot_planner.Build_unit.key;
}

type error = Package_scheduler.error = {
  lane: Build_lane.locked Build_lane.t;
  reason: string;
}

type completion = Package_scheduler.completion = {
  target: Riot_model.Target.t;
  result_count: int;
  had_partial_failure: bool;
}

type summary = Package_scheduler.summary = {
  completions: completion list;
  lane_results: Lane_result.t list;
  errors: error list;
  had_failure: bool;
}

type run_result = summary

let runtime_phase_of_package_scheduler_event = fun __tmp1 ->
  match __tmp1 with
  | Package_scheduler.PlanningStarted { lane_count; package_count } ->
      Event.PackagePlanningStarted { lane_count; package_count }
  | Package_scheduler.PackagePlanStarted {
      package;
      build_target;
      source_count;
      started_at;
    } ->
      Event.PackagePlanStarted {
        package;
        build_target;
        source_count;
        started_at;
      }
  | Package_scheduler.PackagePlanSourceStarted {
      package;
      build_target;
      source;
      source_index;
      source_count;
      started_at;
    } ->
      Event.PackagePlanSourceStarted {
        package;
        build_target;
        source;
        source_index;
        source_count;
        started_at;
      }
  | Package_scheduler.PackagePlanFinished {
      package;
      build_target;
      source_count;
      completed_at;
      duration;
    } ->
      Event.PackagePlanFinished {
        package;
        build_target;
        source_count;
        completed_at;
        duration;
      }
  | Package_scheduler.PlanningFinished {
      lane_count;
      package_count;
      deferred_count;
      execution_required_count;
      finalized_count;
      cached_count;
      skipped_count;
      failed_count;
      error_count;
    } ->
      Event.PackagePlanningFinished {
        lane_count;
        package_count;
        deferred_count;
        execution_required_count;
        finalized_count;
        cached_count;
        skipped_count;
        failed_count;
        error_count;
      }
  | Package_scheduler.PackageActionGraphPlanned {
      package;
      build_target;
      action_count;
      planned_at;
    } ->
      Event.PackageActionGraphPlanned {
        package;
        build_target;
        action_count;
        planned_at;
      }
  | Package_scheduler.ExecutionStarted { lane_count; package_count } ->
      Event.PackageExecutionStarted { lane_count; package_count }
  | Package_scheduler.ExecutionFinished {
      lane_count;
      package_count;
      finalized_count;
      built_count;
      failed_count;
      error_count;
    } ->
      Event.PackageExecutionFinished {
        lane_count;
        package_count;
        finalized_count;
        built_count;
        failed_count;
        error_count;
      }

let plan_package = fun lane unit_key -> { lane; unit_key }

let initial_plan_packages = fun lane ->
  Build_lane.build_unit_keys lane
  |> List.map ~fn:(plan_package lane)

let plan_package_key = fun (plan_package: plan_package) -> plan_package.unit_key

let plan_package_target = fun (plan_package: plan_package) -> Build_lane.target plan_package.lane

let prepare_lane = fun context build_plan ~toolchain target ->
  Build_lane.prepare
    context
    build_plan
    ~target
    ~toolchain

let release_lanes = fun lanes -> List.for_each lanes ~fn:Build_lane.release

let prepare_lanes = fun context spec ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list (Resolved_build.targets spec)
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let* build_plan =
    try Build_lane.plan_build_units context spec with
    | exn -> Error (Build_lane.Failure (Exception.to_string exn))
  in
  let results =
    WorkerPool.SimpleWorkerPool.run
      ~concurrency:context.Build_context.parallelism
      ~tasks:targets
      ~fn:(fun target ->
        try prepare_lane context build_plan ~toolchain target with
        | exn -> Error (Build_lane.Failure (Exception.to_string exn)))
      ()
    |> List.map ~fn:(fun (_index, result) -> result)
  in
  let rec collect prepared first_error = fun __tmp1 ->
    match __tmp1 with
    | [] -> (
        match first_error with
        | None -> Ok (List.reverse prepared)
        | Some error ->
            release_lanes prepared;
            error
      )
    | (Ok lane) :: rest -> collect (lane :: prepared) first_error rest
    | (Error _ as error) :: rest ->
        let first_error =
          match first_error with
          | Some existing_error -> Some existing_error
          | None -> Some error
        in
        collect prepared first_error rest
  in
  collect [] None results

let run = fun context lanes ->
  try
    let summary =
      Package_scheduler.run
        ~parallelism:context.Build_context.parallelism
        ~on_event:(fun event ->
          Build_context.emit_phase
            context
            (runtime_phase_of_package_scheduler_event event))
        lanes
    in
    release_lanes lanes;
    summary
  with
  | exn ->
      release_lanes lanes;
      raise exn

let summarize = fun run_result -> run_result
