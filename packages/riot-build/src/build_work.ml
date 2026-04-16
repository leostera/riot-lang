open Std
open Std.Result.Syntax

type plan_package = {
  lane: Build_lane.locked Build_lane.t;
  package_key: Riot_model.Package.key;
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

let plan_package = fun lane package_key -> { lane; package_key }

let initial_plan_packages = fun lane ->
  Build_lane.package_keys lane
  |> List.map ~fn:(plan_package lane)

let plan_package_key = fun (plan_package: plan_package) -> plan_package.package_key

let plan_package_target = fun (plan_package: plan_package) -> Build_lane.target plan_package.lane

let prepare_lane = fun context spec ~toolchain target ->
  Build_lane.prepare context spec ~target ~toolchain

let prepare_lanes = fun context spec ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list (Resolved_build.targets spec)
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let release_lanes = fun lanes -> List.for_each lanes ~fn:Build_lane.release in
  let rec loop prepared = function
    | [] -> Ok (List.reverse prepared)
    | target :: rest -> (
        match prepare_lane context spec ~toolchain target with
        | Ok lane -> loop (lane :: prepared) rest
        | Error _ as error ->
            release_lanes prepared;
            error
      )
  in
  loop [] targets

let run = fun context lanes ->
  try
    let summary =
      Package_scheduler.run ~parallelism:context.Build_context.parallelism lanes
    in
    List.for_each lanes ~fn:Build_lane.release;
    summary
  with
  | exn ->
      List.for_each lanes ~fn:Build_lane.release;
      raise exn

let summarize = fun run_result -> run_result
