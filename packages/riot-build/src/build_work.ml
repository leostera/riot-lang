open Std
open Std.Result.Syntax

type t =
  | BuildLane of Build_lane.locked Build_lane.t

type output =
  | LaneCompleted of Lane_result.t

type error = Build_lane.error

type run_result = (t * (output, error) result) list

type completion = {
  target: Riot_model.Target.t;
  result_count: int;
  had_partial_failure: bool;
}

type summary = {
  completions: completion list;
  lane_results: Lane_result.t list;
  errors: error list;
  had_failure: bool;
}

let lane = fun lane -> BuildLane lane

let target = function
  | BuildLane lane -> Build_lane.target lane

let release = function
  | BuildLane lane -> Build_lane.release lane

let prepare_lane = fun context spec ~toolchain target ->
  Build_lane.prepare
    context
    spec
    ~target
    ~toolchain
  |> Result.map ~fn:lane

let prepare_lanes = fun context spec ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list (Resolved_build.targets spec)
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let release_items = fun items -> List.for_each items ~fn:release in
  let rec loop prepared = function
    | [] -> Ok (List.reverse prepared)
    | target :: rest -> (
        match prepare_lane context spec ~toolchain target with
        | Ok item -> loop (item :: prepared) rest
        | Error _ as error ->
            release_items prepared;
            error
      )
  in
  loop [] targets

let lane_result = function
  | LaneCompleted result -> Some result

let had_partial_failure = function
  | LaneCompleted result -> Lane_result.had_partial_failure result

let result_count = function
  | LaneCompleted result -> List.length (Lane_result.results result)

let execute = function
  | BuildLane lane ->
      let* result = Build_lane.execute lane in
      Ok (LaneCompleted result, [])

let run = fun context work_items ->
  Build_scheduler.run
    ~concurrency:context.Build_context.parallelism
    ~tasks:work_items
    ~fn:execute

let completion_of_result = fun work outcome ->
  {
    target = target work;
    result_count =
      (match outcome with
      | Ok output -> result_count output
      | Error _ -> 0);
    had_partial_failure =
      (match outcome with
      | Ok output -> had_partial_failure output
      | Error _ -> true);
  }

let summarize = fun results ->
  let completions =
    List.map results
      ~fn:(fun (work, outcome) -> completion_of_result work outcome)
  in
  let lane_results =
    List.filter_map results
      ~fn:(fun (_, outcome) ->
        match outcome with
        | Ok output -> lane_result output
        | Error _ -> None)
  in
  let errors =
    List.filter_map results
      ~fn:(fun (_, outcome) ->
        match outcome with
        | Error err -> Some err
        | Ok _ -> None)
  in
  let had_failure =
    List.exists
      (fun (_, outcome) ->
        match outcome with
        | Error _ -> true
        | Ok output -> had_partial_failure output)
      results
  in
  {
    completions;
    lane_results;
    errors;
    had_failure;
  }
