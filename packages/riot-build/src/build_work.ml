open Std
open Std.Result.Syntax

type t =
  | BuildLane of Build_lane.locked Build_lane.t

type output =
  | LaneCompleted of Lane_result.t

type error = Build_lane.error

let lane = fun lane -> BuildLane lane

let target = function
  | BuildLane lane -> Build_lane.target lane

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
