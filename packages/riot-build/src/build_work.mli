open Std

type t =
  | BuildLane of Build_lane.locked Build_lane.t

type output =
  | LaneCompleted of Lane_result.t

type error = Build_lane.error

val lane: Build_lane.locked Build_lane.t -> t

val target: t -> Riot_model.Target.t

val execute: t -> (output * t list, error) result
