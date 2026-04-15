open Std

type t =
  | BuildLane of Build_lane.locked Build_lane.t

type output =
  | LaneCompleted of Lane_result.t

type error = Build_lane.error

type run_result = (t * (output, error) result) list

val lane: Build_lane.locked Build_lane.t -> t

val target: t -> Riot_model.Target.t

val release: t -> unit

val prepare_lanes:
  Build_context.t ->
  Resolved_build.t ->
  toolchain:Riot_toolchain.t ->
  (t list, error) result

val lane_result: output -> Lane_result.t option

val had_partial_failure: output -> bool

val result_count: output -> int

val execute: t -> (output * t list, error) result

val run: Build_context.t -> t list -> run_result
