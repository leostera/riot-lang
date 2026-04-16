open Std

type error = {
  lane: Build_lane.locked Build_lane.t;
  reason: string;
}

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

val run: parallelism:int -> Build_lane.locked Build_lane.t list -> summary
