open Std

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

val initial_plan_packages: Build_lane.locked Build_lane.t -> plan_package list

val plan_package_key: plan_package -> Riot_planner.Build_unit.key

val plan_package_target: plan_package -> Riot_model.Target.t

val prepare_lanes:
  Build_context.t ->
  Resolved_build.t ->
  toolchain:Riot_toolchain.t ->
  (Build_lane.locked Build_lane.t list, Build_lane.error) result

val release_lanes: Build_lane.locked Build_lane.t list -> unit

val run: Build_context.t -> Build_lane.locked Build_lane.t list -> run_result

val summarize: run_result -> summary
