open Std

type error =
  | MissingPackages of {
      missing: Riot_planner.Build_unit_graph.missing_package list;
    }
  | CycleDetected of {
      cycle: Riot_planner.Build_unit.key list;
    }
type t

val request_of_resolved:
  ?synthetic_tools:Riot_planner.Build_unit_graph.synthetic_tool list ->
  Build_context.t ->
  Resolved_build.t ->
  Riot_planner.Build_unit_graph.request

val create_graph:
  ?synthetic_tools:Riot_planner.Build_unit_graph.synthetic_tool list ->
  Build_context.t ->
  Resolved_build.t ->
  (Riot_planner.Build_unit_graph.t, error) result

val create:
  ?synthetic_tools:Riot_planner.Build_unit_graph.synthetic_tool list ->
  Build_context.t ->
  Resolved_build.t ->
  (t, error) result

val request: t -> Riot_planner.Build_unit_graph.request

val graph: t -> Riot_planner.Build_unit_graph.t

val units: t -> Riot_planner.Build_unit.t list
