open Std

type error = Riot_planner.Build_unit_graph.create_error =
  | MissingPackages of {
      missing: Riot_planner.Build_unit_graph.missing_package list;
    }

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
