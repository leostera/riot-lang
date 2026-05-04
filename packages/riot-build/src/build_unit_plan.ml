open Std

type error = Riot_planner.Build_unit_graph.create_error =
  | MissingPackages of {
      missing: Riot_planner.Build_unit_graph.missing_package list;
    }

let targets_of_resolved = fun resolved ->
  Resolved_build.targets resolved
  |> Riot_model.Target.Set.to_list
  |> List.sort ~compare:Riot_model.Target.compare

let kind_of_resolved = fun resolved ->
  match Resolved_build.scope resolved with
  | Runtime -> Riot_planner.Build_unit_graph.Runtime
  | Dev -> Riot_planner.Build_unit_graph.Dev (Resolved_build.dev_artifacts resolved)

let request_of_resolved = fun ?(synthetic_tools = []) context resolved ->
  Riot_planner.Build_unit_graph.{
    roots = Some (Resolved_build.package_names resolved);
    targets = targets_of_resolved resolved;
    profile = context.Build_context.profile;
    kind = kind_of_resolved resolved;
    synthetic_tools;
  }

let create_graph = fun ?synthetic_tools context resolved ->
  let request = request_of_resolved ?synthetic_tools context resolved in
  Riot_planner.Build_unit_graph.create context.Build_context.workspace request
