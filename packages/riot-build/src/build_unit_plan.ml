open Std
open Std.Result.Syntax

type error =
  | MissingPackages of {
      missing: Riot_planner.Build_unit_graph.missing_package list;
    }
  | CycleDetected of {
      cycle: Riot_planner.Build_unit.key list;
    }

type t = {
  request: Riot_planner.Build_unit_graph.request;
  graph: Riot_planner.Build_unit_graph.t;
  units: Riot_planner.Build_unit.t list;
}

let trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_PLANNER_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace = fun message ->
  if trace_enabled () then
    eprintln ("riot-build build-unit-plan " ^ message)

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
    synthetic_tools = synthetic_tools @ Resolved_build.synthetic_tools resolved;
  }

let create_graph = fun ?synthetic_tools context resolved ->
  let request = request_of_resolved ?synthetic_tools context resolved in
  Riot_planner.Build_unit_graph.create context.Build_context.workspace request
  |> Result.map_err
    ~fn:(fun (Riot_planner.Build_unit_graph.MissingPackages { missing }) ->
      MissingPackages { missing })

let create = fun ?synthetic_tools context resolved ->
  let request = request_of_resolved ?synthetic_tools context resolved in
  let graph_started_at = Time.Instant.now () in
  let* graph =
    Riot_planner.Build_unit_graph.create context.Build_context.workspace request
    |> Result.map_err
      ~fn:(fun (Riot_planner.Build_unit_graph.MissingPackages { missing }) ->
        MissingPackages { missing })
  in
  let graph_duration =
    Time.Instant.duration_since ~earlier:graph_started_at (Time.Instant.now ())
  in
  trace
    ("graph_create_us="
    ^ Int.to_string (Time.Duration.to_micros graph_duration)
    ^ " graph_size="
    ^ Int.to_string (Riot_planner.Build_unit_graph.size graph));
  let sort_started_at = Time.Instant.now () in
  match Riot_planner.Build_unit_graph.topological_sort graph with
  | Ok units ->
      let sort_duration =
        Time.Instant.duration_since ~earlier:sort_started_at (Time.Instant.now ())
      in
      trace
        ("topological_sort_us="
        ^ Int.to_string (Time.Duration.to_micros sort_duration)
        ^ " unit_count="
        ^ Int.to_string (List.length units));
      Ok { request; graph; units }
  | Error cycle -> Error (CycleDetected { cycle })

let request = fun t -> t.request

let graph = fun t -> t.graph

let units = fun t -> t.units
