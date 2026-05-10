open Std
open Std.Collections

module G = Graph.SimpleGraph
module Module_node = Riot_planner.Module_node

type source = {
  node_id: G.Node_id.t;
  display_path: Path.t;
  source_hash: Crypto.hash;
  module_path: string list option;
  modules: string list;
  unresolved: string list;
  resolved_dep_ids: G.Node_id.t list;
}

type t = {
  sources: source list;
  by_node_id: (int, source) HashMap.t;
}

let resolution_modules = fun analyzed ->
  match analyzed.Riot_planner.Module_graph.deps with
  | Ok resolution -> Riot_planner.Dep_analyzer.Resolution.modules resolution
  | Error _ -> []

let from_analyzed_modules = fun analyzed_modules ->
  let sources =
    analyzed_modules
    |> List.map
      ~fn:(fun (node_id, analyzed) ->
        {
          node_id;
          display_path = analyzed.Riot_planner.Module_graph.display_path;
          source_hash = analyzed.source_hash;
          module_path = None;
          modules = resolution_modules analyzed;
          unresolved = analyzed.unresolved_deps;
          resolved_dep_ids = analyzed.resolved_dep_ids;
        })
  in
  let by_node_id = HashMap.create () in
  List.for_each
    sources
    ~fn:(fun source ->
      ignore (HashMap.insert by_node_id ~key:(G.Node_id.to_int source.node_id) ~value:source));
  { sources; by_node_id }

let sources = fun t -> t.sources

let find_source = fun t node_id -> HashMap.get t.by_node_id ~key:(G.Node_id.to_int node_id)

let resolved_dependency_ids = fun t node_id ->
  match find_source t node_id with
  | Some source -> source.resolved_dep_ids
  | None -> []

let is_source_node = fun node ->
  match (G.value node).Module_node.kind with
  | Module_node.ML _
  | MLI _ -> true
  | _ -> false

let is_concrete_source_node = fun node ->
  is_source_node node
  && match (G.value node).Module_node.file with
  | Module_node.Concrete _ -> true
  | Generated _ -> false

let is_generated_source_node = fun node ->
  is_source_node node
  && match (G.value node).Module_node.file with
  | Module_node.Generated _ -> true
  | Concrete _ -> false

let same_module_interface_dependency_ids = fun module_graph node ->
  match (G.value node).Module_node.kind with
  | Module_node.ML mod_ ->
      G.map module_graph ~fn:(fun (candidate_id, candidate_node) -> (candidate_id, candidate_node))
      |> List.filter_map
        ~fn:(fun (candidate_id, candidate_node) ->
          match (G.value candidate_node).Module_node.kind with
          | Module_node.MLI candidate_mod when Riot_model.Module.eq candidate_mod mod_ ->
              Some candidate_id
          | _ -> None)
  | _ -> []

let cmi_provider_dependency_id = fun module_graph dep_id ->
  match G.get_node module_graph dep_id with
  | None -> None
  | Some dep_node -> (
      match (G.value dep_node).Module_node.kind with
      | Module_node.MLI _ -> Some dep_id
      | ML dep_mod ->
          G.map module_graph ~fn:(fun (candidate_id, candidate_node) -> (candidate_id, candidate_node))
          |> List.find
            ~fn:(fun (_candidate_id, candidate_node) ->
              match (G.value candidate_node).Module_node.kind with
              | Module_node.MLI candidate_mod when Riot_model.Module.eq candidate_mod dep_mod -> true
              | _ -> false)
          |> Option.map ~fn:(fun (candidate_id, _candidate_node) -> candidate_id)
          |> Option.or_else ~fn:(fun () -> Some dep_id)
      | _ -> None
    )

let unique_node_ids = fun ids ->
  let seen = HashSet.create () in
  List.filter_map
    ids
    ~fn:(fun id ->
      let key = G.Node_id.to_int id in
      if HashSet.insert seen ~value:key then
        Some id
      else
        None)

let source_dependency_ids = fun module_graph ids ->
  ids
  |> List.filter
    ~fn:(fun id ->
      match G.get_node module_graph id with
      | Some node -> is_source_node node
      | None -> false)

let concrete_source_dependency_ids = fun module_graph ids ->
  ids
  |> List.filter
    ~fn:(fun id ->
      match G.get_node module_graph id with
      | Some node -> is_concrete_source_node node || is_generated_source_node node
      | None -> false)

let generated_source_dependency_ids = fun module_graph ids ->
  ids
  |> List.filter
    ~fn:(fun id ->
      match G.get_node module_graph id with
      | Some node -> is_generated_source_node node
      | None -> false)

let compile_dependency_ids = fun t module_graph node ->
  let semantic_ids = resolved_dependency_ids t (G.id node) in
  let generated_ids = generated_source_dependency_ids module_graph (G.deps node) in
  match ((G.value node).Module_node.kind, (G.value node).file) with
  | ((Module_node.MLI _), Module_node.Concrete _) ->
      (
        semantic_ids
        |> List.filter_map ~fn:(cmi_provider_dependency_id module_graph)
      ) @ generated_ids
      |> unique_node_ids
  | ((Module_node.ML _), Module_node.Concrete _) ->
      (
        semantic_ids
        |> List.filter_map ~fn:(cmi_provider_dependency_id module_graph)
      )
      @ generated_ids
      @ same_module_interface_dependency_ids module_graph node
      |> unique_node_ids
  | ((Module_node.ML _ | Module_node.MLI _), Module_node.Generated _) ->
      G.deps node
      |> source_dependency_ids module_graph
      |> unique_node_ids
  | _ ->
      G.deps node
      |> unique_node_ids
