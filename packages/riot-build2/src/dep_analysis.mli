open Std

module G = Graph.SimpleGraph

type source = {
  node_id: G.Node_id.t;
  display_path: Path.t;
  source_hash: Crypto.hash;
  module_path: string list option;
  modules: string list;
  unresolved: string list;
  resolved_dep_ids: G.Node_id.t list;
}

type t

val from_analyzed_modules:
  (G.Node_id.t * Riot_planner.Module_graph.analyzed_module) list -> t

val sources: t -> source list

val find_source: t -> G.Node_id.t -> source option

val resolved_dependency_ids: t -> G.Node_id.t -> G.Node_id.t list

val compile_dependency_ids:
  t ->
  Riot_planner.Module_node.t G.t ->
  Riot_planner.Module_node.t G.node ->
  G.Node_id.t list
