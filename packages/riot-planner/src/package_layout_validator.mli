open Std
open Riot_model

module G = Std.Graph.SimpleGraph

val validate:
  direct_dependency_modules:string list ->
  package:Package.t ->
  module_graph:Module_node.t G.t ->
  analyzed_modules:(G.Node_id.t * Module_graph.analyzed_module) list ->
  (unit, Planning_error.t) result
