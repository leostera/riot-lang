open Std
open Tusk_model
module G = Std.Graph.SimpleGraph

val template : Module.t list -> string

val make_node :
  Module.t ->
  Module.t list ->
  Module_node.t G.node list ->
  exists:bool ->
  actual_path:Path.t option ->
  Module_node.t
