(** Graph-related utilities and formats *)

module Dot = Dot
module Mermaid = Mermaid

(** Generic graph structure with topological sorting *)
module Node_id : sig
  type t

  val next : unit -> t
  val eq : t -> t -> bool
  val to_int : t -> int
  val to_string : t -> string
end

type 'value node = {
  id : Node_id.t;
  mutable deps : Node_id.t list;
  value : 'value;
}

type 'value t

val make : unit -> 'value t
val add_node : 'value t -> 'value -> 'value node
val get_node : 'value t -> Node_id.t -> 'value node
val add_edge : 'value node -> depends_on:'value node -> unit
val iter : (Node_id.t -> 'value node -> unit) -> 'value t -> unit
val to_dot :
  'value t ->
  name:string ->
  node_to_label:('value -> string) ->
  node_to_attrs:('value -> (string * string) list) ->
  string

exception Cycle of Node_id.t list

val topo_sort : 'value t -> 'value node list
