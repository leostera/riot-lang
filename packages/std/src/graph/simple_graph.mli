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

val make : unit -> 'a t
val add_node : 'a t -> 'a -> 'a node
val get_node : 'a t -> Node_id.t -> 'a node
val add_edge : 'a node -> depends_on:'b node -> unit
val iter : (Node_id.t -> 'a node -> unit) -> 'a t -> unit
val topo_sort : 'a t -> 'a node list

exception Cycle of Node_id.t list
