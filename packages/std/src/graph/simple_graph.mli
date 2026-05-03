open Global

(**
   Dependency graph with topological sorting.

   A simple directed graph implementation focused on dependency tracking and
   topological sorting. Detects cycles.

   ## Examples

   ```ocaml open Std

   let graph = Graph.SimpleGraph.make () in

   (* Add nodes with values *) let task_a = Graph.SimpleGraph.add_node graph
   "Build A" in let task_b = Graph.SimpleGraph.add_node graph "Build B" in let
   task_c = Graph.SimpleGraph.add_node graph "Build C" in

   (* Define dependencies *) Graph.SimpleGraph.add_edge task_c
   ~depends_on:task_b; Graph.SimpleGraph.add_edge task_b ~depends_on:task_a;

   (* Get execution order *) let order = Graph.SimpleGraph.topo_sort graph in
   (* [task_a; task_b; task_c] *)

   (* Iterate over nodes *) Graph.SimpleGraph.iter graph ~fn:(fun id node ->
   Log.info "Node %s: %s" (Graph.SimpleGraph.Node_id.to_string id) node.value )
   ```

   Cycle detection:

   ```ocaml let task_a = Graph.SimpleGraph.add_node graph "A" in let task_b =
   Graph.SimpleGraph.add_node graph "B" in Graph.SimpleGraph.add_edge task_a
   ~depends_on:task_b; Graph.SimpleGraph.add_edge task_b ~depends_on:task_a;

   match Graph.SimpleGraph.topo_sort graph with
   | Ok sorted -> (* process sorted nodes *)
   | Error ids -> Log.error "Cycle detected with nodes: %s"
       (String.concat ", " (List.map Graph.SimpleGraph.Node_id.to_string ids))
   ```

   ## Use Cases

   - Build system dependency resolution
   - Task scheduling with dependencies
   - Module dependency analysis
   - Any DAG (Directed Acyclic Graph) operations
*)
module Node_id: sig
  (** Unique node identifier. *)
  type t

  (** Generate a new unique node ID. *)
  val next: unit -> t

  (** Check node ID equality. *)
  val eq: t -> t -> bool

  (** Convert to integer. *)
  val to_int: t -> int

  (** Convert to string. *)
  val to_string: t -> string
end

(** Graph node with value and dependencies. *)
type 'value node = {
  id: Node_id.t;
  mutable deps: Node_id.t list;
  mutable value: 'value;
}
(** Graph type. *)
type 'value t

(** Create an empty graph. *)
val make: unit -> 'a t

(** Add a node with the given value. *)
val add_node: 'a t -> 'a -> 'a node

(** Retrieve a node by ID. Returns None if not found. *)
val get_node: 'a t -> Node_id.t -> 'a node option

(** Add a dependency edge (from depends on to). *)
val add_edge: 'a node -> depends_on:'b node -> unit

(** Iterate over all nodes. *)
val iter: 'a t -> fn:(Node_id.t -> 'a node -> unit) -> unit

(** Map over all nodes. *)
val map: 'a t -> fn:(Node_id.t * 'a node -> 'b) -> 'b list

(** Topological sort. Returns Ok with sorted nodes, or Error with cycle node IDs if graph has cycles. *)
val topo_sort: 'a t -> ('a node list, Node_id.t list) result

(**
   Get all nodes reachable from a given starting set through their
   dependency edges. Returns a list of node IDs that can be reached.
*)
val reachable_from: 'a t -> 'a node list -> Node_id.t list
