open Global

(** # SimpleGraph - Dependency graph with topological sorting

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
    - Any DAG (Directed Acyclic Graph) operations *)

module Node_id : sig
  type t
  (** Unique node identifier. *)
  val next : unit -> t

  (** Generate a new unique node ID. *)
  val eq : t -> t -> bool

  (** Check node ID equality. *)
  val to_int : t -> int

  (** Convert to integer. *)
  val to_string : t -> string

  (** Convert to string. *)
end

type 'value node = {
  id : Node_id.t;
  mutable deps : Node_id.t list;
  mutable value : 'value;
}
(** Graph node with value and dependencies. *)
type 'value t
(** Graph type. *)
val make : unit -> 'a t

(** Create an empty graph. *)
val add_node : 'a t -> 'a -> 'a node

(** Add a node with the given value. *)
val get_node : 'a t -> Node_id.t -> 'a node option

(** Retrieve a node by ID. Returns None if not found. *)
val add_edge : 'a node -> depends_on:'b node -> unit

(** Add a dependency edge (from depends on to). *)
val iter : 'a t -> fn:(Node_id.t -> 'a node -> unit) -> unit

(** Iterate over all nodes. *)
val map : 'a t -> fn:(Node_id.t * 'a node -> 'b) -> 'b list

(** Map over all nodes. *)
val topo_sort : 'a t -> ('a node list, Node_id.t list) result

(** Topological sort. Returns Ok with sorted nodes, or Error with cycle node IDs if graph has cycles. *)
val reachable_from : 'a t -> 'a node list -> Node_id.t list

(** Get all nodes reachable from a given starting set through their
    dependency edges. Returns a list of node IDs that can be reached. *)
