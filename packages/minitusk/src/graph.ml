module Node_id : sig
  type t

  val next : unit -> t
  val eq : t -> t -> bool
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = int

  let counter = ref 0

  let next () =
    incr counter;
    !counter

  let eq = ( = )
  let to_int id = id
  let to_string id = string_of_int id
end

type 'value node = {
  id : Node_id.t;
  mutable deps : Node_id.t list;
  value : 'value;
}

type 'value t = { nodes : (Node_id.t, 'value node) Hashtbl.t }

let make () = { nodes = Hashtbl.create 100 }

let add_node graph value =
  let id = Node_id.next () in
  let node = { id; deps = []; value } in
  Hashtbl.add graph.nodes id node;
  node

let get_node t node_id = Hashtbl.find t.nodes node_id

(** Add a dependency edge between two nodes *)
let add_edge node ~depends_on = node.deps <- depends_on.id :: node.deps

(** Generate DOT format output for visualization *)
let to_dot graph ~name ~node_to_label ~node_to_attrs =
  let buffer = Buffer.create 1024 in

  (* Write header *)
  Buffer.add_string buffer (Printf.sprintf "digraph %s {\n" name);
  Buffer.add_string buffer "  rankdir=TB;\n";
  Buffer.add_string buffer "  node [shape=box];\n\n";

  (* Add nodes *)
  Hashtbl.iter
    (fun _id node ->
      let label = node_to_label node.value in
      let attrs = node_to_attrs node.value in
      let attrs_str =
        if attrs = [] then ""
        else
          let attr_pairs =
            List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v) attrs
          in
          ", " ^ String.concat ", " attr_pairs
      in
      Buffer.add_string buffer
        (Printf.sprintf "  n%s [label=\"%s\"%s];\n"
           (Node_id.to_string node.id)
           label attrs_str))
    graph.nodes;

  Buffer.add_string buffer "\n";

  (* Add edges *)
  Hashtbl.iter
    (fun _id node ->
      List.iter
        (fun dep_id ->
          Buffer.add_string buffer
            (Printf.sprintf "  n%s -> n%s;\n"
               (Node_id.to_string node.id)
               (Node_id.to_string dep_id)))
        node.deps)
    graph.nodes;

  (* Write footer *)
  Buffer.add_string buffer "}\n";

  Buffer.contents buffer

exception Cycle of Node_id.t list
(** Exception raised when a cycle is detected *)

(** Topological sort using Kahn's algorithm *)
let topo_sort graph =
  (* Create in-degree table *)
  let in_degree = Hashtbl.create (Hashtbl.length graph.nodes) in

  (* Create reverse dependency map: for each node, track who depends on it *)
  let reverse_deps = Hashtbl.create (Hashtbl.length graph.nodes) in

  (* Initialize all nodes with in-degree 0 and empty reverse deps *)
  Hashtbl.iter
    (fun id _ ->
      Hashtbl.add in_degree id 0;
      Hashtbl.add reverse_deps id [])
    graph.nodes;

  (* Calculate in-degrees and reverse dependencies *)
  (* If node A depends on node B (A.deps contains B), then:
     - B must come before A in the build order
     - A has an incoming edge FROM B (A's in-degree increases)
     - B has A as a reverse dependency (when B is processed, A's in-degree decreases)
  *)
  Hashtbl.iter
    (fun my_id node ->
      (* For each dependency I have, I have an incoming edge from it *)
      let my_in_degree = List.length node.deps in
      Hashtbl.replace in_degree my_id my_in_degree;

      (* Also track reverse dependencies *)
      List.iter
        (fun dep_id ->
          let current_rev_deps = Hashtbl.find reverse_deps dep_id in
          Hashtbl.replace reverse_deps dep_id (my_id :: current_rev_deps))
        node.deps)
    graph.nodes;

  (* Find nodes with no incoming edges *)
  let queue = Queue.create () in
  Hashtbl.iter (fun id count -> if count = 0 then Queue.add id queue) in_degree;

  (* Process queue *)
  let sorted = ref [] in
  let processed = ref 0 in

  while not (Queue.is_empty queue) do
    let id = Queue.take queue in
    let node = Hashtbl.find graph.nodes id in
    sorted := node :: !sorted;
    incr processed;

    (* Decrease in-degree of nodes that depend on this one *)
    let rev_deps = Hashtbl.find reverse_deps id in
    List.iter
      (fun dependent_id ->
        let count = Hashtbl.find in_degree dependent_id in
        let new_count = count - 1 in
        Hashtbl.replace in_degree dependent_id new_count;
        if new_count = 0 then Queue.add dependent_id queue)
      rev_deps
  done;

  (* Check for cycles *)
  if !processed <> Hashtbl.length graph.nodes then (
    (* Find nodes that are part of cycles (those with in-degree > 0) *)
    let cycle_nodes = ref [] in
    Hashtbl.iter
      (fun id count -> if count > 0 then cycle_nodes := id :: !cycle_nodes)
      in_degree;
    raise (Cycle !cycle_nodes))
  else List.rev !sorted
