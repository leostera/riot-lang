open Global
open Sync

module Node_id : sig
  type t

  val next : unit -> t
  val eq : t -> t -> bool
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = int

  let counter = cell 0

  let next () =
    Cell.incr counter;
    !counter

  let eq = ( = )
  let to_int id = id
  let to_string id = string_of_int id
end

type 'value node = {
  id : Node_id.t;
  mutable deps : Node_id.t list;
  mutable value : 'value;
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
  let initial_nodes =
    Hashtbl.fold
      (fun id count acc -> if count = 0 then id :: acc else acc)
      in_degree []
    |> List.sort (fun a b -> Int.compare (Node_id.to_int a) (Node_id.to_int b))
  in
  List.iter (fun id -> Queue.add id queue) initial_nodes;

  (* Process queue *)
  let sorted = cell [] in
  let processed = cell 0 in

  while not (Queue.is_empty queue) do
    let id = Queue.take queue in
    let node = Hashtbl.find graph.nodes id in
    sorted := node :: !sorted;
    Cell.incr processed;

    (* Decrease in-degree of nodes that depend on this one *)
    let rev_deps =
      Hashtbl.find reverse_deps id
      |> List.sort (fun a b ->
          Int.compare (Node_id.to_int a) (Node_id.to_int b))
    in
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
    (* Find actual cycle using DFS from a node involved in the cycle *)
    let find_cycle () =
      (* Find any node with in-degree > 0 (it's in a cycle) *)
      let start_node =
        Hashtbl.fold
          (fun id count acc ->
            match acc with Some _ -> acc | None -> if count > 0 then Some id else None)
          in_degree None
      in
      match start_node with
      | None -> []  (* No cycle found, shouldn't happen *)
      | Some start_id ->
          (* DFS to find cycle path *)
          let visited = Hashtbl.create (Hashtbl.length graph.nodes) in
          let rec_stack = Hashtbl.create (Hashtbl.length graph.nodes) in
          
          let rec dfs node_id path =
            if Hashtbl.mem rec_stack node_id then
              (* Found back edge to node_id. 
                 Path is [newest, ..., node_id] (reversed order)
                 We want cycle: [node_id, ..., newest, node_id] *)
              let rec extract_cycle acc = function
                | [] -> acc
                | id :: rest ->
                    if Node_id.eq id node_id then 
                      (* Found start, return [node_id] ++ acc ++ [node_id] *)
                      node_id :: (List.rev acc) @ [ node_id ]
                    else extract_cycle (id :: acc) rest
              in
              Some (extract_cycle [] path)
            else if Hashtbl.mem visited node_id then None
            else (
              Hashtbl.add visited node_id ();
              Hashtbl.add rec_stack node_id ();
              let node = Hashtbl.find graph.nodes node_id in
              let result =
                List.fold_left
                  (fun acc dep_id ->
                    match acc with
                    | Some _ -> acc
                    | None -> dfs dep_id (node_id :: path))
                  None node.deps
              in
              Hashtbl.remove rec_stack node_id;
              result)
          in
          
          match dfs start_id [] with
          | Some cycle -> cycle
          | None -> [ start_id ]  (* Fallback *)
    in
    Error (find_cycle ()))
  else Ok (List.rev !sorted)

let iter graph ~fn = Hashtbl.iter fn graph.nodes
let map graph ~fn = Hashtbl.to_seq graph.nodes |> List.of_seq |> List.map fn

let reachable_from graph start_nodes =
  let visited = Hashtbl.create (Hashtbl.length graph.nodes) in

  let rec visit node_id =
    if not (Hashtbl.mem visited node_id) then (
      Hashtbl.add visited node_id ();
      let node = Hashtbl.find graph.nodes node_id in
      List.iter visit node.deps)
  in

  List.iter (fun node -> visit node.id) start_nodes;

  Hashtbl.fold (fun node_id () acc -> node_id :: acc) visited []
