open Global
open Collections
open Sync
open Sync.Cell

module Node_id: sig
  type t
  val next: unit -> t

  val eq: t -> t -> bool

  val to_int: t -> int

  val to_string: t -> string
end = struct
  type t = int

  let counter = Cell.create 0

  let next = fun () ->
    Cell.incr counter;
    !counter

  let eq = ( = )

  let to_int = fun id -> id

  let to_string = fun id -> string_of_int id
end

type 'value node = {
  id: Node_id.t;
  mutable deps: Node_id.t list;
  mutable value: 'value;
}

type 'value t = {
  nodes: (Node_id.t, 'value node) HashMap.t;
}

let make = fun () -> {nodes = HashMap.with_capacity 100}

let add_node = fun graph value ->
  let id = Node_id.next () in
  let node = {id; deps = []; value} in
  let _ = HashMap.insert graph.nodes id node in
  node

let get_node = fun t node_id ->
  HashMap.get t.nodes node_id

(** Add a dependency edge between two nodes *)
let add_edge = fun node ~depends_on -> node.deps <- depends_on.id :: node.deps

(** Topological sort using Kahn's algorithm *)
let topo_sort = fun graph ->
  (* Create in-degree table *)
  let in_degree = HashMap.with_capacity (HashMap.len graph.nodes) in
  (* Create reverse dependency map: for each node, track who depends on it *)
  let reverse_deps = HashMap.with_capacity (HashMap.len graph.nodes) in
  (* Initialize all nodes with in-degree 0 and empty reverse deps *)
  HashMap.iter
    (fun id _ ->
      let _ = HashMap.insert in_degree id 0 in
      let _ = HashMap.insert reverse_deps id [] in
      ())
    graph.nodes;
  (* Calculate in-degrees and reverse dependencies *)
  (* If node A depends on node B (A.deps contains B), then:
     - B must come before A in the build order
     - A has an incoming edge FROM B (A's in-degree increases)
     - B has A as a reverse dependency (when B is processed, A's in-degree decreases)
  *)
  HashMap.iter
    (fun my_id node ->
      (* For each dependency I have, I have an incoming edge from it *)
      let my_in_degree = List.length node.deps in
      let _ = HashMap.insert in_degree my_id my_in_degree in
      (* Also track reverse dependencies *)
      List.iter
        (fun dep_id ->
          let current_rev_deps = HashMap.get reverse_deps dep_id |> Option.unwrap_or ~default:[] in
          let _ = HashMap.insert reverse_deps dep_id (my_id :: current_rev_deps) in
          ())
        node.deps)
    graph.nodes;
  (* Find nodes with no incoming edges *)
  let queue = Queue.create () in
  let initial_nodes =
    HashMap.fold
      (fun id count acc ->
        if count = 0 then
          id :: acc
        else
          acc)
      in_degree
      []
    |> List.sort
      (fun a b ->
        Int.compare (Node_id.to_int a) (Node_id.to_int b))
  in
  List.iter
    (fun id ->
      Queue.push queue id)
    initial_nodes;
  (* Process queue *)
  let sorted = cell [] in
  let processed = cell 0 in
  while not (Queue.is_empty queue) do
    let id = Queue.pop queue |> Option.unwrap in
    let node = HashMap.get graph.nodes id |> Option.unwrap in
    sorted := node :: !sorted;
    Cell.incr processed;
    (* Decrease in-degree of nodes that depend on this one *)
    let rev_deps =
      HashMap.get reverse_deps id
      |> Option.unwrap_or ~default:[]
      |> List.sort
        (fun a b ->
          Int.compare (Node_id.to_int a) (Node_id.to_int b))
    in
    List.iter
      (fun dependent_id ->
        let count = HashMap.get in_degree dependent_id |> Option.unwrap in
        let new_count = count - 1 in
        let _ = HashMap.insert in_degree dependent_id new_count in
        if new_count = 0 then
          Queue.push queue dependent_id)
      rev_deps
  done;
  (* Check for cycles *)
  if !processed != HashMap.len graph.nodes then
    (
      (* Find actual cycle using DFS from a node involved in the cycle *)
      let find_cycle = fun () ->
        let start_node =
          HashMap.fold
            (fun id count acc ->
              match acc with
              | Some _ -> acc
              | None ->
                  if count > 0 then
                    Some id
                  else
                    None)
            in_degree
            None
        in
        match start_node with
        | None -> []
        | Some start_id ->
            (* DFS to find cycle path *)
            let visited = HashMap.with_capacity (HashMap.len graph.nodes) in
            let rec_stack = HashMap.with_capacity (HashMap.len graph.nodes) in
            let rec dfs = fun node_id path ->
              if HashMap.contains_key rec_stack node_id then
                let rec extract_cycle = fun acc ->
                  function
                  | [] -> acc
                  | id :: rest ->
                      if Node_id.eq id node_id then
                        node_id :: (List.rev acc) @ [ node_id ]
                      else
                        extract_cycle (id :: acc) rest
                in
                Some (extract_cycle [] path)
              else if HashMap.contains_key visited node_id then
                None
              else
                (
                  let _ = HashMap.insert visited node_id () in
                  let _ = HashMap.insert rec_stack node_id () in
                  let node = HashMap.get graph.nodes node_id |> Option.unwrap in
                  let result =
                    List.fold_left
                      (fun acc dep_id ->
                        match acc with
                        | Some _ -> acc
                        | None -> dfs dep_id (node_id :: path))
                      None
                      node.deps
                  in
                  let _ = HashMap.remove rec_stack node_id in
                  result
                )
            in
            match dfs start_id [] with
            | Some cycle -> cycle
            | None -> [ start_id ]
      in
      Error (find_cycle ())
    )
  else
    Ok (List.rev !sorted)

let iter = fun graph ~fn ->
  HashMap.iter fn graph.nodes

let map = fun graph ~fn -> HashMap.to_list graph.nodes |> List.map fn

let reachable_from = fun graph start_nodes ->
  let visited = HashMap.with_capacity (HashMap.len graph.nodes) in
  let rec visit = fun node_id ->
    if not (HashMap.contains_key visited node_id) then
      (
        let _ = HashMap.insert visited node_id () in
        let node = HashMap.get graph.nodes node_id |> Option.unwrap in
        List.iter visit node.deps
      )
  in
  List.iter (fun node -> visit node.id) start_nodes;
  HashMap.fold (fun node_id () acc -> node_id :: acc) visited []
