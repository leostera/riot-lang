open Global
open Collections
open Sync

module Node_id: sig
  type t

  val next: unit -> t

  val eq: t -> t -> bool

  val to_int: t -> int

  val to_string: t -> string
end = struct
  type t = int

  let counter = Atomic.make 0

  let next = fun () ->
    Atomic.fetch_and_add counter 1 + 1

  let eq = ( = )

  let to_int = fun id -> id

  let to_string = Int.to_string
end

type 'value node = {
  id: Node_id.t;
  deps: Node_id.t IndexSet.t;
  mutable value: 'value;
}

type 'value t = {
  nodes: (Node_id.t, 'value node) HashMap.t;
}

let make = fun () -> { nodes = HashMap.with_capacity ~size:100 }

let add_node = fun graph value ->
  let id = Node_id.next () in
  let node = { id; deps = IndexSet.with_capacity ~size:4; value } in
  let _ = HashMap.insert graph.nodes ~key:id ~value:node in
  node

let id = fun node -> node.id

let value = fun node -> node.value

let set_value = fun node value -> node.value <- value

let deps = fun node -> IndexSet.to_list node.deps

let get_node = fun t node_id -> HashMap.get t.nodes ~key:node_id

(** Add a dependency edge between two nodes *)
let add_edge = fun node ~depends_on ->
  let _ = IndexSet.insert node.deps ~value:depends_on.id in
  ()

(** Topological sort using Kahn's algorithm *)
let topo_sort = fun graph ->
  (* Create in-degree table *)
  let in_degree = HashMap.with_capacity ~size:(HashMap.length graph.nodes) in
  (* Create reverse dependency map: for each node, track who depends on it *)
  let reverse_deps = HashMap.with_capacity ~size:(HashMap.length graph.nodes) in
  (* Initialize all nodes with in-degree 0 and empty reverse deps *)
  HashMap.for_each
    graph.nodes
    ~fn:(fun id _ ->
      let _ = HashMap.insert in_degree ~key:id ~value:0 in
      let _ = HashMap.insert reverse_deps ~key:id ~value:[] in
      ());
  (* Calculate in-degrees and reverse dependencies *)
  (* If node A depends on node B (A.deps contains B), then:
     - B must come before A in the build order
     - A has an incoming edge FROM B (A's in-degree increases)
     - B has A as a reverse dependency (when B is processed, A's in-degree decreases)
  *)
  HashMap.for_each
    graph.nodes
    ~fn:(fun my_id node ->
      (* For each dependency I have, I have an incoming edge from it *)
      let my_in_degree = IndexSet.length node.deps in
      let _ = HashMap.insert in_degree ~key:my_id ~value:my_in_degree in
      (* Also track reverse dependencies *)
      IndexSet.for_each
        node.deps
        ~fn:(fun dep_id ->
          let current_rev_deps =
            HashMap.get reverse_deps ~key:dep_id
            |> Option.unwrap_or ~default:[]
          in
          let _ = HashMap.insert reverse_deps ~key:dep_id ~value:(my_id :: current_rev_deps) in
          ()));
  (* Find nodes with no incoming edges *)
  let queue = Queue.create () in
  let initial_nodes =
    HashMap.fold_left
      in_degree
      ~init:[]
      ~fn:(fun acc id count ->
        if count = 0 then
          id :: acc
        else
          acc)
    |> List.sort ~compare:(fun a b -> Int.compare (Node_id.to_int a) (Node_id.to_int b))
  in
  List.for_each initial_nodes ~fn:(fun id -> Queue.push queue ~value:id);
  (* Process queue *)
  let sorted = cell [] in
  let processed = cell 0 in
  while not (Queue.is_empty queue) do
    let id =
      Queue.pop queue
      |> Option.unwrap
    in
    let node =
      HashMap.get graph.nodes ~key:id
      |> Option.unwrap
    in
    sorted := node :: !sorted;
    Cell.incr processed;
    (* Decrease in-degree of nodes that depend on this one *)
    let rev_deps =
      HashMap.get reverse_deps ~key:id
      |> Option.unwrap_or ~default:[]
      |> List.sort ~compare:(fun a b -> Int.compare (Node_id.to_int a) (Node_id.to_int b))
    in
    List.for_each
      rev_deps
      ~fn:(fun dependent_id ->
        let count =
          HashMap.get in_degree ~key:dependent_id
          |> Option.unwrap
        in
        let new_count = count - 1 in
        let _ = HashMap.insert in_degree ~key:dependent_id ~value:new_count in
        if new_count = 0 then
          Queue.push queue ~value:dependent_id)
  done;
  (* Check for cycles *)
  if !processed != HashMap.length graph.nodes then (
    (* Find actual cycle using DFS from a node involved in the cycle *)
    let find_cycle () =
      let start_node =
        HashMap.fold_left
          in_degree
          ~init:None
          ~fn:(fun acc id count ->
            match acc with
            | Some _ -> acc
            | None ->
                if count > 0 then
                  Some id
                else
                  None)
      in
      match start_node with
      | None -> []
      | Some start_id ->
          (* DFS to find cycle path *)
          let visited = HashMap.with_capacity ~size:(HashMap.length graph.nodes) in
          let rec_stack = HashMap.with_capacity ~size:(HashMap.length graph.nodes) in
          let rec dfs node_id path =
            if HashMap.has_key rec_stack ~key:node_id then
              let rec extract_cycle = fun acc ->
                fun __tmp1 ->
                  match __tmp1 with
                  | [] -> acc
                  | id :: rest ->
                      if Node_id.eq id node_id then
                        (node_id :: (List.reverse acc)) @ [ node_id ]
                      else
                        extract_cycle (id :: acc) rest
              in
              Some (extract_cycle [] path)
            else if HashMap.has_key visited ~key:node_id then
              None
            else
              (
                let _ = HashMap.insert visited ~key:node_id ~value:() in
                let _ = HashMap.insert rec_stack ~key:node_id ~value:() in
                let node =
                  HashMap.get graph.nodes ~key:node_id
                  |> Option.unwrap
                in
                let result =
                  IndexSet.fold_left
                    node.deps
                    ~init:None
                    ~fn:(fun acc dep_id ->
                      match acc with
                      | Some _ -> acc
                      | None -> dfs dep_id (node_id :: path))
                in
                let _ = HashMap.remove rec_stack ~key:node_id in
                result
              )
          in
          match dfs start_id [] with
          | Some cycle -> cycle
          | None -> [ start_id ]
    in
    Error (find_cycle ())
  ) else
    Ok (List.reverse !sorted)

let iter = fun graph ~fn ->
  HashMap.for_each
    graph.nodes
    ~fn:(fun node_id node_value -> fn node_id node_value)

let map = fun graph ~fn ->
  HashMap.to_list graph.nodes
  |> List.map ~fn

let reachable_from = fun graph start_nodes ->
  let visited = HashMap.with_capacity ~size:(HashMap.length graph.nodes) in
  let rec visit node_id =
    if not (HashMap.has_key visited ~key:node_id) then (
      let _ = HashMap.insert visited ~key:node_id ~value:() in
      let node =
        HashMap.get graph.nodes ~key:node_id
        |> Option.unwrap
      in
      IndexSet.for_each node.deps ~fn:visit
    )
  in
  List.for_each start_nodes ~fn:(fun node -> visit node.id);
  HashMap.fold_left visited ~init:[] ~fn:(fun acc node_id () -> node_id :: acc)
  |> List.reverse
