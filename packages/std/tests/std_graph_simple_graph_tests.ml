open Std

module SimpleGraph = Graph.SimpleGraph

let contains_id = fun ids needle -> List.any ids ~fn:(fun id -> SimpleGraph.Node_id.eq id needle)

let ids_of_nodes = fun nodes -> List.map nodes ~fn:(fun node -> SimpleGraph.id node)

let test_make_starts_empty = fun _ctx ->
  let graph = SimpleGraph.make () in
  match SimpleGraph.topo_sort graph with
  | Ok [] -> Ok ()
  | Ok _ -> Error "A new graph should topo_sort to the empty list"
  | Error _ -> Error "A new graph should not report a cycle"

let test_add_node_and_get_node_roundtrip = fun _ctx ->
  let graph = SimpleGraph.make () in
  let node = SimpleGraph.add_node graph "alpha" in
  match SimpleGraph.get_node graph (SimpleGraph.id node) with
  | Some found when String.equal (SimpleGraph.value found) "alpha"
  && SimpleGraph.Node_id.eq (SimpleGraph.id found) (SimpleGraph.id node) -> Ok ()
  | Some _ -> Error "get_node returned the wrong node"
  | None -> Error "get_node should return nodes that were added"

let test_get_node_unknown_id_returns_none = fun _ctx ->
  let graph = SimpleGraph.make () in
  let unknown = SimpleGraph.Node_id.next () in
  match SimpleGraph.get_node graph unknown with
  | None -> Ok ()
  | Some _ -> Error "get_node should return None for unknown ids"

let test_add_edge_records_dependency = fun _ctx ->
  let graph = SimpleGraph.make () in
  let a = SimpleGraph.add_node graph "A" in
  let b = SimpleGraph.add_node graph "B" in
  SimpleGraph.add_edge a ~depends_on:b;
  if
    List.any (SimpleGraph.deps a) ~fn:(fun id -> SimpleGraph.Node_id.eq id (SimpleGraph.id b))
  then
    Ok ()
  else
    Error "add_edge should record the dependency id on the dependent node"

let test_add_edge_is_idempotent = fun _ctx ->
  let graph = SimpleGraph.make () in
  let a = SimpleGraph.add_node graph "A" in
  let b = SimpleGraph.add_node graph "B" in
  SimpleGraph.add_edge a ~depends_on:b;
  SimpleGraph.add_edge a ~depends_on:b;
  if Int.equal (List.length (SimpleGraph.deps a)) 1 then
    Ok ()
  else
    Error "add_edge should not record duplicate dependency ids"

let test_topo_sort_single_node = fun _ctx ->
  let graph = SimpleGraph.make () in
  let node = SimpleGraph.add_node graph "only" in
  match SimpleGraph.topo_sort graph with
  | Ok [ found ] when SimpleGraph.Node_id.eq (SimpleGraph.id found) (SimpleGraph.id node) -> Ok ()
  | Ok _ -> Error "topo_sort should return the single node"
  | Error _ -> Error "topo_sort should not fail on an acyclic graph"

let test_topo_sort_simple_chain_respects_dependencies = fun _ctx ->
  let graph = SimpleGraph.make () in
  let a = SimpleGraph.add_node graph "A" in
  let b = SimpleGraph.add_node graph "B" in
  let c = SimpleGraph.add_node graph "C" in
  SimpleGraph.add_edge b ~depends_on:a;
  SimpleGraph.add_edge c ~depends_on:b;
  match SimpleGraph.topo_sort graph with
  | Ok sorted ->
      let ids = ids_of_nodes sorted in
      if ids = [ SimpleGraph.id a; SimpleGraph.id b; SimpleGraph.id c ] then
        Ok ()
      else
        Error "topo_sort should return dependency order for a chain"
  | Error _ -> Error "topo_sort should succeed for a simple chain"

let test_topo_sort_diamond_places_dependencies_first = fun _ctx ->
  let graph = SimpleGraph.make () in
  let root = SimpleGraph.add_node graph "root" in
  let left = SimpleGraph.add_node graph "left" in
  let right = SimpleGraph.add_node graph "right" in
  let top = SimpleGraph.add_node graph "top" in
  SimpleGraph.add_edge left ~depends_on:root;
  SimpleGraph.add_edge right ~depends_on:root;
  SimpleGraph.add_edge top ~depends_on:left;
  SimpleGraph.add_edge top ~depends_on:right;
  match SimpleGraph.topo_sort graph with
  | Ok sorted ->
      let positions = List.enumerate (ids_of_nodes sorted) in
      let find_pos target =
        List.find positions ~fn:(fun (_, id) -> SimpleGraph.Node_id.eq id target)
        |> Option.map ~fn:(fun (index, _) -> index)
      in
      (
        match (
          find_pos (SimpleGraph.id root),
          find_pos (SimpleGraph.id left),
          find_pos (SimpleGraph.id right),
          find_pos (SimpleGraph.id top)
        ) with
        | (Some root_pos, Some left_pos, Some right_pos, Some top_pos) when root_pos < left_pos
        && root_pos < right_pos
        && left_pos < top_pos
        && right_pos < top_pos -> Ok ()
        | _ -> Error "topo_sort should place dependencies before dependents in a diamond graph"
      )
  | Error _ -> Error "topo_sort should succeed for a diamond graph"

let test_topo_sort_detects_self_cycle = fun _ctx ->
  let graph = SimpleGraph.make () in
  let node = SimpleGraph.add_node graph "self" in
  SimpleGraph.add_edge node ~depends_on:node;
  match SimpleGraph.topo_sort graph with
  | Error ids when contains_id ids (SimpleGraph.id node) -> Ok ()
  | Error _ -> Error "cycle detection should mention the self-cycling node"
  | Ok _ -> Error "topo_sort should fail on a self-cycle"

let test_topo_sort_detects_two_node_cycle = fun _ctx ->
  let graph = SimpleGraph.make () in
  let a = SimpleGraph.add_node graph "A" in
  let b = SimpleGraph.add_node graph "B" in
  SimpleGraph.add_edge a ~depends_on:b;
  SimpleGraph.add_edge b ~depends_on:a;
  match SimpleGraph.topo_sort graph with
  | Error ids when contains_id ids (SimpleGraph.id a) && contains_id ids (SimpleGraph.id b) -> Ok ()
  | Error _ -> Error "cycle detection should mention both nodes in the cycle"
  | Ok _ -> Error "topo_sort should fail on a two-node cycle"

let test_iter_visits_every_node_once = fun _ctx ->
  let graph = SimpleGraph.make () in
  let a = SimpleGraph.add_node graph "A" in
  let b = SimpleGraph.add_node graph "B" in
  let seen = Sync.Atomic.make [] in
  SimpleGraph.iter graph ~fn:(fun id _ -> Sync.Atomic.set seen (id :: Sync.Atomic.get seen));
  let ids = Sync.Atomic.get seen in
  if
    List.length ids = 2 && contains_id ids (SimpleGraph.id a) && contains_id ids (SimpleGraph.id b)
  then
    Ok ()
  else
    Error "iter should visit every node exactly once"

let test_map_returns_one_item_per_node = fun _ctx ->
  let graph = SimpleGraph.make () in
  let _ = SimpleGraph.add_node graph "alpha" in
  let _ = SimpleGraph.add_node graph "beta" in
  let values = SimpleGraph.map graph ~fn:(fun (_id, node) -> SimpleGraph.value node) in
  if
    List.length values = 2
    && List.contains values ~value:"alpha"
    && List.contains values ~value:"beta"
  then
    Ok ()
  else
    Error "map should return one mapped item per node"

let test_reachable_from_includes_start_nodes_and_dependencies = fun _ctx ->
  let graph = SimpleGraph.make () in
  let root = SimpleGraph.add_node graph "root" in
  let middle = SimpleGraph.add_node graph "middle" in
  let top = SimpleGraph.add_node graph "top" in
  SimpleGraph.add_edge middle ~depends_on:root;
  SimpleGraph.add_edge top ~depends_on:middle;
  let reachable = SimpleGraph.reachable_from graph [ top ] in
  if
    contains_id reachable (SimpleGraph.id root)
    && contains_id reachable (SimpleGraph.id middle)
    && contains_id reachable (SimpleGraph.id top)
  then
    Ok ()
  else
    Error "reachable_from should include the start node and all dependencies"

let test_reachable_from_merges_multiple_start_nodes_without_duplicates = fun _ctx ->
  let graph = SimpleGraph.make () in
  let shared = SimpleGraph.add_node graph "shared" in
  let left = SimpleGraph.add_node graph "left" in
  let right = SimpleGraph.add_node graph "right" in
  SimpleGraph.add_edge left ~depends_on:shared;
  SimpleGraph.add_edge right ~depends_on:shared;
  let reachable = SimpleGraph.reachable_from graph [ left; right; left ] in
  let shared_count =
    List.fold_left
      reachable
      ~init:0
      ~fn:(fun count id ->
        if SimpleGraph.Node_id.eq id (SimpleGraph.id shared) then
          count + 1
        else
          count)
  in
  if
    contains_id reachable (SimpleGraph.id left)
    && contains_id reachable (SimpleGraph.id right)
    && Int.equal shared_count 1
  then
    Ok ()
  else
    Error "reachable_from should merge duplicate starting nodes without duplicate reachable ids"

let test_node_id_accessors_are_consistent = fun _ctx ->
  let id = SimpleGraph.Node_id.next () in
  if not (SimpleGraph.Node_id.eq id id) then
    Error "Node_id.eq should report reflexive equality"
  else if
    not
      (String.equal
        (SimpleGraph.Node_id.to_string id)
        (Int.to_string (SimpleGraph.Node_id.to_int id)))
  then
    Error "Node_id.to_string should agree with Node_id.to_int"
  else
    Ok ()

let tests =
  Test.[
    case "make starts empty" test_make_starts_empty;
    case "add_node and get_node roundtrip" test_add_node_and_get_node_roundtrip;
    case "get_node returns None for unknown ids" test_get_node_unknown_id_returns_none;
    case "add_edge records dependency ids" test_add_edge_records_dependency;
    case "add_edge is idempotent" test_add_edge_is_idempotent;
    case "topo_sort returns the single node" test_topo_sort_single_node;
    case
      "topo_sort respects simple chain dependencies"
      test_topo_sort_simple_chain_respects_dependencies;
    case
      "topo_sort places diamond dependencies first"
      test_topo_sort_diamond_places_dependencies_first;
    case "topo_sort detects self-cycles" test_topo_sort_detects_self_cycle;
    case "topo_sort detects two-node cycles" test_topo_sort_detects_two_node_cycle;
    case "iter visits every node once" test_iter_visits_every_node_once;
    case "map returns one item per node" test_map_returns_one_item_per_node;
    case
      "reachable_from includes start nodes and dependencies"
      test_reachable_from_includes_start_nodes_and_dependencies;
    case
      "reachable_from merges multiple starts without duplicates"
      test_reachable_from_merges_multiple_start_nodes_without_duplicates;
    case "Node_id accessors are internally consistent" test_node_id_accessors_are_consistent;
  ]

let main ~args = Test.Cli.main ~name:"graph_simple_graph" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
