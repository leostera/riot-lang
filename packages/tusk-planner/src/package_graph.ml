open Std
open Std.Collections
open Tusk_model
module G = Graph.SimpleGraph

exception Cycle_detected of string list

type t = {
  graph : Package.t G.t;
  name_to_node : (string, Package.t G.node) HashMap.t;
}

let create (workspace : Workspace.t) =
  let graph = G.make () in
  let name_to_node = HashMap.create () in

  List.iter
    (fun (pkg : Package.t) ->
      let node = G.add_node graph pkg in
      let _ = HashMap.insert name_to_node pkg.name node in
      ())
    workspace.packages;

  List.iter
    (fun (pkg : Package.t) ->
      match HashMap.get name_to_node pkg.name with
      | None -> ()
      | Some pkg_node ->
          List.iter
            (fun (dep : Package.dependency) ->
              match HashMap.get name_to_node dep.name with
              | Some dep_node -> G.add_edge pkg_node ~depends_on:dep_node
              | None -> ())
            pkg.dependencies)
    workspace.packages;

  { graph; name_to_node }

let size pg = HashMap.len pg.name_to_node
let packages pg = G.map pg.graph ~fn:(fun (_id, node) -> node.value)

let find_package pg name =
  match HashMap.get pg.name_to_node name with
  | Some node -> Some node.value
  | None -> None

let get_dependencies pg (pkg : Package.t) =
  match HashMap.get pg.name_to_node pkg.name with
  | None -> []
  | Some node ->
      List.filter_map
        (fun dep_id ->
          try Some (G.get_node pg.graph dep_id).value with _ -> None)
        node.deps

let filter_for_package pg pkg_name =
  match HashMap.get pg.name_to_node pkg_name with
  | None -> { graph = G.make (); name_to_node = HashMap.create () }
  | Some target_node ->
      let reachable_ids = G.reachable_from pg.graph [ target_node ] in
      let reachable_set = HashSet.create () in
      List.iter
        (fun id ->
          let _ = HashSet.insert reachable_set id in
          ())
        (target_node.id :: reachable_ids);

      let filtered_graph = G.make () in
      let filtered_name_to_node = HashMap.create () in

      G.iter pg.graph ~fn:(fun id node ->
          if HashSet.contains reachable_set id then
            let new_node = G.add_node filtered_graph node.value in
            let _ =
              HashMap.insert filtered_name_to_node node.value.name new_node
            in
            ());

      G.iter pg.graph ~fn:(fun id node ->
          if HashSet.contains reachable_set id then
            match HashMap.get filtered_name_to_node node.value.name with
            | None -> ()
            | Some new_node ->
                List.iter
                  (fun dep_id ->
                    if HashSet.contains reachable_set dep_id then
                      let dep_node = G.get_node pg.graph dep_id in
                      match
                        HashMap.get filtered_name_to_node dep_node.value.name
                      with
                      | Some new_dep_node ->
                          G.add_edge new_node ~depends_on:new_dep_node
                      | None -> ())
                  node.deps);

      { graph = filtered_graph; name_to_node = filtered_name_to_node }

let topological_sort pg =
  try
    let sorted_nodes = G.topo_sort pg.graph in
    List.map (fun (node : Package.t G.node) -> node.value) sorted_nodes
  with G.Cycle node_ids ->
    let names =
      List.filter_map
        (fun id ->
          try
            let node : Package.t G.node = G.get_node pg.graph id in
            Some node.value.name
          with _ -> None)
        node_ids
    in
    raise (Cycle_detected names)
