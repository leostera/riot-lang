open Std
open Std.Collections
open Tusk_model
open Tusk_store
module G = Graph.SimpleGraph

exception Cycle_detected of string list

type missing_dependency = {
  package : string;
  dependency : string;
}

type create_error = 
  | MissingPackages of { missing : missing_dependency list }

type build_status = Cached | Fresh

type package_node =
  | Unplanned of Package.t
  | Planned of {
      package : Package.t;
      module_graph : Module_node.t G.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
    }
  | Built of {
      package : Package.t;
      module_graph : Module_node.t G.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      artifact : Artifact.t;
      status : build_status;
      depset : Dependency.t list;
    }
  | Failed of { package : Package.t; hash : Std.Crypto.hash; error : string }
  | Skipped of { package : Package.t; reason : string }

type t = {
  graph : package_node G.t;
  name_to_node : (string, package_node G.node) HashMap.t;
}

let get_package = function
  | Unplanned pkg -> pkg
  | Planned { package; _ } -> package
  | Built { package; _ } -> package
  | Failed { package; _ } -> package
  | Skipped { package; _ } -> package

let is_planned = function
  | Unplanned _ -> false
  | Planned _ -> true
  | Built _ -> true
  | Failed _ -> true
  | Skipped _ -> true

let get_hash = function
  | Unplanned _ -> None
  | Planned { hash; _ } -> Some hash
  | Built { hash; _ } -> Some hash
  | Failed { hash; _ } -> Some hash
  | Skipped _ -> None

let get_planned_data = function
  | Unplanned _ -> None
  | Planned { package; module_graph; action_graph; hash } ->
      Some (package, module_graph, action_graph, hash)
  | Built { package; module_graph; action_graph; hash; _ } ->
      Some (package, module_graph, action_graph, hash)
  | Failed _ -> None

let is_well_known_package name =
  (* OCaml standard library packages that are distributed with OCaml *)
  match name with
  | "unix" | "stdlib" | "threads" | "str" | "bigarray" | "dynlink" 
  | "compiler-libs" | "graphics" -> true
  | _ -> false

let create (workspace : Workspace.t) : (t, create_error) result =
  let graph = G.make () in
  let name_to_node = HashMap.create () in
  let missing = vec[] in

  List.iter
    (fun (pkg : Package.t) ->
      let node = G.add_node graph (Unplanned pkg) in
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
              | None -> 
                  (* Skip well-known OCaml stdlib packages *)
                  if not (is_well_known_package dep.name) then
                    Vector.push missing { package = pkg.name; dependency = dep.name })
            pkg.dependencies)
    workspace.packages;

  if Vector.len missing > 0 then
    Error (MissingPackages { missing = Vector.to_list missing })
  else
    Ok { graph; name_to_node }

let get_node pg package = HashMap.get pg.name_to_node package.Package.name

let mark_planned pg (package : Package.t) ~module_graph ~action_graph ~hash =
  match HashMap.get pg.name_to_node package.name with
  | None -> ()
  | Some node ->
      node.value <- Planned { package; module_graph; action_graph; hash }

let size pg = HashMap.len pg.name_to_node
let packages pg = G.map pg.graph ~fn:(fun (_id, node) -> get_package node.value)

let find_package pg name =
  match HashMap.get pg.name_to_node name with
  | Some node -> Some (get_package node.value)
  | None -> None

let get_package_node pg package =
  match get_node pg package with
  | Some node -> Some node.value
  | None -> None

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
            let pkg = get_package node.value in
            let new_node = G.add_node filtered_graph node.value in
            let _ = HashMap.insert filtered_name_to_node pkg.name new_node in
            ());

      G.iter pg.graph ~fn:(fun id node ->
          if HashSet.contains reachable_set id then
            let pkg = get_package node.value in
            match HashMap.get filtered_name_to_node pkg.name with
            | None -> ()
            | Some new_node ->
                List.iter
                  (fun dep_id ->
                    if HashSet.contains reachable_set dep_id then
                      let dep_node = G.get_node pg.graph dep_id in
                      let dep_pkg = get_package dep_node.value in
                      match HashMap.get filtered_name_to_node dep_pkg.name with
                      | Some new_dep_node ->
                          G.add_edge new_node ~depends_on:new_dep_node
                      | None -> ())
                  node.deps);

      { graph = filtered_graph; name_to_node = filtered_name_to_node }

let topological_sort pg =
  match G.topo_sort pg.graph with
  | Ok sorted_nodes ->
      List.map (fun (node : package_node G.node) -> node.value) sorted_nodes
  | Error node_ids ->
      let names =
        List.filter_map
          (fun id ->
            try
              let node : package_node G.node = G.get_node pg.graph id in
              Some (get_package node.value).name
            with _ -> None)
          node_ids
      in
      raise (Cycle_detected names)

let get_dependencies graph (package : Package.t) =
  let filtered_graph = filter_for_package graph package.name in
  topological_sort filtered_graph
  |> List.filter_map (fun node ->
      let pkg = get_package node in
      if Package.equal pkg package then None else Some node)

let get_unplanned_dependencies pg (pkg : Package.t) =
  let deps = get_dependencies pg pkg in
  List.filter_map
    (fun dep -> if not (is_planned dep) then Some (get_package dep) else None)
    deps

let iter_nodes pg ~fn = G.iter pg.graph ~fn:(fun _id node -> fn node)
