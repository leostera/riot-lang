open Std
open Std.Collections
open Std.Iter
open Riot_model
open Riot_store
module G = Graph.SimpleGraph

exception Cycle_detected of string list

type missing_dependency = {
  package: string;
  dependency: string;
}

type create_error =
  | MissingPackages of { missing: missing_dependency list }

type build_status =
  Cached
  | Fresh

type build_scope =
  Build
  | Runtime
  | Dev

type package_scope = build_scope

type package_node =
  | Unplanned of { package: Package.t; scope: package_scope }
  | Planned of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash
    }
  | Cached of {
      package: Package.t;
      scope: package_scope;
      hash: Std.Crypto.hash;
      artifact: Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list
    }
  | Built of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      artifact: Artifact.t;
      status: build_status;
      depset: Dependency.t list
    }
  | Failed of { package: Package.t; scope: package_scope; hash: Std.Crypto.hash; error: string }
  | Skipped of { package: Package.t; scope: package_scope; reason: string }

type t = {
  graph: package_node G.t;
  name_to_node: (Package.key, package_node G.node) HashMap.t;
}

let get_package = function
  | Unplanned { package; _ } -> package
  | Planned { package; _ } -> package
  | Cached { package; _ } -> package
  | Built { package; _ } -> package
  | Failed { package; _ } -> package
  | Skipped { package; _ } -> package

let get_scope = function
  | Unplanned { scope; _ } -> scope
  | Planned { scope; _ } -> scope
  | Cached { scope; _ } -> scope
  | Built { scope; _ } -> scope
  | Failed { scope; _ } -> scope
  | Skipped { scope; _ } -> scope

let package_key = fun ~package_name scope ->
  Package.key_of_string
    (
      package_name ^ ":" ^ match scope with
      | Build -> "build"
      | Runtime -> "runtime"
      | Dev -> "dev"
    )

let get_key = fun node ->
  let package = get_package node in
  package_key ~package_name:package.name (get_scope node)

let is_planned = function
  | Unplanned _ -> false
  | Planned _ -> true
  | Cached _ -> true
  | Built _ -> true
  | Failed _ -> true
  | Skipped _ -> true

let get_hash = function
  | Unplanned _ -> None
  | Planned { hash; _ } -> Some hash
  | Cached { hash; _ } -> Some hash
  | Built { hash; _ } -> Some hash
  | Failed { hash; _ } -> Some hash
  | Skipped _ -> None

let get_planned_data = function
  | Unplanned _ -> None
  | Planned {
    package;
    module_graph;
    action_graph;
    hash;
    _
  } -> Some (package, module_graph, action_graph, hash)
  | Built {
    package;
    module_graph;
    action_graph;
    hash;
    _
  } -> Some (package, module_graph, action_graph, hash)
  | Cached _ -> None
  | Failed _ -> None
  | Skipped _ -> None

let is_well_known_package = fun name -> Riot_model.Package.is_builtin_dependency_name name

let dependencies_for_scope = fun scope (pkg: Package.t) ->
  match scope with
  | Build -> pkg.build_dependencies
  | Runtime -> pkg.dependencies
  | Dev -> pkg.dev_dependencies

let projected_package = fun scope pkg ->
  match scope with
  | Build -> Package.for_scope Package.Build pkg
  | Runtime -> Package.for_scope Package.Normal pkg
  | Dev -> Package.for_scope Package.Dev pkg

let needs_build_scope_node = fun (pkg: Package_manifest.t) -> List.length pkg.build_dependencies > 0

let realize_projected_package = fun (workspace: Workspace.t) scope (pkg: Package_manifest.t) ->
  let intent =
    match scope with
    | Build -> Package.Build
    | Runtime -> Package.Runtime
    | Dev -> Package.Dev
  in
  Workspace.realize_package ~intent pkg
  |> projected_package scope

let create ~scope (workspace: Workspace.t): (t, create_error) result =
  let graph = G.make () in
  let name_to_node = HashMap.create () in
  let missing = vec [] in
  let insert_node package scope =
    let node = G.add_node graph (Unplanned { package; scope }) in
    let _ = HashMap.insert
      name_to_node
      ~key:(package_key ~package_name:package.name scope)
      ~value:node in
    ()
  in
  (
    match scope with
    | Build ->
        List.for_each
          workspace.packages
          ~fn:(fun (pkg: Package_manifest.t) ->
            insert_node (realize_projected_package workspace Build pkg) Build)
    | Runtime
    | Dev ->
        List.for_each
          workspace.packages
          ~fn:(fun (pkg: Package_manifest.t) ->
            if needs_build_scope_node pkg then
              insert_node (realize_projected_package workspace Build pkg) Build)
  );
  (
    match scope with
    | Build -> ()
    | Runtime
    | Dev ->
        List.for_each
          workspace.packages
          ~fn:(fun (pkg: Package_manifest.t) ->
            insert_node (realize_projected_package workspace Runtime pkg) Runtime)
  );
  (
    match scope with
    | Dev ->
        List.for_each
          workspace.packages
          ~fn:(fun (pkg: Package_manifest.t) ->
            insert_node (realize_projected_package workspace Dev pkg) Dev)
    | Build
    | Runtime -> ()
  );
  List.for_each
    workspace.packages
    ~fn:(fun (pkg: Package_manifest.t) ->
      let add_dep_edge ~from_scope dep_name =
        match HashMap.get name_to_node ~key:(package_key ~package_name:pkg.name from_scope) with
        | None -> ()
        | Some from_node -> (
            let dep_scope =
              match from_scope with
              | Build -> Build
              | Runtime -> Runtime
              | Dev -> Runtime
            in
            match HashMap.get name_to_node ~key:(package_key ~package_name:dep_name dep_scope) with
            | Some dep_node -> G.add_edge from_node ~depends_on:dep_node
            | None ->
                if not (is_well_known_package dep_name) then
                  Vector.push missing ~value:{ package = pkg.name; dependency = dep_name }
          )
      in
      (
        match (
          HashMap.get name_to_node ~key:(package_key ~package_name:pkg.name Runtime),
          HashMap.get name_to_node ~key:(package_key ~package_name:pkg.name Build)
        ) with
        | Some runtime_node, Some build_node -> G.add_edge runtime_node ~depends_on:build_node
        | _ -> ()
      );
      (
        match (
          HashMap.get name_to_node ~key:(package_key ~package_name:pkg.name Dev),
          HashMap.get name_to_node ~key:(package_key ~package_name:pkg.name Runtime)
        ) with
        | Some dev_node, Some runtime_node -> G.add_edge dev_node ~depends_on:runtime_node
        | _ -> ()
      );
      (* Build-phase dependencies are for build-tooling surfaces such as fused
         riot-fix providers and future build scripts. Runtime/dev scopes only
         materialize pkg.build nodes for packages that declare
         build_dependencies, avoiding needless duplicate package planning while
         still preserving build-dependency ordering.

         They should not pull build-time libraries into the normal runtime graph
         or we recreate cycles like:

           std.runtime -> std.build -> fixme.runtime -> syn.runtime -> std.runtime

         A dedicated Build graph can still wire these edges when build-only
         tooling is the target. *)
      (
        match scope with
        | Build ->
            List.for_each
              pkg.build_dependencies
              ~fn:(fun (dep: Package.dependency) -> add_dep_edge ~from_scope:Build dep.name)
        | Runtime
        | Dev -> ()
      );
      List.for_each
        pkg.dependencies
        ~fn:(fun (dep: Package.dependency) -> add_dep_edge ~from_scope:Runtime dep.name);
      match scope with
      | Build
      | Runtime -> ()
      | Dev ->
          List.for_each
            pkg.dev_dependencies
            ~fn:(fun (dep: Package.dependency) -> add_dep_edge ~from_scope:Dev dep.name));
  if Vector.length missing > 0 then
    Error (MissingPackages { missing = Vector.iter missing |> Iterator.to_list })
  else
    Ok { graph; name_to_node }

let get_node = fun pg package ->
  HashMap.get pg.name_to_node ~key:(package_key ~package_name:package.Package.name Runtime)

let get_node_by_key = fun pg key ->
  HashMap.get pg.name_to_node ~key

let mark_planned = fun pg package_key ~module_graph ~action_graph ~hash ->
  match HashMap.get pg.name_to_node ~key:package_key with
  | None -> ()
  | Some node ->
      let package = get_package node.value in
      let scope = get_scope node.value in
      node.value <- Planned {
        package;
        scope;
        module_graph;
        action_graph;
        hash;
      }

let size = fun pg -> HashMap.length pg.name_to_node

let packages = fun pg -> G.map pg.graph ~fn:(fun ((_id, node)) -> get_package node.value)

let find_package = fun pg name ->
  match HashMap.get pg.name_to_node ~key:(package_key ~package_name:name Runtime) with
  | Some node -> Some (get_package node.value)
  | None -> None

let get_package_node = fun pg package ->
  match get_node pg package with
  | Some node -> Some node.value
  | None -> None

let target_node_for_package = fun pg pkg_name ->
  let key_for scope = package_key ~package_name:pkg_name scope in
  match HashMap.get pg.name_to_node ~key:(key_for Dev) with
  | Some node -> Some node
  | None -> HashMap.get pg.name_to_node ~key:(key_for Runtime)

let filter_for_packages = fun pg pkg_names ->
  let target_nodes = List.filter_map pkg_names ~fn:(target_node_for_package pg) in
  match target_nodes with
  | [] -> { graph = G.make (); name_to_node = HashMap.create () }
  | _ ->
      let reachable_ids = G.reachable_from pg.graph target_nodes in
      let reachable_set = HashSet.create () in
      List.for_each
        target_nodes
        ~fn:(fun (node: package_node G.node) ->
          let _ = HashSet.insert reachable_set ~value:node.id in
          ())
      ;
      List.for_each
        reachable_ids
        ~fn:(fun id ->
          let _ = HashSet.insert reachable_set ~value:id in
          ())
      ;
      let filtered_graph = G.make () in
      let filtered_name_to_node = HashMap.create () in
      G.iter pg.graph
        ~fn:(fun id node ->
          if HashSet.contains reachable_set ~value:id then
            let new_node = G.add_node filtered_graph node.value in
            let _ = HashMap.insert filtered_name_to_node ~key:(get_key node.value) ~value:new_node in
            ());
      G.iter pg.graph
        ~fn:(fun id node ->
          if HashSet.contains reachable_set ~value:id then
            match HashMap.get filtered_name_to_node ~key:(get_key node.value) with
            | None -> ()
            | Some new_node ->
                List.for_each
                  node.deps
                  ~fn:(fun dep_id ->
                    if HashSet.contains reachable_set ~value:dep_id then
                      match G.get_node pg.graph dep_id with
                      | Some dep_node -> (
                          match HashMap.get filtered_name_to_node ~key:(get_key dep_node.value) with
                          | Some new_dep_node -> G.add_edge new_node ~depends_on:new_dep_node
                          | None -> ()
                        )
                      | None -> ()));
      { graph = filtered_graph; name_to_node = filtered_name_to_node }

let filter_for_package = fun pg pkg_name -> filter_for_packages pg [ pkg_name ]

let get_graph_node = fun pg node_id ->
  G.get_node pg.graph node_id

let get_dependencies_for_node = fun pg (node: package_node G.node) ->
  List.filter_map
    node.deps
    ~fn:(fun dep_id ->
      match G.get_node pg.graph dep_id with
      | Some dep_node -> Some dep_node.value
      | None -> None)

let get_dependencies = fun graph (package: Package.t) ->
  let filtered_graph = filter_for_package graph package.name in
  match HashMap.get filtered_graph.name_to_node ~key:(package_key ~package_name:package.name Runtime) with
  | None -> []
  | Some runtime_node -> get_dependencies_for_node filtered_graph runtime_node

let get_unplanned_dependencies = fun pg (pkg: Package.t) ->
  let deps = get_dependencies pg pkg in
  List.filter_map
    deps
    ~fn:(fun dep ->
      if not (is_planned dep) then
        Some (get_package dep)
      else
        None)

let iter_nodes = fun pg ~fn -> G.iter pg.graph ~fn:(fun _id node -> fn node)

let topological_sort = fun pg ->
  match G.topo_sort pg.graph with
  | Ok sorted_nodes -> List.map sorted_nodes ~fn:(fun (node: package_node G.node) -> node.value)
  | Error node_ids ->
      let names =
        List.filter_map
          node_ids
          ~fn:(fun id ->
            match G.get_node pg.graph id with
            | Some node -> Some (get_package node.value).name
            | None -> None)
      in
      raise (Cycle_detected names)
