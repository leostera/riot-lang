open Std
open Std.Collections
open Std.Iter
open Riot_model
open Riot_store

module G = Graph.SimpleGraph

exception Cycle_detected of string list

type missing_dependency = { package: string; dependency: string }

type create_error =
  | MissingPackages of {
      missing: missing_dependency list;
    }

type create_breakdown = {
  build_node_realization_count: int;
  build_node_realization_duration: Time.Duration.t;
  runtime_node_realization_count: int;
  runtime_node_realization_duration: Time.Duration.t;
  dev_node_realization_count: int;
  dev_node_realization_duration: Time.Duration.t;
  edge_wiring_duration: Time.Duration.t;
}

type build_status =
  | Cached
  | Fresh

type build_scope =
  | Build
  | Runtime
  | Dev

type dev_artifacts = Riot_model.Package.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}

type package_scope = build_scope

type realized_node = {
  realized_package: Package.t;
  realized_scope: package_scope;
  realization_duration: Time.Duration.t;
}

type realization_task = package_scope list * Package_manifest.t

type package_node =
  | Unplanned of {
      package: Package.t;
      scope: package_scope;
    }
  | Planned of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
    }
  | Cached of {
      package: Package.t;
      scope: package_scope;
      hash: Std.Crypto.hash;
      artifact: Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
    }
  | Built of {
      package: Package.t;
      scope: package_scope;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      artifact: Artifact.t;
      status: build_status;
      depset: Dependency.t list;
    }
  | Failed of {
      package: Package.t;
      scope: package_scope;
      hash: Std.Crypto.hash;
      error: string;
    }
  | Skipped of {
      package: Package.t;
      scope: package_scope;
      reason: string;
    }

type t = {
  graph: package_node G.t;
  name_to_node: (Package.key, package_node G.node) HashMap.t;
}

let get_package = fun __tmp1 ->
  match __tmp1 with
  | Unplanned { package; _ } -> package
  | Planned { package; _ } -> package
  | Cached { package; _ } -> package
  | Built { package; _ } -> package
  | Failed { package; _ } -> package
  | Skipped { package; _ } -> package

let get_scope = fun __tmp1 ->
  match __tmp1 with
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

let elapsed_us_since = fun started_at ->
  Time.Instant.elapsed started_at
  |> Time.Duration.to_micros

let trace_package_graph = fun message ->
  let _ = message in
  ()

let string_of_build_scope = fun __tmp1 ->
  match __tmp1 with
  | Build -> "build"
  | Runtime -> "runtime"
  | Dev -> "dev"

let get_key = fun node ->
  let package = get_package node in
  package_key ~package_name:(Package_name.to_string package.name) (get_scope node)

let is_planned = fun __tmp1 ->
  match __tmp1 with
  | Unplanned _ -> false
  | Planned _ -> true
  | Cached _ -> true
  | Built _ -> true
  | Failed _ -> true
  | Skipped _ -> true

let get_hash = fun __tmp1 ->
  match __tmp1 with
  | Unplanned _ -> None
  | Planned { hash; _ } -> Some hash
  | Cached { hash; _ } -> Some hash
  | Built { hash; _ } -> Some hash
  | Failed { hash; _ } -> Some hash
  | Skipped _ -> None

let get_planned_data = fun __tmp1 ->
  match __tmp1 with
  | Unplanned _ -> None
  | Planned {
      package;
      module_graph;
      action_graph;
      hash;
      _;
    } ->
      Some (package, module_graph, action_graph, hash)
  | Built {
      package;
      module_graph;
      action_graph;
      hash;
      _;
    } ->
      Some (package, module_graph, action_graph, hash)
  | Cached _ -> None
  | Failed _ -> None
  | Skipped _ -> None

let is_well_known_package = fun name ->
  Riot_model.Package.is_builtin_dependency_name
    (Package_name.to_string name)

let dependencies_for_scope = fun scope (pkg: Package.t) ->
  match scope with
  | Build -> pkg.build_dependencies
  | Runtime -> pkg.dependencies
  | Dev -> pkg.dependencies @ pkg.dev_dependencies

let projected_package = fun
  ?(dev_artifacts = {tests = true; examples = true; benches = true}) scope pkg ->
  match scope with
  | Build -> Package.for_scope Package.Build pkg
  | Runtime -> Package.for_scope Package.Normal pkg
  | Dev -> Package.for_scope ~dev_artifacts Package.Dev pkg

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

let empty_breakdown = {
  build_node_realization_count = 0;
  build_node_realization_duration = Time.Duration.zero;
  runtime_node_realization_count = 0;
  runtime_node_realization_duration = Time.Duration.zero;
  dev_node_realization_count = 0;
  dev_node_realization_duration = Time.Duration.zero;
  edge_wiring_duration = Time.Duration.zero;
}

let add_realized_node_to_breakdown = fun breakdown realized ->
  match realized.realized_scope with
  | Build ->
      {
        breakdown with
        build_node_realization_count = breakdown.build_node_realization_count + 1;
        build_node_realization_duration = Time.Duration.add
          breakdown.build_node_realization_duration
          realized.realization_duration;
      }
  | Runtime ->
      {
        breakdown with
        runtime_node_realization_count = breakdown.runtime_node_realization_count + 1;
        runtime_node_realization_duration = Time.Duration.add
          breakdown.runtime_node_realization_duration
          realized.realization_duration;
      }
  | Dev ->
      {
        breakdown with
        dev_node_realization_count = breakdown.dev_node_realization_count + 1;
        dev_node_realization_duration = Time.Duration.add
          breakdown.dev_node_realization_duration
          realized.realization_duration;
      }

let realized_node = fun ~scope ~package ~realization_duration ->
  {
    realized_package = package;
    realized_scope = scope;
    realization_duration;
  }

let scope_requested = fun scope scopes ->
  List.any scopes ~fn:(fun requested -> requested = scope)

let duration_since = fun started_at ->
  Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ())

let realize_task = fun ~dev_artifacts workspace (scopes, pkg) ->
  let nodes = Vector.with_capacity ~size:(List.length scopes) in
  if scope_requested Build scopes then (
    let realization_started_at = Time.Instant.now () in
    let package = realize_projected_package workspace Build pkg in
    Vector.push
      nodes
      ~value:(realized_node
        ~scope:Build
        ~package
        ~realization_duration:(duration_since realization_started_at))
  );
  if scope_requested Dev scopes then (
    let realization_started_at = Time.Instant.now () in
    let package = Workspace.realize_package ~intent:Package.Dev pkg in
    let realization_duration = duration_since realization_started_at in
    if scope_requested Runtime scopes then
      Vector.push
        nodes
        ~value:(realized_node
          ~scope:Runtime
          ~package:(projected_package Runtime package)
          ~realization_duration:Time.Duration.zero);
    Vector.push
      nodes
      ~value:(realized_node
        ~scope:Dev
        ~package:(projected_package ~dev_artifacts Dev package)
        ~realization_duration)
  ) else if scope_requested Runtime scopes then (
    let realization_started_at = Time.Instant.now () in
    let package = realize_projected_package workspace Runtime pkg in
    Vector.push
      nodes
      ~value:(realized_node
        ~scope:Runtime
        ~package
        ~realization_duration:(duration_since realization_started_at))
  );
  Array.to_list (Vector.to_array nodes)

let create_with_breakdown
  ~scope
  ?(dev_artifacts = {tests = true; examples = true; benches = true})
  ?dev_roots
  (workspace: Workspace.t) =
  let started_at = Time.Instant.now () in
  let graph = G.make () in
  let name_to_node = HashMap.create () in
  let missing = vec [] in
  let should_create_dev_node =
    match dev_roots with
    | None -> fun (_pkg: Package_manifest.t) -> true
    | Some roots ->
        fun (pkg: Package_manifest.t) ->
          List.any
            roots
            ~fn:(fun root -> Package_name.equal root pkg.name)
  in
  let insert_node package scope =
    let node = G.add_node graph (Unplanned { package; scope }) in
    let _ =
      HashMap.insert
        name_to_node
        ~key:(package_key ~package_name:(Package_name.to_string package.name) scope)
        ~value:node
    in
    ()
  in
  let task_scopes =
    match scope with
    | Build -> fun (_pkg: Package_manifest.t) -> [ Build ]
    | Runtime ->
        fun pkg ->
          if needs_build_scope_node pkg then
            [ Build; Runtime ]
          else
            [ Runtime ]
    | Dev ->
        fun pkg ->
          let scopes =
            if should_create_dev_node pkg then
              [ Runtime; Dev ]
            else
              [ Runtime ]
          in
          if needs_build_scope_node pkg then
            Build :: scopes
          else
            scopes
  in
  let realized_nodes =
    WorkerPool.SimpleWorkerPool.run
      ~tasks:(List.map workspace.packages ~fn:(fun pkg -> (task_scopes pkg, pkg)))
      ~fn:(realize_task ~dev_artifacts workspace)
      ()
    |> List.sort ~compare:(fun (left_index, _) (right_index, _) ->
      Int.compare left_index right_index)
    |> List.map ~fn:(fun (_index, realized) -> realized)
    |> List.concat
  in
  let breakdown =
    List.fold_left
      realized_nodes
      ~init:empty_breakdown
      ~fn:add_realized_node_to_breakdown
  in
  List.for_each
    realized_nodes
    ~fn:(fun realized -> insert_node realized.realized_package realized.realized_scope);
  let edge_wiring_started_at = Time.Instant.now () in
  List.for_each
    workspace.packages
    ~fn:(fun (pkg: Package_manifest.t) ->
      let add_dep_edge ~from_scope dep_name =
        let dep_name_string = Package_name.to_string dep_name in
        match HashMap.get
          name_to_node
          ~key:(package_key ~package_name:(Package_name.to_string pkg.name) from_scope) with
        | None -> ()
        | Some from_node -> (
            let dep_scope =
              match from_scope with
              | Build -> Build
              | Runtime -> Runtime
              | Dev -> Runtime
            in
            match HashMap.get
              name_to_node
              ~key:(package_key ~package_name:dep_name_string dep_scope) with
            | Some dep_node -> G.add_edge from_node ~depends_on:dep_node
            | None ->
                if not (is_well_known_package dep_name) then
                  Vector.push
                    missing
                    ~value:{
                      package = Package_name.to_string pkg.name;
                      dependency = dep_name_string;
                    }
          )
      in
      (
        match (
          HashMap.get
            name_to_node
            ~key:(package_key ~package_name:(Package_name.to_string pkg.name) Runtime),
          HashMap.get
            name_to_node
            ~key:(package_key ~package_name:(Package_name.to_string pkg.name) Build)
        ) with
        | (Some runtime_node, Some build_node) -> G.add_edge runtime_node ~depends_on:build_node
        | _ -> ()
      );
      (
        match (
          HashMap.get
            name_to_node
            ~key:(package_key ~package_name:(Package_name.to_string pkg.name) Dev),
          HashMap.get
            name_to_node
            ~key:(package_key ~package_name:(Package_name.to_string pkg.name) Runtime)
        ) with
        | (Some dev_node, Some runtime_node) -> G.add_edge dev_node ~depends_on:runtime_node
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
         tooling is the target.
      *)
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
  let edge_wiring_duration =
    Time.Instant.duration_since ~earlier:edge_wiring_started_at (Time.Instant.now ())
  in
  let breakdown = { breakdown with edge_wiring_duration } in
  let missing =
    Vector.iter missing
    |> Iterator.to_list
  in
  let result =
    if List.length missing > 0 then
      Error (MissingPackages { missing })
    else
      Ok ({ graph; name_to_node }, breakdown)
  in
  let () =
    trace_package_graph
      ("create scope="
      ^ string_of_build_scope scope
      ^ " manifests="
      ^ Int.to_string (List.length workspace.packages)
      ^ " nodes="
      ^ Int.to_string (HashMap.length name_to_node)
      ^ " missing="
      ^ Int.to_string (List.length missing)
      ^ " total_us="
      ^ Int.to_string (elapsed_us_since started_at))
  in
  result

let create ~scope ?dev_artifacts ?dev_roots workspace =
  create_with_breakdown ~scope ?dev_artifacts ?dev_roots workspace
  |> Result.map ~fn:(fun (graph, _breakdown) -> graph)

let clone_module_graph = fun (module_graph: Module_node.t G.t) ->
  let cloned_graph = G.make () in
  let node_by_id = HashMap.create () in
  G.iter
    module_graph
    ~fn:(fun _id node ->
      let cloned_value = { node.value with Module_node.open_modules = [] } in
      let cloned_node = G.add_node cloned_graph cloned_value in
      let _ = HashMap.insert node_by_id ~key:node.id ~value:cloned_node in
      ());
  G.iter
    module_graph
    ~fn:(fun _id node ->
      match HashMap.get node_by_id ~key:node.id with
      | None -> ()
      | Some cloned_node ->
          let cloned_open_modules =
            List.filter_map
              node.value.open_modules
              ~fn:(fun open_node -> HashMap.get node_by_id ~key:open_node.id)
          in
          Module_node.set_open_modules cloned_node.value cloned_open_modules;
          List.for_each
            node.deps
            ~fn:(fun dep_id ->
              match HashMap.get node_by_id ~key:dep_id with
              | None -> ()
              | Some cloned_dep_node -> G.add_edge cloned_node ~depends_on:cloned_dep_node)
    );
  cloned_graph

let clone_node_value = fun __tmp1 ->
  match __tmp1 with
  | Unplanned { package; scope } -> Unplanned { package; scope }
  | Planned {
      package;
      scope;
      module_graph;
      action_graph;
      hash;
    } ->
      Planned {
        package;
        scope;
        module_graph = clone_module_graph module_graph;
        action_graph = Action_graph.clone action_graph;
        hash;
      }
  | Cached {
      package;
      scope;
      hash;
      artifact;
      depset;
      exports;
    } ->
      Cached {
        package;
        scope;
        hash;
        artifact;
        depset;
        exports;
      }
  | Built {
      package;
      scope;
      module_graph;
      action_graph;
      hash;
      artifact;
      status;
      depset;
    } ->
      Built {
        package;
        scope;
        module_graph = clone_module_graph module_graph;
        action_graph = Action_graph.clone action_graph;
        hash;
        artifact;
        status;
        depset;
      }
  | Failed {
      package;
      scope;
      hash;
      error;
    } ->
      Failed {
        package;
        scope;
        hash;
        error;
      }
  | Skipped { package; scope; reason } -> Skipped { package; scope; reason }

let clone = fun pg ->
  let graph = G.make () in
  let name_to_node = HashMap.with_capacity ~size:(HashMap.length pg.name_to_node) in
  let node_by_id = HashMap.with_capacity ~size:(HashMap.length pg.name_to_node) in
  G.iter
    pg.graph
    ~fn:(fun id node ->
      let cloned = G.add_node graph (clone_node_value node.value) in
      let _ = HashMap.insert node_by_id ~key:id ~value:cloned in
      let _ = HashMap.insert name_to_node ~key:(get_key node.value) ~value:cloned in
      ());
  G.iter
    pg.graph
    ~fn:(fun id node ->
      match HashMap.get node_by_id ~key:id with
      | None -> ()
      | Some cloned_node ->
          List.for_each
            node.deps
            ~fn:(fun dep_id ->
              match HashMap.get node_by_id ~key:dep_id with
              | None -> ()
              | Some cloned_dep_node -> G.add_edge cloned_node ~depends_on:cloned_dep_node)
    );
  { graph; name_to_node }

let get_node = fun pg package ->
  HashMap.get
    pg.name_to_node
    ~key:(package_key ~package_name:(Package_name.to_string package.Package.name) Runtime)

let get_node_by_key = fun pg key -> HashMap.get pg.name_to_node ~key

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

let packages = fun pg -> G.map pg.graph ~fn:(fun (_id, node) -> get_package node.value)

let find_package = fun pg name ->
  match HashMap.get
    pg.name_to_node
    ~key:(package_key ~package_name:(Package_name.to_string name) Runtime) with
  | Some node -> Some (get_package node.value)
  | None -> None

let get_package_node = fun pg package ->
  match get_node pg package with
  | Some node -> Some node.value
  | None -> None

let target_node_for_package = fun pg pkg_name ->
  let package_name = Package_name.to_string pkg_name in
  let key_for scope = package_key ~package_name scope in
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
          ());
      List.for_each
        reachable_ids
        ~fn:(fun id ->
          let _ = HashSet.insert reachable_set ~value:id in
          ());
      let filtered_graph = G.make () in
      let filtered_name_to_node = HashMap.create () in
      let filtered_node_by_id = HashMap.create () in
      G.iter
        pg.graph
        ~fn:(fun id node ->
          if HashSet.contains reachable_set ~value:id then
            let new_node = G.add_node filtered_graph (clone_node_value node.value) in
            let _ = HashMap.insert filtered_node_by_id ~key:id ~value:new_node in
            let _ =
              HashMap.insert filtered_name_to_node ~key:(get_key node.value) ~value:new_node
            in
            ());
      G.iter
        pg.graph
        ~fn:(fun id node ->
          if HashSet.contains reachable_set ~value:id then
            match HashMap.get filtered_node_by_id ~key:id with
            | None -> ()
            | Some new_node ->
                List.for_each
                  node.deps
                  ~fn:(fun dep_id ->
                    if HashSet.contains reachable_set ~value:dep_id then
                      match HashMap.get filtered_node_by_id ~key:dep_id with
                      | Some new_dep_node -> G.add_edge new_node ~depends_on:new_dep_node
                      | None -> ());
      );
      { graph = filtered_graph; name_to_node = filtered_name_to_node }

let filter_for_package = fun pg pkg_name -> filter_for_packages pg [ pkg_name ]

let get_graph_node = fun pg node_id -> G.get_node pg.graph node_id

let get_dependencies_for_node = fun pg (node: package_node G.node) ->
  List.filter_map
    node.deps
    ~fn:(fun dep_id ->
      match G.get_node pg.graph dep_id with
      | Some dep_node -> Some dep_node.value
      | None -> None)

let direct_runtime_dependencies = fun pg package_name ->
  let key = package_key ~package_name:(Package_name.to_string package_name) Runtime in
  match get_node_by_key pg key with
  | None -> []
  | Some node ->
      get_dependencies_for_node pg node
      |> List.filter_map
        ~fn:(fun dependency ->
          match get_scope dependency with
          | Runtime ->
              let dependency_package = get_package dependency in
              if Package_name.equal dependency_package.name package_name then
                None
              else
                Some dependency_package
          | Build
          | Dev -> None)

let get_dependencies = fun graph (package: Package.t) ->
  let filtered_graph = filter_for_package graph package.name in
  match HashMap.get
    filtered_graph.name_to_node
    ~key:(package_key ~package_name:(Package_name.to_string package.name) Runtime) with
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
            | Some node -> Some (Package_name.to_string (get_package node.value).name)
            | None -> None)
      in
      raise (Cycle_detected names)
