open Std
open Std.Collections
open Riot_model
module G = Std.Graph.SimpleGraph

let sorted_nodes = fun module_graph ->
  match G.topo_sort module_graph with
  | Ok nodes -> nodes
  | Error _cycle_ids -> G.map module_graph ~fn:(fun (_id, node) -> node)

let analyzed_modules_by_id = fun analyzed_modules ->
  let by_id = HashMap.create () in
  let () =
    List.for_each analyzed_modules
      ~fn:(fun (node_id, analyzed_module) ->
        let _ = HashMap.insert by_id ~key:node_id ~value:analyzed_module in
        ())
  in
  by_id

let library_root_candidates = fun ~package nodes ->
  let package_namespace = Package.root_module_name package in
  List.filter nodes
    ~fn:(fun (node: Module_node.t G.node) ->
      match node.value.kind with
      | Module_node.ML mod_
      | Module_node.MLI mod_ ->
          String.equal
            (Module.module_name mod_ |> Module_name.to_string)
            package_namespace
      | _ -> false)

let concrete_library_reachable_set = fun library_roots module_graph ->
  let reachable = HashSet.create () in
  let rec visit node_id =
    if HashSet.insert reachable ~value:node_id then
      match G.get_node module_graph node_id with
      | Some (node: Module_node.t G.node) ->
          List.for_each node.deps
            ~fn:(fun dep_id ->
              match G.get_node module_graph dep_id with
              | Some (dep_node: Module_node.t G.node) -> (
                  match dep_node.value.kind with
                  | Module_node.Root
                  | Module_node.Library _
                  | Module_node.Binary _ -> ()
                  | _ -> visit dep_id
                )
              | None -> ())
      | None -> ()
  in
  let () =
    List.for_each library_roots ~fn:(fun (node: Module_node.t G.node) -> visit node.id)
  in
  reachable

let binary_source_nodes = fun ~source_path nodes ->
  List.filter nodes
    ~fn:(fun (node: Module_node.t G.node) ->
      match node.value.kind, node.value.file with
      | (Module_node.ML _ | Module_node.MLI _), Module_node.Concrete path -> Path.equal path source_path
      | _ -> false)

let target_root_nodes = fun package nodes ->
  List.filter_map package.Package.binaries
    ~fn:(fun (binary: Package.binary) ->
      binary_source_nodes ~source_path:binary.path nodes
      |> List.map ~fn:(fun node -> (binary.name, node))
      |> List.head)

let target_reachable_set = fun start_nodes module_graph ->
  let reachable = HashSet.create () in
  let rec visit node_id =
    if HashSet.insert reachable ~value:node_id then
      match G.get_node module_graph node_id with
      | Some (node: Module_node.t G.node) ->
          List.for_each node.deps
            ~fn:(fun dep_id ->
              match G.get_node module_graph dep_id with
              | Some (dep_node: Module_node.t G.node) -> (
                  match dep_node.value.kind with
                  | Module_node.Root
                  | Module_node.Library _
                  | Module_node.Binary _ -> ()
                  | _ -> visit dep_id
                )
              | None -> ())
      | None -> ()
  in
  let () =
    List.for_each start_nodes ~fn:(fun (node: Module_node.t G.node) -> visit node.id)
  in
  reachable

let requested_modules = fun analyzed_module ->
  match analyzed_module.Module_graph.deps with
  | Ok deps -> Syn.Deps.modules deps
  | Error _ -> []

let classify_other_target_root_error = fun ~target_name ~source ~requested_modules ~other_target_name ~public_module (
  dep_node: Module_node.t G.node
) ->
  match dep_node.value.kind with
  | Module_node.ML mod_
  | Module_node.MLI mod_ ->
      let simple_name = Module.module_name mod_ |> Module_name.to_string in
      let other_target_module = Module.namespaced_name mod_ in
      let requested_module =
        if List.any requested_modules ~fn:(String.equal other_target_module) then
          other_target_module
        else if List.any requested_modules ~fn:(String.equal simple_name) then
          simple_name
        else
          other_target_module
      in
      Planning_error.TargetDependsOnOtherTargetRoot {
        target_name;
        source;
        requested_module;
        other_target_name;
        other_target_module;
        public_module;
      }
  | _ -> panic "expected concrete ML/MLI dependency when classifying other target root error"

let classify_internal_module_error = fun ~target_name ~source ~requested_modules (
  dep_node: Module_node.t G.node
) ~public_module ->
  match dep_node.value.kind with
  | Module_node.ML mod_
  | Module_node.MLI mod_ ->
      let simple_name = Module.module_name mod_ |> Module_name.to_string in
      let internal_name = Module.namespaced_name mod_ in
      if List.any requested_modules ~fn:(String.equal internal_name) then
        Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
          target_name;
          source;
          requested_module = internal_name;
          internal_module = internal_name;
          public_module;
        }
      else
        Planning_error.TargetDependsOnInternalLibraryModule {
          target_name;
          source;
          requested_module =
            if List.any requested_modules ~fn:(String.equal simple_name) then
              simple_name
            else
              internal_name;
          internal_module = internal_name;
          public_module;
        }
  | _ -> panic "expected concrete ML/MLI dependency when classifying internal module error"

let validate_target_source = fun ~target_name ~(source_node:Module_node.t G.node) ~library_reachable_set ~public_root_ids ~public_module ~module_graph ~analyzed_modules_by_id ~other_target_root_ids ->
  let nodes_to_check =
    let reachable = target_reachable_set [ source_node ] module_graph in
    HashSet.to_list reachable
    |> List.filter_map
      ~fn:(fun node_id ->
        match G.get_node module_graph node_id with
        | Some (node: Module_node.t G.node) -> (
            match node.value.kind, node.value.file with
            | (Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _ ->
                if
                  G.Node_id.eq node.id source_node.id
                  || not (HashSet.contains library_reachable_set ~value:node.id)
                then
                  Some node
                else
                  None
            | _ -> None
          )
        | None -> None)
  in
  List.fold_left nodes_to_check ~init:(Ok ())
    ~fn:(fun acc (node: Module_node.t G.node) ->
      match acc with
      | Error _ as err ->
          err
      | Ok () -> (
          match HashMap.get analyzed_modules_by_id ~key:node.id with
          | None ->
              Ok ()
          | Some analyzed_module ->
              let requested = requested_modules analyzed_module in
              match
                List.find node.deps
                  ~fn:(fun dep_id -> Option.is_some (HashMap.get other_target_root_ids ~key:dep_id))
              with
              | Some dep_id -> (
                  match G.get_node module_graph dep_id with
                  | Some (dep_node: Module_node.t G.node) ->
                      let other_target_name =
                        HashMap.get other_target_root_ids ~key:dep_id
                        |> Option.unwrap_or ~default:"unknown"
                      in
                      Error (
                        classify_other_target_root_error
                          ~target_name
                          ~source:(
                            node.value.file |> function
                            | Module_node.Concrete path -> path
                            | Module_node.Generated { path; _ } -> path
                          )
                          ~requested_modules:requested
                          ~other_target_name
                          ~public_module
                          dep_node
                      )
                  | None ->
                      Ok ()
                )
              | None -> (
                  match
                    List.find node.deps
                      ~fn:(fun dep_id ->
                        if HashSet.contains public_root_ids ~value:dep_id then
                          false
                        else if not (HashSet.contains library_reachable_set ~value:dep_id) then
                          false
                        else
                          match G.get_node module_graph dep_id with
                          | Some (dep_node: Module_node.t G.node) -> (
                              match dep_node.value.kind, dep_node.value.file with
                              | (Module_node.ML _ | Module_node.MLI _), Module_node.Concrete _ ->
                                  true
                              | _ ->
                                  false
                            )
                          | None ->
                              false)
                  with
                  | Some dep_id -> (
                      match G.get_node module_graph dep_id with
                      | Some (dep_node: Module_node.t G.node) ->
                          Error (
                            classify_internal_module_error
                              ~target_name
                              ~source:(
                                node.value.file |> function
                                | Module_node.Concrete path -> path
                                | Module_node.Generated { path; _ } -> path
                              )
                              ~requested_modules:requested
                              dep_node
                              ~public_module
                          )
                      | None ->
                          Ok ()
                    )
                  | None ->
                      Ok ()
                )
        ))

let validate = fun ~package ~module_graph ~analyzed_modules ->
  let nodes = sorted_nodes module_graph in
  let analyzed_modules_by_id = analyzed_modules_by_id analyzed_modules in
  let public_roots = library_root_candidates ~package nodes in
  let public_root_ids = HashSet.create () in
  let () =
    List.for_each public_roots
      ~fn:(fun (node: Module_node.t G.node) ->
        let _ = HashSet.insert public_root_ids ~value:node.id in
        ())
  in
  let library_reachable_set = concrete_library_reachable_set public_roots module_graph in
  let public_module = Package.root_module_name package in
  let target_root_nodes = target_root_nodes package nodes in
  List.fold_left nodes ~init:(Ok ())
    ~fn:(fun acc (node: Module_node.t G.node) ->
      match acc with
      | Error _ as err -> err
      | Ok () -> (
          match node.value.kind with
          | Module_node.Binary { name; source; _ } ->
              let source_nodes = binary_source_nodes ~source_path:source nodes in
              let other_target_root_ids = HashMap.create () in
              let () =
                List.for_each target_root_nodes
                  ~fn:(fun (other_target_name, other_target_node) ->
                    if not (String.equal other_target_name name) then
                      let _ = HashMap.insert
                        other_target_root_ids
                        ~key:other_target_node.id
                        ~value:other_target_name
                      in
                      ())
              in
              List.fold_left source_nodes ~init:(Ok ())
                ~fn:(fun source_acc (source_node: Module_node.t G.node) ->
                  match source_acc with
                  | Error _ as err -> err
                  | Ok () -> validate_target_source
                    ~target_name:name
                    ~source_node
                    ~library_reachable_set
                    ~public_root_ids
                    ~public_module
                    ~module_graph
                    ~analyzed_modules_by_id
                    ~other_target_root_ids)
          | _ -> Ok ()
        ))
