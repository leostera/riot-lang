(** Build Planner - Orchestrates build graph creation, wiring, and action
    generation *)

open Std
open Tusk_model
open Tusk_ocaml
module G = Graph.SimpleGraph

type plan_input = {
  package : Package.t;
  toolchain : Tusk_toolchain.t;
  workspace : Workspace.t;
  planning_root : Path.t;
  dependencies : Dependency.t list;
}

type plan_result = {
  module_graph : Module_node.t G.t;
  action_graph : Action_graph.t;
}

let plan_node input =
  let namespace = String.capitalize_ascii input.package.name in

  let config =
    Graph_builder.
      {
        root = input.package.path;
        source_dir = input.planning_root;
        namespace;
        package = input.package;
        toolchain = input.toolchain;
        workspace = input.workspace;
      }
  in

  try
    let graph_builder = Graph_builder.create config in

    let native_dir = Path.v "native" in
    let native_path = Path.(input.package.path / native_dir) in
    (match Fs.is_dir native_path with
    | Ok true ->
        let native_entries =
          Module_scanner.scan ~root:input.package.path ~source_dir:native_dir
        in
        List.iter
          (fun entry ->
            match entry with
            | Module_scanner.C (name, path) ->
                let c_node =
                  Module_node.
                    { file = Concrete path; open_modules = []; kind = C }
                in
                let _ = G.add_node graph_builder.graph c_node in
                ()
            | _ -> ())
          native_entries
    | _ -> ());

    let sandbox_dir = Path.(input.package.path / input.planning_root) in
    Graph_builder.wire_dependencies graph_builder sandbox_dir;

    (match input.package.library with
    | Some _lib ->
        Graph_builder.add_library_node graph_builder ~name:input.package.name
          ~includes:[]
    | None -> ());

    List.iter
      (fun (bin : Package.binary) ->
        Graph_builder.add_binary_node graph_builder ~name:bin.name
          ~source:bin.path ~libraries:[] ~includes:[])
      input.package.binaries;

    let main_library_node_id : G.Node_id.t option =
      match input.package.library with
      | Some _lib ->
          let result = ref None in
          G.iter graph_builder.graph ~fn:(fun node_id node ->
              match node.value.Module_node.kind with
              | Module_node.Library _ when !result = None ->
                  result := Some node_id
              | _ -> ());
          !result
      | None -> None
    in

    (match input.package.test_library with
    | Some _test_lib ->
        let test_namespace = namespace ^ "Tests" in
        let test_config =
          Graph_builder.
            {
              root = input.package.path;
              source_dir = Path.v "tests";
              namespace = test_namespace;
              package = input.package;
              toolchain = input.toolchain;
              workspace = input.workspace;
            }
        in
        let test_graph_builder = Graph_builder.create test_config in
        let test_sandbox_dir = Path.(input.package.path / Path.v "tests") in
        Graph_builder.wire_dependencies test_graph_builder test_sandbox_dir;

        let test_nodes =
          G.map test_graph_builder.graph ~fn:(fun (node_id, node) -> node.value)
        in
        List.iter
          (fun test_node ->
            let test_node_added = G.add_node graph_builder.graph test_node in
            match (main_library_node_id, test_node.kind) with
            | Some lib_node_id, (Module_node.ML _ | Module_node.MLI _) ->
                let main_lib_node =
                  G.get_node graph_builder.graph lib_node_id
                in
                G.add_edge test_node_added ~depends_on:main_lib_node
            | _ -> ())
          test_nodes;

        Graph_builder.add_library_node test_graph_builder
          ~name:(input.package.name ^ "_tests")
          ~includes:[]
    | None -> ());

    let module_graph = Graph_builder.graph graph_builder in

    let sorted_modules = G.topo_sort module_graph in

    let action_graph, _outputs =
      Action_graph.from_module_graph ~package:input.package
        ~toolchain:input.toolchain module_graph
    in

    Ok { module_graph; action_graph }
  with
  | G.Cycle cycle_ids ->
      let cycle = List.map G.Node_id.to_string cycle_ids in
      Error (Planning_error.CyclicDependency { cycle })
  | exn -> Error (Planning_error.Exception { exn })
