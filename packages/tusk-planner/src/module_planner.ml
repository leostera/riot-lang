(** Build Planner - Orchestrates build graph creation, wiring, and action
    generation *)

open Std
open Tusk_model
module G = Graph.SimpleGraph

type plan_input = {
  package : Package.t;
  toolchain : Tusk_toolchain.t;
  workspace : Workspace.t;
  planning_root : Path.t;
  dependencies : Dependency.t list;
}

type plan_result = {
  sources : Path.t list;
  module_graph : Module_node.t G.t;
  action_graph : Action_graph.t;
}

let plan_node input =
  let namespace = String.capitalize_ascii input.package.name in

  let config =
    Module_graph.
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
    let graph_builder = Module_graph.create config in

    (match input.package.sources.native with
    | [] -> ()
    | files ->
        let native_node = Module_node.make_native ~files in
        let _ = G.add_node (Module_graph.graph graph_builder) native_node in
        ());

    let sandbox_dir = Path.(input.package.path / input.planning_root) in
    Module_graph.wire_dependencies graph_builder sandbox_dir;

    (match input.package.library with
    | Some _lib ->
        Module_graph.add_library_node graph_builder ~name:input.package.name
          ~includes:[]
    | None -> ());

    List.iter
      (fun (bin : Package.binary) ->
        Module_graph.add_binary_node graph_builder ~name:bin.name
          ~source:bin.path ~libraries:[] ~includes:[])
      input.package.binaries;

    let main_library_node_id : G.Node_id.t option =
      match input.package.library with
      | Some _lib ->
          let result = ref None in
          G.iter (Module_graph.graph graph_builder) ~fn:(fun node_id node ->
              match node.value.Module_node.kind with
              | Module_node.Library _ when !result = None ->
                  result := Some node_id
              | _ -> ());
          !result
      | None -> None
    in

    let module_graph = Module_graph.graph graph_builder in

    let sorted_modules = G.topo_sort module_graph in

    let action_graph, _outputs =
      Action_graph.from_module_graph ~package:input.package
        ~toolchain:input.toolchain module_graph
    in

    let sources =
      module_graph |> G.topo_sort
      |> List.concat_map (fun (node : Module_node.t G.node) ->
          match node.value.kind with
          | Native { files } ->
              List.map
                (fun path ->
                  if Path.is_absolute path then path
                  else Path.(input.package.path / path))
                files
          | _ -> (
              match node.value.file with
              | Concrete path when Path.to_string path <> "" ->
                  let abs_path =
                    if Path.is_absolute path then path
                    else Path.(input.package.path / path)
                  in
                  [ abs_path ]
              | _ -> []))
    in

    Ok { sources; module_graph; action_graph }
  with
  | G.Cycle cycle_ids ->
      let cycle = List.map G.Node_id.to_string cycle_ids in
      Error (Planning_error.CyclicDependency { cycle })
  | exn -> Error (Planning_error.Exception { exn })
