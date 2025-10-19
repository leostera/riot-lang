(** Build Planner - Orchestrates build graph creation, wiring, and action generation *)

open Std
open Tusk_model
open Tusk_ocaml

module G = Graph.SimpleGraph

type plan_input = {
  package : Package.t;
  toolchain : Toolchains.toolchain;
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
  
  let config = Graph_builder.{
    root = input.package.path;
    source_dir = input.planning_root;
    namespace;
    package = input.package;
    toolchain = input.toolchain;
    workspace = input.workspace;
  } in
  
  try
    let graph_builder = Graph_builder.create config in
    Graph_builder.wire_dependencies graph_builder input.planning_root;
    
    Graph_builder.add_library_node graph_builder 
      ~name:input.package.name 
      ~includes:[];
    
    List.iter (fun (bin : Package.binary) ->
      Graph_builder.add_binary_node graph_builder
        ~name:bin.name
        ~source:bin.path
        ~libraries:[]
        ~includes:[]
    ) input.package.binaries;
    
    let module_graph = Graph_builder.graph graph_builder in
    
    let sorted_modules = G.topo_sort module_graph in
    
    let action_graph, _outputs = Action_graph.from_module_graph ~package:input.package ~toolchain:input.toolchain module_graph in
    
    Ok { module_graph; action_graph }
  with
  | G.Cycle cycle_ids ->
      let cycle = List.map G.Node_id.to_string cycle_ids in
      Error (Planning_error.CyclicDependency { cycle })
  | exn ->
      Error (Planning_error.Exception { exn })

