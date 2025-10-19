open Std
open Std.Collections
open Tusk_model
open Tusk_ocaml

module G = Std.Graph.SimpleGraph

type t = { 
  graph : Action_node.action_spec G.t;
}

let create () = { 
  graph = G.make ();
}

let add_node t node_value =
  G.add_node t.graph node_value

let add_dependency t node ~depends_on =
  G.add_edge node ~depends_on

let topo_sort t = G.topo_sort t.graph

let nodes t =
  G.map t.graph ~fn:(fun (_id, node) -> node)

let graph t = t.graph

let to_action_list t =
  let sorted = topo_sort t in
  List.concat_map (fun (node : Action_node.t) -> node.value.actions) sorted

let hash_action_node _t (node : Action_node.t) =
  node.value.hash

let opens mods =
  List.filter_map
    (fun (node : Module_node.t G.node) ->
      match node.value.kind with
      | ML mod_ | MLI mod_ -> Some (Ocamlc.Open (Module.namespaced_name mod_))
      | _ -> None)
    mods

let module_to_actions (module_node : Module_node.t) : Action.t list * Path.t list * Path.t list =
  match module_node with
  | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [cmti_output; cmi_output] in
      let sources = [path] in
      let compile = Action.CompileInterface {
        source = path;
        output = cmi_output;
        includes = [ Path.v "." ];
        flags = opens open_modules;
      } in
      ([compile], outputs, sources)
      
  | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [cmt_output; cmi_output; cmx_output] in
      let sources = [path] in
      let compile = Action.CompileImplementation {
        source = path;
        output = cmx_output;
        includes = [ Path.v "." ];
        flags = opens open_modules;
      } in
      ([compile], outputs, sources)
      
  | { kind = ML mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { 
        destination = path; 
        content = contents 
      } in
      
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [cmt_output; cmi_output; cmx_output] in
      let sources = [] in
      
      let is_alias_file =
        String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path)
      in
      let flags = if is_alias_file then Ocamlc.NoAliasDeps :: opens open_modules
                 else opens open_modules in
      
      let compile_action = Action.CompileImplementation {
        source = path;
        output = cmx_output;
        includes = [ Path.v "." ];
        flags;
      } in
      ([write_action; compile_action], outputs, sources)
      
  | { kind = MLI mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { 
        destination = path; 
        content = contents 
      } in
      
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [cmti_output; cmi_output] in
      let sources = [] in
      let compile_action = Action.CompileInterface {
        source = path;
        output = cmi_output;
        includes = [ Path.v "." ];
        flags = opens open_modules;
      } in
      ([write_action; compile_action], outputs, sources)
      
  | { kind = C; file = Concrete path; _ } ->
      let obj_file = Path.remove_extension path |> Path.add_extension ~ext:"o" in
      let output_name = Path.basename obj_file |> Path.v in
      let outputs = [output_name] in
      let sources = [path] in
      let compile = Action.CompileC { 
        source = path; 
        output = output_name 
      } in
      ([compile], outputs, sources)
      
  | { kind = C; file = Generated _; _ }
  | { kind = H; _ } 
  | { kind = Root; _ } 
  | { kind = Other _; _ } -> ([], [], [])
  
  | { kind = Library { name; includes }; _ } ->
      let library_name = Module_name.(of_string name |> cmxa) in
      let static_lib_name = Module_name.(of_string name |> a) in
      let outputs = [library_name; static_lib_name] in
      let sources = [] in
      let create_lib = Action.CreateLibrary {
        output = library_name;
        objects = [];
        includes;
      } in
      ([create_lib], outputs, sources)
      
  | { kind = Binary { name; source; libraries; includes }; _ } ->
      let binary_mod = Module.make ~namespace:Namespace.empty ~filename:source in
      let binary_cmx = Module.cmx binary_mod in
      let sources = [source] in
      
      let compile_action = Action.CompileImplementation {
        source;
        output = binary_cmx;
        includes = [ Path.v "." ];
        flags = [];
      } in
      
      let binary_output = Path.v name in
      let link_action = Action.CreateExecutable {
        output = binary_output;
        objects = [ binary_cmx ];
        libraries;
        includes;
      } in
      ([compile_action; link_action], [binary_output], sources)

let from_module_graph ~package ~toolchain (module_graph : Module_node.t G.t) : t * Path.t list =
  let action_graph = create () in
  let node_mapping = HashMap.create () in
  let action_spec_hashes = HashMap.create () in
  let all_outputs = Cell.create [] in
  
  let sorted_modules = G.topo_sort module_graph in
  
  let get_dep_hash dep_id =
    match HashMap.get action_spec_hashes dep_id with
    | Some h -> h
    | None -> 
        panic 
          "Dependency hash not found for node %s. Graph not in topological order!"
          (G.Node_id.to_string dep_id)
  in
  
  List.iter (fun (module_node : Module_node.t G.node) ->
    let actions, outputs, sources = module_to_actions module_node.value in
    if actions = [] then (
      let placeholder_hash = Crypto.hash_string 
        (format "no-actions:%s" (G.Node_id.to_string module_node.id)) in
      let _ = HashMap.insert action_spec_hashes module_node.id placeholder_hash in
      ()
    ) else (
      let action_spec = Action_node.make 
        ~actions 
        ~outs:outputs 
        ~srcs:sources 
        ~package 
        ~toolchain 
        ~dependency_hashes:get_dep_hash
        ~deps:module_node.deps
      in
      let action_node = add_node action_graph action_spec in
      
      List.iter (fun dep_id ->
        match HashMap.get node_mapping dep_id with
        | Some dep_action_node ->
            add_dependency action_graph action_node ~depends_on:dep_action_node
        | None -> ()
      ) module_node.deps;
      
      let _ = HashMap.insert node_mapping module_node.id action_node in
      let _ = HashMap.insert action_spec_hashes module_node.id action_spec.hash in
      Cell.set all_outputs (outputs @ Cell.get all_outputs)
    )
  ) sorted_modules;
  
  (action_graph, Cell.get all_outputs)
