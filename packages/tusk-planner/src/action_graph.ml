open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

type t = { graph : Action_node.action_spec G.t }

let create () = { graph = G.make () }
let add_node t node_value = G.add_node t.graph node_value
let add_dependency t node ~depends_on = G.add_edge node ~depends_on
let topo_sort t = G.topo_sort t.graph
let nodes t = topo_sort t
let graph t = t.graph

let to_action_list t =
  let sorted = topo_sort t in
  List.concat_map (fun (node : Action_node.t) -> node.value.actions) sorted

let hash_action_node _t (node : Action_node.t) = node.value.hash

let opens mods =
  List.filter_map
    (fun (node : Module_node.t G.node) ->
      match node.value.kind with
      | ML mod_ | MLI mod_ ->
          Some (Tusk_toolchain.Ocamlc.Open (Module.namespaced_name mod_))
      | _ -> None)
    mods

let module_to_actions ~package ~dep_includes ~get_dep_outputs
    (module_node : Module_node.t) (deps : G.Node_id.t list) :
    Action.t list * Path.t list * Path.t list =
  match module_node with
  | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [ path ] in
      let compile =
        Action.CompileInterface
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = opens open_modules;
          }
      in
      ([ compile ], outputs, sources)
  | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output ] in
      let sources = [ path ] in
      let compile =
        Action.CompileImplementation
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = opens open_modules;
          }
      in
      ([ compile ], outputs, sources)
  | { kind = ML mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action =
        Action.WriteFile { destination = path; content = contents }
      in

      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output ] in
      let sources = [] in

      let is_alias_file =
        String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path)
      in
      let flags =
        if is_alias_file then
          Tusk_toolchain.Ocamlc.Impl path :: Tusk_toolchain.Ocamlc.NoAliasDeps
          :: opens open_modules
        else opens open_modules
      in

      let compile_action =
        Action.CompileImplementation
          { source = path; outputs; includes = Path.v "." :: dep_includes; flags }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = MLI mod_; file = Generated { path; contents }; open_modules; _ } ->
      let write_action =
        Action.WriteFile { destination = path; content = contents }
      in

      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [] in
      let compile_action =
        Action.CompileInterface
          {
            source = path;
            outputs;
            includes = Path.v "." :: dep_includes;
            flags = opens open_modules;
          }
      in
      ([ write_action; compile_action ], outputs, sources)
  | { kind = Native { files }; _ } ->
      let c_files =
        List.filter
          (fun path -> String.ends_with ~suffix:".c" (Path.to_string path))
          files
      in

      let actions =
        List.map
          (fun c_file ->
            let obj_file =
              Path.remove_extension c_file |> Path.add_extension ~ext:"o"
            in
            let output_name = Path.basename obj_file |> Path.v in
            Action.CompileC { source = c_file; outputs = [ output_name ] })
          c_files
      in

      let outputs =
        List.map
          (fun c_file ->
            let obj_file =
              Path.remove_extension c_file |> Path.add_extension ~ext:"o"
            in
            Path.basename obj_file |> Path.v)
          c_files
      in

      (actions, outputs, files)
  | { kind = C; file = Generated _; _ }
  | { kind = H; _ }
  | { kind = Root; _ }
  | { kind = Other _; _ } ->
      ([], [], [])
  | { kind = Library { name; includes }; _ } ->
      let library_name = Module_name.(of_string name |> cmxa) in
      let static_lib_name = Module_name.(of_string name |> a) in
      let outputs = [ library_name; static_lib_name ] in
      let sources = [] in

      let objects =
        List.concat_map
          (fun dep_id ->
            let dep_outputs = get_dep_outputs dep_id in
            List.filter
              (fun path ->
                match Path.extension path with
                | Some ".cmx" | Some ".o" -> true
                | _ -> false)
              dep_outputs)
          deps
      in

      Log.debug
        "[ACTION_GRAPH] CreateLibrary for %s: %d dependencies, %d objects" name
        (List.length deps) (List.length objects);
      if List.length objects > 0 then
        Log.debug "[ACTION_GRAPH] CreateLibrary first 5 objects: %s"
          (String.concat ", "
             (List.map Path.to_string
                (let rec take n lst =
                   match (n, lst) with
                   | 0, _ | _, [] -> []
                   | n, x :: xs -> x :: take (n - 1) xs
                 in
                 take 5 objects)));

      let create_lib = Action.CreateLibrary { outputs; objects; includes } in
      ([ create_lib ], outputs, sources)
  | { kind = Binary { name; source; libraries; includes }; _ } ->
      Log.debug "[ACTION_GRAPH] Creating binary %s with %d libraries: [%s]" name
        (List.length libraries)
        (String.concat ", " (List.map Path.to_string libraries));
      let binary_mod =
        Module.make ~namespace:Namespace.empty ~filename:source
      in
      let binary_cmx = Module.cmx binary_mod in
      let sources = [ source ] in

      let compile_action =
        Action.CompileImplementation
          {
            source;
            outputs = [ binary_cmx ];
            includes = Path.v "." :: dep_includes;
            flags = [];
          }
      in

      let binary_output = Path.v name in
      let link_action =
        Action.CreateExecutable
          {
            outputs = [ binary_output ];
            objects = [ binary_cmx ];
            libraries;
            includes;
          }
      in
      ([ compile_action; link_action ], [ binary_output ], sources)

let from_module_graph ~package ~toolchain ~store ~depset
    (module_graph : Module_node.t G.t) : t * Path.t list =
  Log.info "[ACTION_GRAPH] from_module_graph starting for package: %s"
    package.Package.name;

  (* Extract dependency cache include paths - no file copying needed! *)
  let dep_includes =
    List.map
      (fun (dep : Dependency.t) ->
        Tusk_store.Store.get_artifact_dir store dep.artifact)
      depset
  in
  Log.info "[ACTION_GRAPH] Dependency includes (%d): [%s]"
    (List.length dep_includes)
    (String.concat ", " (List.map Path.to_string dep_includes));

  let action_graph = create () in
  let node_mapping = HashMap.create () in
  let action_spec_hashes = HashMap.create () in
  let node_outputs = HashMap.create () in
  let all_outputs = Cell.create [] in

  let sorted_modules = G.topo_sort module_graph in
  Log.info "[ACTION_GRAPH] Topologically sorted %d modules"
    (List.length sorted_modules);
  List.iteri
    (fun i (node : Module_node.t G.node) ->
      Log.debug "[ACTION_GRAPH] Topo order #%d: node_id=%s kind=%s" i
        (G.Node_id.to_string node.id)
        (Module_node.kind_to_string node.value.Module_node.kind))
    sorted_modules;

  let get_dep_hash dep_id =
    match HashMap.get action_spec_hashes dep_id with
    | Some h -> h
    | None ->
        panic
          "Dependency hash not found for node %s. Graph not in topological \
           order!"
          (G.Node_id.to_string dep_id)
  in

  let get_dep_outputs dep_id =
    match HashMap.get node_outputs dep_id with Some outs -> outs | None -> []
  in

  List.iter
    (fun (module_node : Module_node.t G.node) ->
      Log.debug "[ACTION_GRAPH] Processing module_node id=%s kind=%s deps=[%s]"
        (G.Node_id.to_string module_node.id)
        (Module_node.kind_to_string module_node.value.kind)
        (String.concat ", " (List.map G.Node_id.to_string module_node.deps));

      let actions, outputs, sources =
        module_to_actions ~package ~dep_includes ~get_dep_outputs
          module_node.value module_node.deps
      in

      Log.debug "[ACTION_GRAPH]   -> %d actions, %d outputs, %d sources"
        (List.length actions) (List.length outputs) (List.length sources);
      Log.debug "[ACTION_GRAPH]   -> outputs: [%s]"
        (String.concat ", " (List.map Path.to_string outputs));

      let _ = HashMap.insert node_outputs module_node.id outputs in
      if actions = [] then (
        let placeholder_hash =
          Crypto.hash_string
            (format "no-actions:%s" (G.Node_id.to_string module_node.id))
        in
        Log.debug "[ACTION_GRAPH]   -> placeholder hash: %s"
          (Crypto.Digest.hex placeholder_hash);
        let _ =
          HashMap.insert action_spec_hashes module_node.id placeholder_hash
        in
        ())
      else
        let action_spec =
          Action_node.make ~actions ~outs:outputs ~srcs:sources ~package
            ~toolchain ~dependency_hashes:get_dep_hash ~deps:module_node.deps
        in
        Log.info "[ACTION_GRAPH]   -> action_spec hash: %s"
          (Crypto.Digest.hex action_spec.hash);

        let action_node = add_node action_graph action_spec in

        List.iter
          (fun dep_id ->
            match HashMap.get node_mapping dep_id with
            | Some dep_action_node ->
                add_dependency action_graph action_node
                  ~depends_on:dep_action_node
            | None -> ())
          module_node.deps;

        let _ = HashMap.insert node_mapping module_node.id action_node in
        let _ =
          HashMap.insert action_spec_hashes module_node.id action_spec.hash
        in
        Cell.set all_outputs (outputs @ Cell.get all_outputs))
    sorted_modules;

  (action_graph, Cell.get all_outputs)

let to_json t =
  let open Data.Json in
  let all_nodes = nodes t in
  let sorted_nodes =
    List.sort
      (fun (a : Action_node.t) (b : Action_node.t) ->
        G.Node_id.to_int a.id - G.Node_id.to_int b.id)
      all_nodes
  in
  obj [ ("nodes", array (List.map Action_node.to_json sorted_nodes)) ]

let from_json json =
  let open Data.Json in
  match get_field "nodes" json with
  | None -> Error "Missing 'nodes' field"
  | Some (Array node_jsons) -> (
      let graph = create () in

      let parse_actions actions_json =
        match actions_json with
        | Array action_jsons ->
            List.fold_left
              (fun acc action_json ->
                match acc with
                | Error _ -> acc
                | Ok actions -> (
                    match Action.from_json action_json with
                    | Ok action -> Ok (action :: actions)
                    | Error err -> Error err))
              (Ok []) action_jsons
            |> Result.map List.rev
        | _ -> Error "actions must be array"
      in

      let parse_paths paths_json =
        match paths_json with
        | Array path_jsons ->
            Ok
              (List.filter_map
                 (function String s -> Some (Path.v s) | _ -> None)
                 path_jsons)
        | _ -> Error "paths must be array"
      in

      match
        List.fold_left
          (fun acc node_json ->
            match acc with
            | Error _ -> acc
            | Ok () -> (
                match
                  ( get_field "package" node_json,
                    get_field "actions" node_json,
                    get_field "outputs" node_json,
                    get_field "sources" node_json )
                with
                | ( Some (String pkg_name),
                    Some actions_json,
                    Some outputs_json,
                    Some sources_json ) -> (
                    match parse_actions actions_json with
                    | Error err -> Error err
                    | Ok actions -> (
                        match
                          (parse_paths outputs_json, parse_paths sources_json)
                        with
                        | Ok outputs, Ok sources ->
                            let package =
                              Package.
                                {
                                  name = pkg_name;
                                  path = Path.v ".";
                                  relative_path = Path.v ".";
                                  dependencies = [];
                                  binaries = [];
                                  library = None;
                                  sources =
                                    { src = []; native = []; tests = [] };
                                }
                            in
                            let toolchain =
                              Tusk_toolchain.init
                                ~config:Tusk_model.Toolchain_config.default
                              |> Result.expect
                                   ~msg:"Failed to initialize toolchain"
                            in
                            let action_spec =
                              Action_node.make ~actions ~outs:outputs
                                ~srcs:sources ~package ~toolchain
                                ~dependency_hashes:(fun _ ->
                                  Crypto.hash_string "")
                                ~deps:[]
                            in
                            let _ = add_node graph action_spec in
                            Ok ()
                        | Error err, _ | _, Error err -> Error err))
                | _ -> Error "Missing required node fields"))
          (Ok ()) node_jsons
      with
      | Error err -> Error err
      | Ok () -> Ok graph)
  | Some _ -> Error "nodes must be array"

let equal g1 g2 =
  let nodes1 = topo_sort g1 in
  let nodes2 = topo_sort g2 in
  try List.for_all2 Action_node.equal nodes1 nodes2 with _ -> false
