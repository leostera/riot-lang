(** Dependency graph for modules with cross-level dependency support *)

open Std

module DepId : sig
  type t

  val next : unit -> t
  val eq : t -> t -> bool
end = struct
  type t = int

  let next =
    let _id = ref 0 in
    fun () ->
      _id := !_id + 1;
      !_id

  let eq = Int.equal
end

type level = Root | Library of { dir : Path.t; parent : level }

let rec level_to_int l =
  match l with Root -> 0 | Library { parent; _ } -> 1 + level_to_int parent

type file_kind = Interface | Implementation | Alias

type node = {
  id : DepId.t; (* The id of this node in the dep graph *)
  module_info : Mod_tree.module_info;
      (* The actual module (can be Concrete or Generated) *)
  file_kind : file_kind; (* Whether this is .mli, .ml, or alias module *)
  level : level; (* 0 = package level, 1 = first subfolder level, etc. *)
  parent_aliases : string list;
      (* Alias modules that should be opened for this module *)
  mutable dependencies : node list; (* Direct dependencies of this module *)
  mutable dependents : node list; (* Modules that depend on this one *)
}
(** A node in the dependency graph *)

type t = {
  nodes : (DepId.t, node) Hashtbl.t; (* Map from node ID to node *)
  name_to_id : (string, DepId.t) Hashtbl.t;
      (* Map from namespaced name to node ID for lookups *)
  toolchain : Toolchains.toolchain; (* OCaml toolchain for running ocamldep *)
  package : Workspace.package; (* The package being compiled *)
}
(** The dependency graph *)

(** Create an empty dependency graph *)
let create ~toolchain ~package =
  {
    nodes = Hashtbl.create 100;
    name_to_id = Hashtbl.create 100;
    toolchain;
    package;
  }

(** Add a node to the graph *)
let add_node graph ~module_info ~file_kind ~level ~parent_aliases =
  (* Create a unique key for this node based on module name and file kind *)
  let namespaced_name =
    match module_info with
    | Mod_tree.Concrete info -> info.namespaced_name
    | Mod_tree.Generated info -> info.simple_name
  in

  (* Add suffix to distinguish interface from implementation *)
  let node_key =
    match file_kind with
    | Interface -> namespaced_name ^ ".mli"
    | Implementation -> namespaced_name ^ ".ml"
    | Alias -> namespaced_name
  in

  let id = DepId.next () in

  let node =
    {
      id;
      module_info;
      file_kind;
      level;
      parent_aliases;
      dependencies = [];
      dependents = [];
    }
  in

  Hashtbl.add graph.nodes id node;
  Hashtbl.add graph.name_to_id node_key id;
  ()

(** Get a node by its namespaced name *)
let find_node graph name =
  match Hashtbl.find_opt graph.name_to_id name with
  | Some id -> Hashtbl.find_opt graph.nodes id
  | None -> None

(** Get a node by its ID *)
let find_node_by_id graph id = Hashtbl.find_opt graph.nodes id

(** Add a dependency edge from source to target *)
let add_dependency graph ~source ~target =
  source.dependencies <- target :: source.dependencies;
  target.dependents <- source :: target.dependents

(** Convert a Mod_tree to a dependency graph *)
let from_mod_tree ~toolchain ~package tree =
  let graph = create ~toolchain ~package in

  (* First pass: create all nodes with proper levels and aliases *)
  let rec collect_nodes tree level parent_aliases =
    match tree with
    | Mod_tree.Package { name; children; aliases; entry_point; _ } ->
        (* Determine package-level aliases *)
        let safe_package_name =
          String.map (fun c -> if c = '-' then '_' else c) package.name
        in
        let main_alias_module =
          String.capitalize_ascii safe_package_name ^ "__aliases"
        in
        let package_aliases =
          if aliases <> [] then [ main_alias_module ] else []
        in

        (* Add alias modules as nodes *)
        List.iter
          (fun alias_info ->
            add_node graph ~module_info:alias_info ~file_kind:Alias ~level
              ~parent_aliases:[])
          aliases;

        (* Add entry point if it exists - create separate nodes for .mli and .ml *)
        (match entry_point with
        | Some (Mod_tree.Concrete { intf; impl; _ } as info) ->
            (* Add interface node if .mli exists *)
            if intf <> None then
              add_node graph ~module_info:info ~file_kind:Interface ~level
                ~parent_aliases:package_aliases;
            (* Add implementation node if .ml exists *)
            if impl <> None then
              add_node graph ~module_info:info ~file_kind:Implementation ~level
                ~parent_aliases:package_aliases
        | Some info ->
            (* Generated module - single node *)
            add_node graph ~module_info:info ~file_kind:Alias ~level
              ~parent_aliases:package_aliases
        | None -> ());

        (* Process children *)
        List.iter
          (fun child ->
            match child with
            | Mod_tree.Module (Mod_tree.Concrete { intf; impl; _ } as info) ->
                (* Direct module at package level - split into interface and implementation *)
                if intf <> None then
                  add_node graph ~module_info:info ~file_kind:Interface ~level
                    ~parent_aliases:package_aliases;
                if impl <> None then
                  add_node graph ~module_info:info ~file_kind:Implementation
                    ~level ~parent_aliases:package_aliases
            | Mod_tree.Module info ->
                (* Generated alias module *)
                add_node graph ~module_info:info ~file_kind:Alias ~level
                  ~parent_aliases:package_aliases
            | Mod_tree.Library { name = lib_name; _ } ->
                (* Subfolder - process recursively *)
                (* Get the directory path for this library *)
                let dir_path =
                  (* Extract directory from the library's qualified name *)
                  let qualified = Mod_name.qualified_name lib_name in
                  (* Remove the package prefix to get the relative path *)
                  let prefix_len = String.length package.name + 2 in
                  (* +2 for "__" *)
                  if String.length qualified > prefix_len then
                    String.sub qualified prefix_len
                      (String.length qualified - prefix_len)
                    |> String.lowercase_ascii
                    |> String.map (fun c -> if c = '_' then '/' else c)
                  else Mod_name.module_name lib_name |> String.lowercase_ascii
                in
                let new_level =
                  Library
                    {
                      dir =
                        Path.of_string dir_path
                        |> Result.expect
                             ~msg:
                               (Printf.sprintf "Invalid library path: %s"
                                  dir_path);
                      parent = level;
                    }
                in
                collect_nodes child new_level package_aliases
            | _ -> ())
          children
    | Mod_tree.Library { name; children; aliases; folder_interface; _ } ->
        (* For libraries/folders, determine their alias module *)
        let folder_alias_name =
          match
            List.find_opt
              (function
                | Mod_tree.Generated { simple_name; _ } ->
                    String.ends_with ~suffix:"__aliases" simple_name
                | _ -> false)
              aliases
          with
          | Some (Mod_tree.Generated { simple_name; _ }) -> Some simple_name
          | _ -> None
        in

        let new_parent_aliases =
          match folder_alias_name with
          | Some name -> parent_aliases @ [ name ]
          | None -> parent_aliases
        in

        (* Add alias modules as nodes *)
        List.iter
          (fun alias_info ->
            add_node graph ~module_info:alias_info ~file_kind:Alias ~level
              ~parent_aliases:[])
          aliases;

        (* Add folder interface if it exists - split into interface and implementation *)
        (match folder_interface with
        | Some (Mod_tree.Concrete { intf; impl; _ } as info) ->
            if intf <> None then
              add_node graph ~module_info:info ~file_kind:Interface ~level
                ~parent_aliases:new_parent_aliases;
            if impl <> None then
              add_node graph ~module_info:info ~file_kind:Implementation ~level
                ~parent_aliases:new_parent_aliases
        | Some info ->
            (* Generated module *)
            add_node graph ~module_info:info ~file_kind:Alias ~level
              ~parent_aliases:new_parent_aliases
        | None -> ());

        (* Process children *)
        List.iter
          (fun child ->
            match child with
            | Mod_tree.Module (Mod_tree.Concrete { intf; impl; _ } as info) ->
                (* Module in subfolder - split into interface and implementation *)
                if intf <> None then
                  add_node graph ~module_info:info ~file_kind:Interface ~level
                    ~parent_aliases:new_parent_aliases;
                if impl <> None then
                  add_node graph ~module_info:info ~file_kind:Implementation
                    ~level ~parent_aliases:new_parent_aliases
            | Mod_tree.Module info ->
                (* Generated alias module *)
                add_node graph ~module_info:info ~file_kind:Alias ~level
                  ~parent_aliases:new_parent_aliases
            | Mod_tree.Library { name = lib_name; _ } ->
                (* Nested subfolder *)
                let dir_path =
                  (* Extract just the folder name for the path *)
                  Mod_name.module_name lib_name |> String.lowercase_ascii
                in
                let new_level =
                  Library
                    {
                      dir =
                        Path.of_string dir_path
                        |> Result.expect
                             ~msg:
                               (Printf.sprintf "Invalid library path: %s"
                                  dir_path);
                      parent = level;
                    }
                in
                collect_nodes child new_level new_parent_aliases
            | _ -> ())
          children
    | Mod_tree.Module info -> (
        (* Standalone module (shouldn't happen at top level) *)
        match info with
        | Mod_tree.Concrete { impl; intf; _ } ->
            (* Create separate nodes for .mli and .ml if they exist *)
            if intf <> None then
              add_node graph ~module_info:info ~file_kind:Interface ~level
                ~parent_aliases;
            if impl <> None then
              add_node graph ~module_info:info ~file_kind:Implementation ~level
                ~parent_aliases
        | Mod_tree.Generated _ ->
            (* Generated modules are alias modules *)
            add_node graph ~module_info:info ~file_kind:Alias ~level
              ~parent_aliases)
  in

  collect_nodes tree Root [];

  Format.eprintf "[DEBUG Dep_graph] Created %d nodes for %s@.%!"
    (Hashtbl.length graph.nodes)
    package.name;
  Format.eprintf "[DEBUG Dep_graph] Nodes:@.%!";
  Hashtbl.iter
    (fun _ node ->
      let name =
        match node.module_info with
        | Mod_tree.Concrete info -> info.namespaced_name
        | Mod_tree.Generated info -> info.simple_name
      in
      let kind_str =
        match node.file_kind with
        | Interface -> ".mli"
        | Implementation -> ".ml"
        | Alias -> " (alias)"
      in
      Format.eprintf "  - %s%s (level=%d, parent_aliases=[%s])@.%!" name
        kind_str (level_to_int node.level)
        (String.concat ", " node.parent_aliases))
    graph.nodes;

  (* Second pass: find dependencies using ocamldep *)
  let find_dependencies () =
    (* Create a temporary directory for ocamldep analysis *)
    let result =
      Fs.with_tempdir ~prefix:"tusk_depgraph" (fun temp_dir_path ->
          let temp_dir = Path.to_string temp_dir_path in

          (* Copy all source files to temp directory *)
          (* We need to copy ALL files (.ml and .mli) for ocamldep to analyze dependencies correctly *)
          (* Map file names to their corresponding nodes *)
          let file_to_node = Hashtbl.create 100 in
          let copied_files = Hashtbl.create 100 in
          (* Track which files we've already copied *)

          Hashtbl.iter
            (fun _id node ->
              match node.module_info with
              | Mod_tree.Concrete
                  { impl; intf; simple_name; namespaced_name; _ } -> (
                  (* Copy files based on what THIS node represents *)
                  match node.file_kind with
                  | Implementation -> (
                      (* This node represents the .ml file *)
                      match impl with
                      | Some src ->
                          let basename = Path.basename src.Build_node.file in
                          if not (Hashtbl.mem copied_files basename) then (
                            let dest = Filename.concat temp_dir basename in
                            let dest_path =
                              Path.of_string dest
                              |> Result.expect
                                   ~msg:
                                     (Printf.sprintf "Invalid destination path: %s"
                                        dest)
                            in
                            let () =
                              Fs.copy_file src.Build_node.file dest_path
                              |> Result.expect
                                   ~msg:
                                     (Printf.sprintf "Failed to copy file %s to %s"
                                        (Path.to_string src.Build_node.file)
                                        (Path.to_string dest_path))
                            in
                            Hashtbl.add copied_files basename ();
                            (* Map this .ml file to this Implementation node *)
                            Hashtbl.add file_to_node basename node)
                      | None -> ())
                  | Interface -> (
                      (* This node represents the .mli file *)
                      match intf with
                      | Some src ->
                          let basename = Path.basename src.Build_node.file in
                          if not (Hashtbl.mem copied_files basename) then (
                            let dest = Filename.concat temp_dir basename in
                            let dest_path =
                              Path.of_string dest
                              |> Result.expect
                                   ~msg:
                                     (Printf.sprintf "Invalid destination path: %s"
                                        dest)
                            in
                            let () =
                              Fs.copy_file src.Build_node.file dest_path
                              |> Result.expect
                                   ~msg:
                                     (Printf.sprintf "Failed to copy file %s to %s"
                                        (Path.to_string src.Build_node.file)
                                        (Path.to_string dest_path))
                            in
                            Hashtbl.add copied_files basename ();
                            (* Map this .mli file to this Interface node *)
                            Hashtbl.add file_to_node basename node)
                      | None -> ())
                  | Alias -> ())
              | Mod_tree.Generated { kind = Static { contents; path }; _ }
                when node.file_kind = Alias ->
                  (* Write generated alias file *)
                  let filename = Path.to_string path in
                  if filename <> "" && not (Hashtbl.mem copied_files filename)
                  then (
                    let dest = Filename.concat temp_dir filename in
                    let oc = open_out dest in
                    output_string oc contents;
                    close_out oc;
                    Hashtbl.add copied_files filename ();
                    Hashtbl.add file_to_node filename node)
              | _ -> ())
            graph.nodes;

          (* Run ocamldep separately for interfaces and implementations *)
          let all_files =
            Sys.readdir temp_dir |> Array.to_list
            |> List.filter (fun f ->
                String.ends_with ~suffix:".ml" f
                || String.ends_with ~suffix:".mli" f
                || String.ends_with ~suffix:".ml.gen" f)
          in

          (* Separate .mli and .ml files *)
          let mli_files =
            List.filter (fun f -> String.ends_with ~suffix:".mli" f) all_files
          in
          let ml_files =
            List.filter
              (fun f ->
                String.ends_with ~suffix:".ml" f
                || String.ends_with ~suffix:".ml.gen" f)
              all_files
          in

          (* Helper to run ocamldep and parse results *)
          let run_ocamldep files =
            if files <> [] then
              (* Get dependencies for all files using Ocamldep module *)
              let deps_list =
                List.map
                  (fun file ->
                    (* Use the Ocamldep module to get dependencies *)
                    (* Get the package namespace for this file *)
                    let package_namespace = Mod_name.namespace_of_list [
                      String.capitalize_ascii package.name
                    ] in
                    let deps = Ocamldep.deps ~toolchain:graph.toolchain ~cwd:temp_dir ~file ~package_namespace in
                    (file, deps))
                  files
              in

              (* Process dependencies from the list *)
              List.iter
                (fun (file, deps) ->
                  (* Find the node for this file *)
                  match Hashtbl.find_opt file_to_node file with
                  | Some source_node ->
                      (* For each dependency, find the corresponding node *)
                      List.iter
                        (fun dep_modname ->
                          let dep_module = Mod_name.module_name dep_modname in
                          let qualified_name = Mod_name.qualified_name dep_modname in
                          
                          (* Try to find a node with this module name *)
                          let possible_names =
                            [
                              qualified_name;  (* Already qualified name from ocamldep *)
                              qualified_name ^ ".ml";
                              qualified_name ^ ".mli";
                              dep_module;  (* Try unqualified too *)
                            ]
                          in

                          (* Track whether we found the dependency *)
                          let found = ref false in
                          
                          List.iter
                            (fun name ->
                              if not !found then (
                                (* Determine what kind of node to look for based on source *)
                                let keys_to_try =
                                  match source_node.file_kind with
                                  | Interface ->
                                      (* .mli files can only depend on other .mli files *)
                                      [ name ^ ".mli"; name ]
                                  | Implementation ->
                                      (* .ml files need both:
                                         - the .mli for type checking during compilation
                                         - the .ml for linking (to get the .cmo)
                                         We add both dependencies to ensure proper ordering *)
                                      [ name ^ ".mli"; name ^ ".ml"; name ]
                                  | Alias ->
                                      (* Alias modules are self-contained *)
                                      [ name ]
                                in
                                
                                List.iter
                                  (fun key ->
                                    if not !found then (
                                      match find_node graph key with
                                      | Some target_node ->
                                          (* Don't add self-dependencies *)
                                          if target_node.id <> source_node.id then (
                                            add_dependency graph ~source:source_node
                                              ~target:target_node;
                                            (* For .ml dependencies, we found at least one target,
                                               but continue looking for both .mli and .ml *)
                                            if source_node.file_kind <> Implementation then
                                              found := true
                                          )
                                      | None -> ()))
                                  keys_to_try;
                                  
                                (* Mark as found if we added any dependency *)
                                if source_node.file_kind = Implementation && 
                                   List.exists (fun dep -> dep <> source_node) source_node.dependencies then
                                  found := true))
                            possible_names;
                          
                          (* If we couldn't find the dependency, it might be a standard library module *)
                          (* Only warn for debugging, don't fail *)
                          ())
                        deps
                  | None -> 
                      Format.eprintf "[DEBUG Dep_graph] WARNING: No node found for file %s@.%!" file)
                deps_list
          in

          (* Run ocamldep on .mli files first *)
          Format.eprintf
            "[DEBUG Dep_graph] Running ocamldep on %d .mli files@.%!"
            (List.length mli_files);
          if package.name = "miniriot" then (
            Format.eprintf "[DEBUG Dep_graph] Files in temp dir for miniriot: [%s]@.%!"
              (String.concat ", " all_files);
            Format.eprintf "[DEBUG Dep_graph] .mli files: [%s]@.%!"
              (String.concat ", " mli_files);
          );
          
          (try
            run_ocamldep mli_files
          with Failure msg ->
            Format.eprintf "[DEBUG Dep_graph] Failed during mli processing: %s@.%!" msg;
            raise (Failure msg));

          (* Run ocamldep on .ml files *)
          Format.eprintf
            "[DEBUG Dep_graph] Running ocamldep on %d .ml files@.%!"
            (List.length ml_files);
          
          (try
            run_ocamldep ml_files
          with Failure msg ->
            Format.eprintf "[DEBUG Dep_graph] Failed during ml processing: %s@.%!" msg;
            raise (Failure msg));
            
          Ok ())
    in
    match result with
    | Ok _ -> ()
    | Error err ->
        Format.eprintf
          "[DEBUG] Failed to process dependencies in temp directory@.";
        ()
  in

  find_dependencies ();

  (* Add dependency from .ml nodes to their corresponding .mli nodes *)
  Hashtbl.iter
    (fun _ node ->
      match (node.module_info, node.file_kind) with
      | Mod_tree.Concrete { namespaced_name; _ }, Implementation -> (
          (* Look for corresponding interface node *)
          let intf_key = namespaced_name ^ ".mli" in
          match find_node graph intf_key with
          | Some intf_node ->
              Format.eprintf "[DEBUG Dep_graph] %s.ml depends on %s.mli@.%!"
                namespaced_name namespaced_name;
              add_dependency graph ~source:node ~target:intf_node
          | None -> ())
      | _ -> ())
    graph.nodes;

  Format.eprintf "[DEBUG Dep_graph] Dependencies found:@.%!";
  Hashtbl.iter
    (fun _ node ->
      if node.dependencies <> [] then
        let name =
          match node.module_info with
          | Mod_tree.Concrete info -> info.namespaced_name
          | Mod_tree.Generated info -> info.simple_name
        in
        let kind_str =
          match node.file_kind with
          | Interface -> ".mli"
          | Implementation -> ".ml"
          | Alias -> ""
        in
        let dep_names =
          List.map
            (fun dep ->
              let dep_name =
                match dep.module_info with
                | Mod_tree.Concrete info -> info.namespaced_name
                | Mod_tree.Generated info -> info.simple_name
              in
              let dep_kind =
                match dep.file_kind with
                | Interface -> ".mli"
                | Implementation -> ".ml"
                | Alias -> ""
              in
              dep_name ^ dep_kind)
            node.dependencies
        in
        Format.eprintf "  %s%s -> [%s]@.%!" name kind_str
          (String.concat ", " dep_names))
    graph.nodes;

  (* Add structural dependencies *)
  (* Folder interfaces depend on all modules in that folder *)
  Hashtbl.iter
    (fun _id node ->
      match node.module_info with
      | Mod_tree.Concrete { simple_name; namespaced_name; _ } ->
          (* Check if this is a folder interface (e.g., Data in data/) *)
          if simple_name <> namespaced_name then
            (* This is a namespaced module, check if there's a folder interface *)
            let prefix =
              String.sub namespaced_name 0
                (String.length namespaced_name - String.length simple_name - 2)
            in

            (* If this module's namespace matches a folder interface, add dependency *)
            Hashtbl.iter
              (fun other_id other_node ->
                match other_node.module_info with
                | Mod_tree.Concrete other_info
                  when other_info.namespaced_name = prefix ->
                    (* The folder interface depends on this module *)
                    if not (DepId.eq other_id node.id) then
                      add_dependency graph ~source:other_node ~target:node
                | _ -> ())
              graph.nodes
      | _ -> ())
    graph.nodes;

  (* Add dependencies FROM modules TO their alias modules *)
  (* Modules that have parent_aliases should depend on those alias modules *)
  Hashtbl.iter
    (fun _id node ->
      (* For each node, make it depend on its parent alias modules *)
      List.iter
        (fun alias_name ->
          (* Find the alias module by name - alias modules don't have suffix *)
          match find_node graph alias_name with
          | Some alias_node ->
              if alias_node.id <> node.id then (
                let node_name =
                  match node.module_info with
                  | Mod_tree.Concrete info -> info.namespaced_name
                  | Mod_tree.Generated info -> info.simple_name
                in
                let kind_str =
                  match node.file_kind with
                  | Interface -> ".mli"
                  | Implementation -> ".ml"
                  | Alias -> ""
                in
                Format.eprintf "[DEBUG Dep_graph] %s%s depends on alias %s@.%!"
                  node_name kind_str alias_name;
                add_dependency graph ~source:node ~target:alias_node)
          | None ->
              Format.eprintf
                "[DEBUG Dep_graph] Warning: Alias module %s not found@.%!"
                alias_name)
        node.parent_aliases)
    graph.nodes;

  graph

(** Topologically sort the graph *)
let topological_sort graph =
  (* Create a copy of in-degree for each node *)
  let in_degree = Hashtbl.create (Hashtbl.length graph.nodes) in
  let queue = Queue.create () in
  let result = ref [] in

  (* Initialize in-degrees and find nodes with no dependencies *)
  Hashtbl.iter
    (fun id node ->
      let degree = List.length node.dependencies in
      Hashtbl.add in_degree id degree;
      if degree = 0 then Queue.push node queue)
    graph.nodes;

  (* Process nodes in topological order *)
  while not (Queue.is_empty queue) do
    let node = Queue.pop queue in
    result := node :: !result;

    (* Decrease in-degree of dependents *)
    List.iter
      (fun dependent ->
        let dep_id = dependent.id in
        let current_degree = Hashtbl.find in_degree dep_id in
        let new_degree = current_degree - 1 in
        Hashtbl.replace in_degree dep_id new_degree;
        if new_degree = 0 then Queue.push dependent queue)
      node.dependents
  done;

  (* Check for cycles *)
  if List.length !result <> Hashtbl.length graph.nodes then
    Error "Dependency cycle detected"
  else Ok (List.rev !result)

(** Print the graph for debugging *)
let print graph =
  Format.eprintf "Dependency Graph:@.%!";
  Hashtbl.iter
    (fun id node ->
      let name =
        match node.module_info with
        | Mod_tree.Concrete info -> info.namespaced_name
        | Mod_tree.Generated info -> info.simple_name
      in
      let kind_str =
        match node.file_kind with
        | Interface -> ".mli"
        | Implementation -> ".ml"
        | Alias -> ""
      in
      let deps =
        List.map
          (fun dep ->
            let dep_name =
              match dep.module_info with
              | Mod_tree.Concrete info -> info.namespaced_name
              | Mod_tree.Generated info -> info.simple_name
            in
            let dep_kind =
              match dep.file_kind with
              | Interface -> ".mli"
              | Implementation -> ".ml"
              | Alias -> ""
            in
            dep_name ^ dep_kind)
          node.dependencies
      in
      Format.eprintf "  %s%s (level %d) -> [%s]@.%!" name kind_str
        (level_to_int node.level) (String.concat ", " deps))
    graph.nodes

(** Convert dependency graph to a list of actions *)
let to_action_list ?(dep_libraries = []) ?(c_objects = []) graph =
  match topological_sort graph with
  | Error msg -> Error msg
  | Ok sorted_nodes ->
      let actions = ref [] in

      (* Keep track of compiled .cmo files for linking *)
      let cmo_files = ref [] in

      (* Process each node in topological order - complete each module before moving to next *)
      List.iter
        (fun node ->
          match (node.module_info, node.file_kind) with
          | Mod_tree.Generated { simple_name; kind = Static { contents; path }; _ }, Alias ->
              (* Generated alias module *)
              Format.eprintf
                "[DEBUG] Generating actions for alias module: %s@.%!"
                simple_name;

              (* Write the file *)
              let filename = Path.to_string path in
              actions :=
                Actions.WriteFile { destination = filename; content = contents }
                :: !actions;

              (* Create module name for compilation *)
              let impl_path = path
              in
              let modname =
                Mod_name.make ~filename:impl_path
                  ~namespace:(Mod_name.namespace_of_list [])
                  ~name:simple_name
              in

              (* Compile interface from implementation *)
              let cmi_output = Mod_name.cmi modname in
              actions :=
                Actions.CompileInterface
                  {
                    source = filename;
                    output = cmi_output;
                    includes = [ "." ];
                    flags =
                      [
                        Ocamlc.NoAliasDeps;
                        Ocamlc.Impl impl_path;
                        Ocamlc.Warning [ Ocamlc.NoCmiFile ];
                      ];
                  }
                :: !actions;

              (* Compile implementation *)
              let cmo_output = Mod_name.cmo modname in
              actions :=
                Actions.CompileImplementation
                  {
                    source = filename;
                    output = cmo_output;
                    includes = [ "." ];
                    flags =
                      [
                        Ocamlc.NoAliasDeps;
                        Ocamlc.Impl impl_path;
                        Ocamlc.Warning [ Ocamlc.NoCmiFile ];
                      ];
                  }
                :: !actions;
              cmo_files := cmo_output :: !cmo_files
          | ( Mod_tree.Concrete { simple_name; namespaced_name; impl; intf },
              Interface ) -> (
              (* Interface node - process .mli file *)
              Format.eprintf
                "[DEBUG] Generating actions for interface: %s.mli@.%!"
                namespaced_name;

              (* Get open flags for this module *)
              let open_flags =
                List.map (fun alias -> Ocamlc.Open alias) node.parent_aliases
              in

              match intf with
              | Some src ->
                  let original_path = Path.to_string src.Build_node.file in
                  let source_name = Filename.basename original_path in
                  let output = namespaced_name ^ ".cmi" in

                  (* Copy source file *)
                  actions :=
                    Actions.CopyFile
                      { source = original_path; destination = source_name }
                    :: !actions;

                  (* Compile interface *)
                  actions :=
                    Actions.CompileInterface
                      {
                        source = source_name;
                        output;
                        includes = [ "." ];
                        flags = open_flags;
                      }
                    :: !actions
              | None ->
                  (* This shouldn't happen - Interface node without .mli file *)
                  ())
          | ( Mod_tree.Concrete { simple_name; namespaced_name; impl; intf },
              Implementation ) -> (
              (* Implementation node - process .ml file *)
              Format.eprintf
                "[DEBUG] Generating actions for implementation: %s.ml@.%!"
                namespaced_name;

              (* Get open flags for this module *)
              let open_flags =
                List.map (fun alias -> Ocamlc.Open alias) node.parent_aliases
              in

              match impl with
              | Some src ->
                  let original_path = Path.to_string src.Build_node.file in
                  let source_name = Filename.basename original_path in

                  (* Copy source file *)
                  actions :=
                    Actions.CopyFile
                      { source = original_path; destination = source_name }
                    :: !actions;

                  (* No need to explicitly generate .cmi - when we compile the .ml file
                     without a corresponding .mli, ocamlc automatically generates both
                     the .cmi and .cmo files in one compilation *)
                  ();

                  (* Compile implementation *)
                  let cmo_output = namespaced_name ^ ".cmo" in
                  actions :=
                    Actions.CompileImplementation
                      {
                        source = source_name;
                        output = cmo_output;
                        includes = [ "." ];
                        flags = open_flags;
                      }
                    :: !actions;
                  cmo_files := cmo_output :: !cmo_files
              | None ->
                  (* This shouldn't happen - Implementation node without .ml file *)
                  ())
          | _ ->
              (* Other combinations shouldn't occur *)
              ())
        sorted_nodes;

      (* Generate link action if we have .cmo files *)
      (if !cmo_files <> [] then
         (* Determine if this is a binary or library based on package *)
         let safe_package_name =
           String.map
             (fun c -> if c = '-' then '_' else c)
             graph.package.Workspace.name
         in

         (* Check if we have a main module *)
         let has_main =
           List.exists
             (fun action ->
               match action with
               | Actions.CompileImplementation { source; _ } ->
                   String.ends_with ~suffix:"main.ml" source
                   || String.ends_with ~suffix:"Main.ml" source
               | _ -> false)
             !actions
         in

         if has_main then
           (* Binary - use the transitive dependency libraries passed from build_planner *)
           let output = graph.package.Workspace.name in
           (* dep_libraries are already in topological order from the build graph *)
           (* Add unix.cma at the beginning for system dependencies *)
          let libraries = "unix.cma" :: dep_libraries in
           actions :=
             Actions.CreateExecutable
               {
                 output;
                 objects = List.rev !cmo_files;
                 libraries;
                 includes = [ "." ];
               }
             :: !actions
         else
           (* Library *)
           let output = safe_package_name ^ ".cma" in
           actions :=
             Actions.CreateLibrary
               { output; objects = List.rev !cmo_files @ c_objects; includes = [ "." ] }
             :: !actions);

      Ok (List.rev !actions)
