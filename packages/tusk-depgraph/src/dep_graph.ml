open Std

(** Constants *)
let ml_gen_extension = ".ml.gen"
let aliases_suffix = "__aliases"
let src_dir = "src"
let current_dir = "."

type file_kind =
  | ML         (** .ml file *)
  | MLI        (** .mli file *)
  | C          (** .c file *)
  | H          (** .h file *)
  | Other of string  (** Other file extensions *)

type node_kind =
  | File       (** Concrete file on disk *)
  | Generated  (** To be generated *)

type node = {
  id : Node_id.t;
  file : string;
  module_name : string;
  namespaced : string;
  file_kind : file_kind;
  node_kind : node_kind;
  mutable deps : Node_id.t list;
}

type t = {
  nodes : (int, node) Hashtbl.t;
  registry : Module_registry.t;
  package_name : string;
}

let create ~package_name =
  let registry = Module_registry.create ~package_name in
  { nodes = Hashtbl.create 100; registry; package_name }

let add_node graph file module_name namespaced file_kind node_kind =
  let id = Node_id.next () in
  let node = { id; file; module_name; namespaced; file_kind; node_kind; deps = [] } in
  Hashtbl.add graph.nodes (Node_id.to_int id) node;
  node

let find_node_by_file graph file =
  Hashtbl.fold
    (fun _ node acc ->
      match acc with
      | Some _ -> acc
      | None -> if node.file = file then Some node else None)
    graph.nodes None

let build graph dir =
  (* Look for src directory under package root *)
  let scan_root =
    let dir_path = Path.of_string dir |> Result.expect ~msg:"Invalid directory path" in
    let src_path = Path.join dir_path (Path.of_string src_dir |> Result.expect ~msg:src_dir) in
    let has_src = Fs.is_directory src_path |> Result.expect ~msg:"Failed to check src directory" in
    if has_src then Path.to_string src_path else dir
  in

  Printf.printf "Scanning from: %s\n" scan_root;

  (* First pass: Recursively scan for all source files *)
  let scan_result = File_scanner.scan ~root:scan_root in
  let ocaml_files = File_scanner.ocaml_source_files scan_result in
  let source_files = List.map (fun f -> File_scanner.(f.path)) ocaml_files in

  Printf.printf "Found %d OCaml source files\n" (List.length source_files);

  (* Extract unique directories from source files *)
  let directories =
    source_files
    |> List.map (fun file ->
        match Path.of_string file with
        | Error _ -> ""
        | Ok p -> Path.dirname p |> Path.to_string)
    |> List.sort_uniq String.compare
    |> List.filter (fun d -> d <> current_dir && d <> "")
  in

  Printf.printf "Found directories: %s\n" (String.concat ", " directories);

  (* Create alias module nodes for each directory including root *)
  let alias_nodes =
    (current_dir :: directories) |> List.map (fun dir ->
      let alias_name =
        if dir = current_dir then
          graph.package_name ^ aliases_suffix
        else
          (* Convert directory path to module name: data -> Data, data/foo -> Data__Foo *)
          let parts = String.split_on_char '/' dir in
          graph.package_name ^ Module_registry.namespace_separator ^
          (parts |> List.map String.capitalize_ascii |> String.concat Module_registry.namespace_separator) ^
          aliases_suffix
      in
      let file_path = alias_name ^ ml_gen_extension in
      let node = add_node graph file_path alias_name alias_name ML Generated in
      (dir, node)
    )
  in

  (* Make root alias module depend on all subdirectory alias modules *)
  (match List.find_opt (fun (d, _) -> d = current_dir) alias_nodes with
  | Some (_, root_alias) ->
      List.iter (fun (dir, subdir_alias) ->
        if dir <> current_dir then
          root_alias.deps <- subdir_alias.id :: root_alias.deps
      ) alias_nodes
  | None -> ());

  (* Create nodes and register modules *)
  List.iter
    (fun file ->
      (* Create registry entry *)
      let entry = Module_registry.entry_from_file graph.registry file in

      (* Add to graph *)
      let module_name = Module_registry.module_name_from_path file in
      let path = Path.of_string file |> Result.expect ~msg:"Invalid file path" in
      let extension = Path.extension path |> Option.value ~default:"" in
      let file_kind =
        match extension with
        | ".mli" -> MLI
        | ".ml" -> ML
        | ".c" -> C
        | ".h" -> H
        | ext -> Other ext
      in
      let node_kind =
        if extension = ml_gen_extension then Generated
        else File
      in
      let node = add_node graph file module_name entry.namespaced file_kind node_kind in

      (* Register in module registry *)
      Module_registry.register graph.registry entry;

      (* Add dependency on the alias module for this file's directory *)
      let file_dir =
        match Path.of_string file with
        | Error _ -> current_dir
        | Ok p ->
            let d = Path.dirname p |> Path.to_string in
            if d = "" then current_dir else d
      in

      (* Find the corresponding alias node *)
      match List.find_opt (fun (d, _) -> d = file_dir) alias_nodes with
      | Some (_, alias_node) ->
          (* This module depends on its directory's alias module *)
          node.deps <- alias_node.id :: node.deps
      | None -> ()
    )
    source_files;

  Printf.printf "Created %d nodes\n" (Hashtbl.length graph.nodes);

  (* Second pass: Find dependencies using ocamldep *)
  Printf.printf "Analyzing dependencies...\n";
  Hashtbl.iter
    (fun _id node ->
      (* Determine which alias module to open for this file *)
      let open_modules =
        let file_dir =
          match Path.of_string node.file with
          | Error _ -> ""
          | Ok p ->
              let d = Path.dirname p |> Path.to_string in
              if d = "." || d = "" then "" else d
        in
        (* Find the alias module for this file's directory *)
        match List.find_opt (fun (d, _) -> d = file_dir) alias_nodes with
        | Some (_, alias_node) -> [ alias_node.namespaced ]
        | None ->
            (* If no specific directory alias, use root alias if it exists *)
            (match List.find_opt (fun (d, _) -> d = current_dir) alias_nodes with
             | Some (_, root_alias) -> [ root_alias.namespaced ]
             | None -> [])
      in

      match Ocamldep.get_deps ~cwd:scan_root ~file:node.file ~open_modules () with
      | Some line ->
          let deps = Ocamldep.parse_deps line in
          Printf.printf "  %s depends on: %s\n" node.file
            (String.concat ", " deps);

          (* Find corresponding nodes for each dependency *)
          List.iter
            (fun dep_name ->
              (* Look up in registry *)
              let dep_entries =
                Module_registry.find_by_simple_name graph.registry dep_name
              in
              List.iter
                (fun dep_entry ->
                  (* Find the node for this entry *)
                  match
                    find_node_by_file graph Module_registry.(dep_entry.file)
                  with
                  | Some dep_node ->
                      if not (Node_id.eq dep_node.id node.id) then
                        (* Don't add self-dependencies *)
                        node.deps <- dep_node.id :: node.deps
                  | None ->
                      (* Might be a stdlib module or external dependency *)
                      ())
                dep_entries)
            deps
      | None -> Printf.printf "  %s: (no dependencies detected)\n" node.file)
    graph.nodes;

  (* Third pass: Add library interface dependencies *)
  (* For each directory, if there's a module with the same name as the directory,
     it's the library interface and should depend on all other modules in that directory *)
  List.iter (fun dir ->
    if dir <> current_dir then (* Skip root directory *)
      let dir_name = Filename.basename dir in
      let library_interface_files = [
        dir ^ "/" ^ dir_name ^ ".ml";
        dir ^ "/" ^ dir_name ^ ".mli";
      ] in

      (* Find all other modules in this directory first *)
      let dir_modules = Hashtbl.fold (fun _ node acc ->
        (* Check if this node is in the current directory *)
        let node_dir =
          match Path.of_string node.file with
          | Error _ -> ""
          | Ok p ->
              let d = Path.dirname p |> Path.to_string in
              if d = "." then "" else d
        in
        (* Include modules in this directory, excluding:
           - The library interface files themselves
           - Generated alias modules *)
        if node_dir = dir &&
           not (List.mem node.file library_interface_files) &&
           node.node_kind <> Generated then
          node :: acc
        else
          acc
      ) graph.nodes [] in

      (* Find or create library interface module(s) *)
      let library_interfaces =
        (* Check if interface already exists *)
        let existing = List.filter_map (fun file ->
          find_node_by_file graph file
        ) library_interface_files in

        (* If no interface exists but directory has modules, create one *)
        if existing = [] && dir_modules <> [] then
          (* Create a generated library interface .ml file *)
          let interface_file = dir ^ "/" ^ dir_name ^ ".ml" in
          let module_name = String.capitalize_ascii dir_name in
          let namespaced =
            (* Build namespaced name like Tusk__Cli__Cli *)
            let dir_parts = String.split_on_char '/' dir in
            graph.package_name ^ Module_registry.namespace_separator ^
            (dir_parts |> List.map String.capitalize_ascii |> String.concat Module_registry.namespace_separator) ^
            Module_registry.namespace_separator ^ module_name
          in
          let interface_node = add_node graph interface_file module_name namespaced ML Generated in

          (* Register in module registry *)
          let entry = {
            Module_registry.file = interface_file;
            simple_name = module_name;
            namespaced = namespaced;
            kind = Module_registry.ML;
            is_library_interface = true;
          } in
          Module_registry.register graph.registry entry;

          [ interface_node ]
        else
          existing
      in

      (* Make library interface depend on all modules in its directory *)
      List.iter (fun lib_interface ->
        List.iter (fun module_in_dir ->
          if not (Node_id.eq lib_interface.id module_in_dir.id) &&
             not (List.exists (fun dep_id -> Node_id.eq dep_id module_in_dir.id) lib_interface.deps) then
            lib_interface.deps <- module_in_dir.id :: lib_interface.deps
        ) dir_modules
      ) library_interfaces
  ) directories

let to_mermaid graph =
  let mermaid = Graph.Mermaid.create ~direction:Graph.Mermaid.TD () in

  (* Add nodes *)
  let mermaid =
    Hashtbl.fold
      (fun _id (node : node) acc ->
        let shape =
          match node.file_kind with
          | MLI -> Graph.Mermaid.Subroutine (* Interface files use double brackets *)
          | _ -> Graph.Mermaid.Rectangle
          (* Other files use regular rectangles *)
        in
        Graph.Mermaid.add_node acc ~id:(Node_id.to_string node.id) ~label:node.file ~shape ())
      graph.nodes mermaid
  in

  (* Add edges *)
  let mermaid =
    Hashtbl.fold
      (fun _id (node : node) acc ->
        List.fold_left
          (fun acc dep_id ->
            Graph.Mermaid.add_edge acc ~from_node:(Node_id.to_string node.id)
              ~to_node:(Node_id.to_string dep_id) ())
          acc node.deps)
      graph.nodes mermaid
  in

  mermaid

let to_dot graph =
  let dot = Graph.Dot.create ~name:graph.package_name ~style:Graph.Dot.Directed in

  (* Add nodes *)
  let dot =
    Hashtbl.fold
      (fun _id (node : node) acc ->
        let color =
          match node.file_kind, node.node_kind with
          | MLI, File ->
            [
              ("color", "blue"); ("style", "filled"); ("fillcolor", "lightblue");
            ]
          | ML, File ->
            [
              ("color", "green");
              ("style", "filled");
              ("fillcolor", "lightgreen");
            ]
          | _, Generated ->
            [
              ("color", "gray");
              ("style", "filled");
              ("fillcolor", "lightgray");
            ]
          | C, File ->
            [
              ("color", "red");
              ("style", "filled");
              ("fillcolor", "lightyellow");
            ]
          | H, File ->
            [
              ("color", "orange");
              ("style", "filled");
              ("fillcolor", "lightyellow");
            ]
          | Other _, File ->
            [
              ("color", "black");
              ("style", "filled");
              ("fillcolor", "white");
            ]
        in
        Graph.Dot.add_node acc ~id:(Node_id.to_string node.id) ~label:node.file
          ~attrs:color ())
      graph.nodes dot
  in

  (* Add edges *)
  Hashtbl.fold
    (fun _id (node : node) acc ->
      List.fold_left
        (fun acc dep_id ->
          Graph.Dot.add_edge acc ~from_node:(Node_id.to_string node.id)
            ~to_node:(Node_id.to_string dep_id) ())
        acc node.deps)
    graph.nodes dot

let topological_sort graph =
  (* Kahn's algorithm *)
  let in_degree = Hashtbl.create (Hashtbl.length graph.nodes) in

  (* Initialize in-degrees - using int key for in_degree table *)
  Hashtbl.iter (fun int_id _ -> Hashtbl.add in_degree int_id 0) graph.nodes;

  (* Calculate in-degrees *)
  Hashtbl.iter
    (fun _ node ->
      List.iter
        (fun dep_id ->
          let dep_int_id = Node_id.to_int dep_id in
          let count = Hashtbl.find in_degree dep_int_id in
          Hashtbl.replace in_degree dep_int_id (count + 1))
        node.deps)
    graph.nodes;

  (* Find nodes with no incoming edges *)
  let queue = Queue.create () in
  Hashtbl.iter (fun int_id count -> if count = 0 then Queue.add int_id queue) in_degree;

  (* Process queue *)
  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let int_id = Queue.take queue in
    let node = Hashtbl.find graph.nodes int_id in
    sorted := node :: !sorted;

    (* Decrease in-degree of dependent nodes *)
    List.iter
      (fun dep_id ->
        let dep_int_id = Node_id.to_int dep_id in
        let count = Hashtbl.find in_degree dep_int_id in
        let new_count = count - 1 in
        Hashtbl.replace in_degree dep_int_id new_count;
        if new_count = 0 then Queue.add dep_int_id queue)
      node.deps
  done;

  !sorted
