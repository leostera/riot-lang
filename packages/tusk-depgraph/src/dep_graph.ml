open Std

type node = {
  id : Node_id.t;
  file : string;
  module_name : string;
  namespaced : string;
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

let add_node graph file module_name namespaced =
  let id = Node_id.next () in
  let node = { id; file; module_name; namespaced; deps = [] } in
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
    let src_path = Path.join dir_path (Path.of_string "src" |> Result.expect ~msg:"src") in
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
    |> List.filter (fun d -> d <> "." && d <> "")
  in

  Printf.printf "Found directories: %s\n" (String.concat ", " directories);

  (* Create alias module nodes for each directory including root *)
  let alias_nodes =
    ("." :: directories) |> List.map (fun dir ->
      let alias_name =
        if dir = "." then
          graph.package_name ^ "__aliases"
        else
          (* Convert directory path to module name: data -> Data, data/foo -> Data__Foo *)
          let parts = String.split_on_char '/' dir in
          graph.package_name ^ "__" ^
          (parts |> List.map String.capitalize_ascii |> String.concat "__") ^
          "__aliases"
      in
      let file_path = alias_name ^ ".ml.gen" in
      let node = add_node graph file_path alias_name alias_name in
      (dir, node)
    )
  in

  (* Make root alias module depend on all subdirectory alias modules *)
  (match List.find_opt (fun (d, _) -> d = ".") alias_nodes with
  | Some (_, root_alias) ->
      List.iter (fun (dir, subdir_alias) ->
        if dir <> "." then
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
      let node = add_node graph file module_name entry.namespaced in

      (* Add dependency on the alias module for this file's directory *)
      let file_dir =
        match Path.of_string file with
        | Error _ -> "."
        | Ok p ->
            let d = Path.dirname p |> Path.to_string in
            if d = "" then "." else d
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
      match Ocamldep.get_deps ~cwd:scan_root ~file:node.file with
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
    graph.nodes

let to_mermaid graph =
  let mermaid = Graph.Mermaid.create ~direction:Graph.Mermaid.TD () in

  (* Add nodes *)
  let mermaid =
    Hashtbl.fold
      (fun _id (node : node) acc ->
        let shape =
          if String.ends_with ~suffix:".mli" node.file then
            Graph.Mermaid.Subroutine (* Interface files use double brackets *)
          else Graph.Mermaid.Rectangle
          (* Implementation files use regular rectangles *)
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
          if String.ends_with ~suffix:".mli" node.file then
            [
              ("color", "blue"); ("style", "filled"); ("fillcolor", "lightblue");
            ]
          else
            [
              ("color", "green");
              ("style", "filled");
              ("fillcolor", "lightgreen");
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

  List.rev !sorted
