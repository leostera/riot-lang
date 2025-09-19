open Std

(** Constants *)
let ml_gen_extension = ".ml.gen"
let aliases_suffix = "__aliases"
let src_dir = "src"
let current_dir = "."

type kind =
  | ML of { module_name: string; namespaced: string list; }
  | MLI of { module_name: string; namespaced: string list; }
  | C
  | H
  | Other of string

type file =
  | Concrete of Path.t
  | Generated of { path: Path.t; contents: string }

type node = {
  id: Node_id.t;
  file: file;
  mutable deps : Node_id.t list;
  kind: kind;
}

type t = {
  root : Path.t;
  nodes : (int, node) Hashtbl.t;
  registry : Module_registry.t;
  package_name : string;
}

let add_node graph file kind =
  let id = Node_id.next () in
  let node = { id; file; kind; deps = [] } in
  Hashtbl.add graph.nodes (Node_id.to_int id) node;
  node

let find_node_by_file graph file_path =
  Hashtbl.fold
    (fun _ node acc ->
      match acc with
      | Some _ -> acc
      | None ->
          let node_path = match node.file with
            | Concrete path -> Path.to_string path
            | Generated { path; _ } -> Path.to_string path
          in
          if node_path = file_path then Some node else None)
    graph.nodes None

let make ~root ~package_name =
  let registry = Module_registry.create ~package_name in
  let graph = { root; nodes = Hashtbl.create 100; registry; package_name } in
  graph

(** Get kind from extension and module info *)
let kind_of_extension ext ~module_name ~namespaced =
  match ext with
  | ".mli" -> MLI { module_name; namespaced }
  | ".ml" -> ML { module_name; namespaced }
  | ".c" -> C
  | ".h" -> H
  | other -> Other other

(** Check if kind is an OCaml source file *)
let is_ocaml_source kind =
  match kind with
  | ML _ | MLI _ -> true
  | _ -> false

(** Recursive directory scanning that builds the graph as it goes *)
let rec scan_directory graph ~current_path ~relative_path ~namespace =
  Printf.printf "Scanning: %s (namespace: [%s])\n"
    (Path.to_string current_path)
    (String.concat "; " namespace);

  (* First, collect all entries in the directory *)
  let sources =
    Fs.read_dir current_path
    |> Result.expect ~msg:("Could not read directory: " ^ Path.to_string current_path)
    |> MutIterator.to_list
  in

  (* Separate files and directories *)
  let files, dirs = List.partition (fun entry ->
    let entry_path = Path.join current_path entry in
    let is_dir = Fs.is_directory entry_path |> Result.expect ~msg:("Could not check if directory: " ^ Path.to_string entry_path) in
    not is_dir
  ) sources in

  (* Get library interface node for this directory (always exists for non-root) *)
  let library_interface_node =
    if relative_path = "" then
      None  (* Root directory doesn't need a library interface *)
    else
      let dir_name = Path.basename current_path in
      let module_name = String.capitalize_ascii dir_name in

      (* Check if user provided the library interface file *)
      let user_provided_interface = List.find_opt (fun file ->
        let file_name = Filename.remove_extension (Path.basename file) in
        file_name = dir_name &&
        let ext = Path.extension file |> Option.value ~default:"" in
        ext = ".ml" || ext = ".mli"
      ) files in

      (* Always create the library interface node *)
      let (file, is_generated) = match user_provided_interface with
      | Some interface_file ->
          (* User provided it - create node for the actual file *)
          let interface_path = Path.join current_path interface_file in
          (Concrete interface_path, false)
      | None ->
          (* Generate library interface file *)
          let interface_path = Path.(current_path / v (dir_name ^ ".ml")) in
          let file = Generated {
            path = interface_path;
            contents = Printf.sprintf "(* Auto-generated library interface for %s *)" dir_name
          } in
          (file, true)
      in

      let kind = ML { module_name; namespaced = namespace } in
      let node = add_node graph file kind in

      (* Register in module registry *)
      let entry_data = {
        Module_registry.file = relative_path ^ "/" ^ dir_name ^ ".ml";
        simple_name = module_name;
        namespaced = namespace;
        kind = Module_registry.ML;
        is_library_interface = true;
      } in
      Module_registry.register graph.registry entry_data;

      if is_generated then
        Printf.printf "  Added generated library interface: %s/%s.ml\n" relative_path dir_name
      else
        Printf.printf "  Found user-provided library interface: %s/%s\n" relative_path (Path.basename (match file with Concrete p -> p | Generated { path; _ } -> path));

      Some node
  in

  (* Create alias file for this directory *)
  let alias_node =
    let alias_name = String.concat "__" namespace ^ "__aliases" in
    let alias_path = Path.(current_path / v (alias_name ^ ".ml")) in
    let kind = ML { module_name = alias_name; namespaced = namespace @ ["Aliases"] } in
    let file = Generated {
      path = alias_path;
      contents = "(* Auto-generated aliases *)"
    } in
    let node = add_node graph file kind in
    Printf.printf "  Added alias file: %s\n" (Path.basename alias_path);
    node
  in

  (* Process all files *)
  let file_nodes = List.filter_map (fun entry ->
    let entry_path = Path.join current_path entry in
    let entry_str = Path.to_string entry in
    let entry_relative = if relative_path = "" then entry_str else relative_path ^ "/" ^ entry_str in

    (* Skip if this is the library interface file we already processed *)
    let dir_name = Path.basename current_path in
    let file_name = Filename.remove_extension (Path.basename entry_path) in
    let is_library_interface =
      relative_path <> "" &&
      file_name = dir_name &&
      (Path.extension entry_path |> Option.value ~default:"" |> fun ext -> ext = ".ml" || ext = ".mli") in

    if is_library_interface then
      None  (* Already processed above *)
    else
      let ext = Path.extension entry_path |> Option.value ~default:"" in
      let module_name = String.capitalize_ascii file_name in
      let full_namespaced = namespace @ [module_name] in

      let kind = kind_of_extension ext ~module_name ~namespaced:full_namespaced in
      let file = Concrete entry_path in

      if is_ocaml_source kind then (
        (* Create node for OCaml source file *)
        let node = add_node graph file kind in

        (* Create registry entry *)
        let entry_data = {
          Module_registry.file = entry_relative;
          simple_name = module_name;
          namespaced = full_namespaced;
          kind = (match kind with ML _ -> Module_registry.ML | MLI _ -> Module_registry.MLI | _ -> Module_registry.ML);
          is_library_interface = false;
        } in
        Module_registry.register graph.registry entry_data;

        Printf.printf "  Added OCaml file: %s -> module %s (namespace: [%s])\n"
          entry_relative module_name (String.concat "; " full_namespaced);

        (* Make file depend on alias module *)
        node.deps <- alias_node.id :: node.deps;
        Some node
      ) else (
        (* For non-OCaml files, still create a node but don't register *)
        let node = add_node graph file kind in
        Printf.printf "  Added other file: %s\n" entry_relative;
        Some node
      )
  ) files in

  (* Recursively process subdirectories *)
  let subdir_nodes = List.concat_map (fun dir ->
    let entry_path = Path.join current_path dir in
    let entry_str = Path.to_string dir in
    let entry_relative = if relative_path = "" then entry_str else relative_path ^ "/" ^ entry_str in
    let dir_name = String.capitalize_ascii entry_str in
    let extended_namespace = namespace @ [dir_name] in

    scan_directory graph ~current_path:entry_path ~relative_path:entry_relative ~namespace:extended_namespace
  ) dirs in

  (* Return all nodes created *)
  let all_nodes = file_nodes @ [alias_node] in
  let all_nodes = match library_interface_node with
    | Some n -> n :: all_nodes
    | None -> all_nodes
  in
  all_nodes @ subdir_nodes

let scan ~(root: Path.t) ~(package_name: string) =
  let graph = make ~root ~package_name in

  (* Start scanning from src directory *)
  let src_root = Path.(root / v "src") in
  Printf.printf "Starting scan from: %s\n" (Path.to_string src_root);

  (* First pass: Build the graph with all nodes *)
  let initial_namespace = [String.capitalize_ascii package_name] in
  let nodes = scan_directory graph ~current_path:src_root ~relative_path:"" ~namespace:initial_namespace in
  Printf.printf "Created %d nodes total\n" (List.length nodes);

  (* Second pass: Analyze dependencies using ocamldep *)
  Printf.printf "Analyzing dependencies...\n";
  Hashtbl.iter
    (fun _id node ->
      (* Only analyze OCaml source files *)
      if is_ocaml_source node.kind then (
        (* TODO: Determine open modules based on namespace for better dependency resolution *)
        let open_modules = [] in

        let path = match node.file with
          | Concrete path -> path
          | Generated { path; _ } -> path
        in
        let relative_file = Path.v (Path.basename path) in

        match Ocamldep.get_deps ~cwd:src_root ~file:relative_file ~open_modules () with
        | Some deps_line ->
            let deps = Ocamldep.parse_deps deps_line in
            Printf.printf "  %s depends on: %s\n" (Path.to_string relative_file) (String.concat ", " deps);

            (* Find corresponding nodes for each dependency *)
            List.iter (fun dep_name ->
              let dep_entries = Module_registry.find_by_simple_name graph.registry dep_name in
              List.iter (fun dep_entry ->
                (* Find the node for this registry entry *)
                match find_node_by_file graph dep_entry.Module_registry.file with
                | Some dep_node ->
                    if not (Node_id.eq dep_node.id node.id) then
                      (* Add dependency (avoid self-dependencies) *)
                      node.deps <- dep_node.id :: node.deps
                | None ->
                    (* Dependency not found - might be external or stdlib *)
                    Printf.printf "    Warning: Dependency '%s' not found in graph\n" dep_name
              ) dep_entries
            ) deps
        | None ->
            Printf.printf "  %s: no dependencies detected\n" (Path.to_string relative_file)
      )
    )
    graph.nodes;

  graph

(** Copy over the output functions from the original *)
let to_mermaid graph =
  let mermaid = Graph.Mermaid.create ~direction:Graph.Mermaid.TD () in

  (* Add nodes *)
  let mermaid =
    Hashtbl.fold
      (fun _id (node : node) acc ->
        let shape =
          match node.kind with
          | MLI _ -> Graph.Mermaid.Subroutine (* Interface files use double brackets *)
          | _ -> Graph.Mermaid.Rectangle
          (* Other files use regular rectangles *)
        in
        let label = match node.file with
          | Concrete path -> Path.basename path 
          | Generated { path; _ } -> Path.basename path 
        in
        Graph.Mermaid.add_node acc ~id:(Node_id.to_string node.id) ~label ~shape ())
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
          match node.kind, node.file with
          | MLI _, Concrete _ ->
            [
              ("color", "blue"); ("style", "filled"); ("fillcolor", "lightblue");
            ]
          | ML _, Concrete _ ->
            [
              ("color", "green");
              ("style", "filled");
              ("fillcolor", "lightgreen");
            ]
          | _, Generated _ ->
            [
              ("color", "gray");
              ("style", "filled");
              ("fillcolor", "lightgray");
            ]
          | C, Concrete _ ->
            [
              ("color", "red");
              ("style", "filled");
              ("fillcolor", "lightyellow");
            ]
          | H, Concrete _ ->
            [
              ("color", "orange");
              ("style", "filled");
              ("fillcolor", "lightyellow");
            ]
          | Other _, Concrete _ ->
            [
              ("color", "black");
              ("style", "filled");
              ("fillcolor", "white");
            ]
        in
        let label = match node.file with
          | Concrete path -> Path.basename path 
          | Generated { path; _ } -> Path.basename path 
        in
        Graph.Dot.add_node acc ~id:(Node_id.to_string node.id) ~label
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
