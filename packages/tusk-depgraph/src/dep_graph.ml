open Std

type node = {
  id : int;
  file : string;
  module_name : string;
  namespaced : string;
  mutable deps : int list;
}

type t = {
  nodes : (int, node) Hashtbl.t;
  registry : Module_registry.t;
  package_name : string;
  mutable next_id : int;
}

let create ~package_name registry =
  { nodes = Hashtbl.create 100; registry; package_name; next_id = 0 }

let add_node graph file module_name namespaced =
  let id = graph.next_id in
  graph.next_id <- graph.next_id + 1;
  let node = { id; file; module_name; namespaced; deps = [] } in
  Hashtbl.add graph.nodes id node;
  node

let find_node_by_file graph file =
  Hashtbl.fold
    (fun _ node acc ->
      match acc with
      | Some _ -> acc
      | None -> if node.file = file then Some node else None)
    graph.nodes None

let build graph dir =
  (* First pass: Create nodes for all files *)
  Printf.printf "Scanning directory %s...\n" dir;
  let files = Sys.readdir dir |> Array.to_list in
  let source_files =
    List.filter
      (fun f ->
        String.ends_with ~suffix:".ml" f || String.ends_with ~suffix:".mli" f)
      files
  in

  Printf.printf "Found %d source files\n" (List.length source_files);

  (* Create nodes and register modules *)
  List.iter
    (fun file ->
      let path =
        Path.of_string file
        |> Result.expect ~msg:(Printf.sprintf "Invalid path: %s" file)
      in
      let stem_path = Path.remove_extension path in
      let stem = Path.basename stem_path in
      let ext = Path.extension path in

      let kind =
        match ext with
        | Some ".mli" -> Module_registry.MLI
        | Some ".ml" -> Module_registry.ML
        | Some ext ->
            failwith
              (Printf.sprintf "Unexpected file type: %s (ext=%s)" file ext)
        | None -> failwith (Printf.sprintf "No extension for file: %s" file)
      in

      let module_name = String.capitalize_ascii stem in
      let namespaced = graph.package_name ^ "__" ^ module_name in

      (* Add to graph *)
      let _node = add_node graph file module_name namespaced in

      (* Register in module registry *)
      let entry : Module_registry.entry =
        {
          file;
          simple_name = module_name;
          namespaced;
          kind;
          is_library_interface = false;
          (* TODO: detect library interface modules *)
        }
      in
      Module_registry.register graph.registry entry)
    source_files;

  Printf.printf "Created %d nodes\n" (Hashtbl.length graph.nodes);

  (* Second pass: Find dependencies using ocamldep *)
  Printf.printf "Analyzing dependencies...\n";
  Hashtbl.iter
    (fun _id node ->
      match Ocamldep.get_deps ~cwd:dir ~file:node.file with
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
                    find_node_by_file graph dep_entry.Module_registry.file
                  with
                  | Some dep_node ->
                      if dep_node.id <> node.id then
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
      (fun id node acc ->
        let shape =
          if String.ends_with ~suffix:".mli" node.file then
            Graph.Mermaid.Subroutine (* Interface files use double brackets *)
          else Graph.Mermaid.Rectangle
          (* Implementation files use regular rectangles *)
        in
        Graph.Mermaid.add_node acc ~id:(string_of_int id) ~label:node.file
          ~shape ())
      graph.nodes mermaid
  in

  (* Add edges *)
  let mermaid =
    Hashtbl.fold
      (fun _id node acc ->
        List.fold_left
          (fun acc dep_id ->
            Graph.Mermaid.add_edge acc ~from_node:(string_of_int node.id)
              ~to_node:(string_of_int dep_id) ())
          acc node.deps)
      graph.nodes mermaid
  in

  mermaid

let to_dot graph =
  let dot =
    Graph.Dot.create ~name:graph.package_name ~style:Graph.Dot.Directed
  in

  (* Add nodes *)
  let dot =
    Hashtbl.fold
      (fun _id node acc ->
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
        Graph.Dot.add_node acc ~id:(string_of_int node.id) ~label:node.file
          ~attrs:color ())
      graph.nodes dot
  in

  (* Add edges *)
  Hashtbl.fold
    (fun _id node acc ->
      List.fold_left
        (fun acc dep_id ->
          Graph.Dot.add_edge acc ~from_node:(string_of_int node.id)
            ~to_node:(string_of_int dep_id) ())
        acc node.deps)
    graph.nodes dot

let topological_sort graph =
  (* Kahn's algorithm *)
  let in_degree = Hashtbl.create (Hashtbl.length graph.nodes) in

  (* Initialize in-degrees *)
  Hashtbl.iter (fun id _ -> Hashtbl.add in_degree id 0) graph.nodes;

  (* Calculate in-degrees *)
  Hashtbl.iter
    (fun _ node ->
      List.iter
        (fun dep_id ->
          let count = Hashtbl.find in_degree dep_id in
          Hashtbl.replace in_degree dep_id (count + 1))
        node.deps)
    graph.nodes;

  (* Find nodes with no incoming edges *)
  let queue = Queue.create () in
  Hashtbl.iter (fun id count -> if count = 0 then Queue.add id queue) in_degree;

  (* Process queue *)
  let sorted = ref [] in
  while not (Queue.is_empty queue) do
    let id = Queue.take queue in
    let node = Hashtbl.find graph.nodes id in
    sorted := node :: !sorted;

    (* Decrease in-degree of dependent nodes *)
    List.iter
      (fun dep_id ->
        let count = Hashtbl.find in_degree dep_id in
        let new_count = count - 1 in
        Hashtbl.replace in_degree dep_id new_count;
        if new_count = 0 then Queue.add dep_id queue)
      node.deps
  done;

  List.rev !sorted
