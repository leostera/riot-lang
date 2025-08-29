(** Build graph module - handles dependency graph construction and topological
    sorting *)

open Std
open Build_node

type node = Build_node.t
type t = { nodes : (string, node) Hashtbl.t; root_nodes : node list }

exception Cycle_detected of string list

(** Find a node by package name *)
let find_node t package_name = Hashtbl.find_opt t.nodes package_name

(** Get a node by its ID *)
let get_node t node_id =
  match Hashtbl.find_opt t.nodes (Node_id.to_string node_id) with
  | Some node -> node
  | None ->
      failwith
        (Printf.sprintf
           "FATAL: Node with ID '%s' not found in build graph. This should \
            never happen!"
           (Node_id.to_string node_id))

(** Create a build graph from a workspace *)
let create workspace toolchain =
  let nodes = Hashtbl.create 16 in

  (* First, collect source files for each package *)
  let get_sources package =
    let pkg_path = Std.Path.to_string package.Workspace.path in
    let src_dir =
      if File_utils.exists ~path:(Filename.concat pkg_path "src") then
        Filename.concat pkg_path "src"
      else pkg_path
    in

    let rec scan_directory dir relative_path acc =
      if File_utils.exists ~path:dir then
        let entries =
          Fs.readdir
            (Path.of_string dir |> Result.expect ~msg:"Invalid dir path")
          |> Result.expect ~msg:"Failed to read directory"
        in
        List.fold_left
          (fun acc entry ->
            let full_path = Filename.concat dir entry in
            let rel_path =
              if relative_path = "" then entry
              else Filename.concat relative_path entry
            in
            if
              Fs.is_directory
                (Path.of_string full_path
                |> Result.expect ~msg:"Invalid full_path")
              |> Result.expect ~msg:"Failed to check directory"
            then scan_directory full_path rel_path acc
            else if
              Filename.check_suffix entry ".ml"
              || Filename.check_suffix entry ".mli"
              || Filename.check_suffix entry ".c"
            then
              match Std.Path.of_string (Filename.concat src_dir rel_path) with
              | Ok path -> 
                  (* Extract namespace from relative path for future use *)
                  let namespace = 
                    if relative_path = "" || relative_path = entry then []
                    else 
                      let dir_part = Filename.dirname rel_path in
                      if dir_part = "." then []
                      else String.split_on_char '/' dir_part
                  in
                  (* Determine file kind and create appropriate source *)
                  let kind = 
                    if Filename.check_suffix entry ".ml" || Filename.check_suffix entry ".mli" then
                      (* Get the simple module name *)
                      let simple_name = 
                        let name_without_ext =
                          if Filename.check_suffix entry ".ml" then
                            Filename.chop_suffix entry ".ml"
                          else
                            Filename.chop_suffix entry ".mli"
                        in
                        String.capitalize_ascii name_without_ext
                      in
                      (* Get the namespaced name *)
                      let namespaced_name = 
                        Namespacing.get_module_name 
                          ~package_name:package.Workspace.name 
                          (Std.Path.to_string path)
                      in
                      (* Create the appropriate variant *)
                      if Filename.check_suffix entry ".ml" then
                        Build_node.ML { simple_name; namespaced_name; namespace }
                      else
                        Build_node.MLI { simple_name; namespaced_name; namespace }
                    else if Filename.check_suffix entry ".c" then 
                      Build_node.C_stub
                    else 
                      Build_node.Other (Filename.extension entry)
                  in
                  let source = {
                    Build_node.file = path;
                    kind
                  } in
                  source :: acc
              | Error _ -> acc
            else acc)
          acc entries
      else acc
    in
    scan_directory src_dir "" []
  in

  (* First pass: create all nodes without dependencies *)
  List.iter
    (fun package ->
      let srcs = get_sources package in
      let node =
        {
          Build_node.package;
          toolchain;
          srcs;
          deps = [];
          (* Will be filled in second pass *)
          spec = Unplanned;
        }
      in
      Hashtbl.add nodes package.Workspace.name node)
    workspace.Workspace.packages;

  (* Second pass: link dependencies *)
  List.iter
    (fun package ->
      match Hashtbl.find_opt nodes package.Workspace.name with
      | None -> ()
      | Some node ->
          let dep_ids =
            List.filter_map
              (fun (dep : Workspace.dependency) ->
                (* Skip self-references *)
                if dep.Workspace.name = package.Workspace.name then None
                else
                  (* Just store the ID, not the node *)
                  match Hashtbl.find_opt nodes dep.Workspace.name with
                  | Some dep_node ->
                      Some (Node_id.of_package dep_node.Build_node.package)
                  | None -> None)
              package.dependencies
          in
          (* Mutate the existing node's deps field with IDs *)
          node.Build_node.deps <- dep_ids)
    workspace.packages;

  (* Find root nodes (no dependencies) *)
  let root_nodes =
    Hashtbl.fold
      (fun _ node acc -> if node.Build_node.deps = [] then node :: acc else acc)
      nodes []
  in

  { nodes; root_nodes }

(** Topological sort using Kahn's algorithm *)
let topological_sort graph =
  (* Use in-degree count for each node *)
  let in_degree = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name node ->
      Hashtbl.add in_degree name (List.length node.Build_node.deps))
    graph.nodes;

  (* Start with nodes that have no dependencies *)
  let queue = Queue.create () in
  List.iter (fun node -> Queue.add node queue) graph.root_nodes;

  let sorted = ref [] in

  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    sorted := node :: !sorted;

    (* Find nodes that depend on this one and decrement their in-degree *)
    let node_id = Node_id.of_package node.Build_node.package in
    Hashtbl.iter
      (fun name other_node ->
        if
          List.exists
            (fun dep_id -> Node_id.equal dep_id node_id)
            other_node.Build_node.deps
        then
          match Hashtbl.find_opt in_degree name with
          | None -> ()
          | Some deg ->
              let new_deg = deg - 1 in
              Hashtbl.replace in_degree name new_deg;
              if new_deg = 0 then Queue.add other_node queue)
      graph.nodes
  done;

  (* Check for cycles *)
  if List.length !sorted <> Hashtbl.length graph.nodes then (
    (* Collect nodes that weren't sorted (likely part of cycle) *)
    let cycle_nodes = ref [] in
    Hashtbl.iter
      (fun name node ->
        if not (List.exists (fun n -> n.Build_node.package.name = name) !sorted)
        then cycle_nodes := name :: !cycle_nodes)
      graph.nodes;

    Printf.eprintf
      "Circular dependency detected: sorted %d nodes but graph has %d nodes\n"
      (List.length !sorted)
      (Hashtbl.length graph.nodes);
    List.iter
      (fun name -> Printf.eprintf "  Node '%s' is part of a cycle\n" name)
      !cycle_nodes;

    raise (Cycle_detected !cycle_nodes));

  List.rev !sorted

(** Print the build graph *)
let print graph =
  Printf.printf "\n=== Build Graph ===\n";

  (* Print in topological order *)
  let sorted = topological_sort graph in

  Printf.printf "\nBuild order:\n";
  List.iteri
    (fun i node ->
      Printf.printf "%d. %s" (i + 1) node.package.name;
      if node.Build_node.deps <> [] then
        Printf.printf " (deps: %s)"
          (String.concat ", "
             (List.map
                (fun dep_id -> Node_id.to_string dep_id)
                node.Build_node.deps));
      Printf.printf "\n")
    sorted;

  Printf.printf "\nDependency tree:\n";
  let rec print_tree indent node visited =
    if List.mem node.package.name visited then
      Printf.printf "%s%s (circular reference)\n" indent node.package.name
    else (
      Printf.printf "%s%s\n" indent node.package.name;
      let visited = node.package.name :: visited in
      List.iter
        (fun dep_id ->
          let dep = get_node graph dep_id in
          print_tree (indent ^ "  ") dep visited)
        node.Build_node.deps)
  in

  List.iter (fun node -> print_tree "" node []) graph.root_nodes

(** Create a filtered build graph containing only a package and its dependencies
*)
let size graph = Hashtbl.length graph.nodes

let filter_for_package graph target_pkg_name =
  match Hashtbl.find_opt graph.nodes target_pkg_name with
  | None ->
      failwith
        (Printf.sprintf "Package '%s' not found in workspace" target_pkg_name)
  | Some target_node ->
      (* Collect target and all its transitive dependencies *)
      let rec collect_deps node visited =
        if List.mem node.package.name visited then visited
        else
          let visited = node.package.name :: visited in
          List.fold_left
            (fun acc dep_id ->
              let dep = get_node graph dep_id in
              collect_deps dep acc)
            visited node.Build_node.deps
      in

      let needed_packages = collect_deps target_node [] in

      (* Create new filtered graph *)
      let filtered_nodes = Hashtbl.create 16 in

      (* Add only needed nodes *)
      List.iter
        (fun pkg_name ->
          match Hashtbl.find_opt graph.nodes pkg_name with
          | Some node -> Hashtbl.add filtered_nodes pkg_name node
          | None -> ())
        needed_packages;

      (* Find root nodes in the filtered set *)
      let root_nodes =
        Hashtbl.fold
          (fun _ node acc ->
            (* A node is a root in our filtered graph if none of its dependencies are in the filtered set *)
            let has_deps_in_filtered =
              List.exists
                (fun dep_id ->
                  let dep_name = Node_id.to_string dep_id in
                  List.mem dep_name needed_packages)
                node.Build_node.deps
            in
            if not has_deps_in_filtered then node :: acc else acc)
          filtered_nodes []
      in

      { nodes = filtered_nodes; root_nodes }
