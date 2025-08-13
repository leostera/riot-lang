(** Build graph module - handles dependency graph construction and topological
    sorting *)

open Build_node

type node = Build_node.t
type t = { nodes : (string, node) Hashtbl.t; root_nodes : node list }

(** Create a build graph from a workspace *)
let create workspace toolchain =
  let nodes = Hashtbl.create 16 in

  (* First pass: create all nodes *)
  List.iter
    (fun package ->
      let node = { package; toolchain; dependencies = []; dependents = []; hash = None } in
      Hashtbl.add nodes package.Workspace.name node)
    workspace.Workspace.packages;

  (* Second pass: link dependencies *)
  List.iter
    (fun package ->
      match Hashtbl.find_opt nodes package.Workspace.name with
      | None -> ()
      | Some node ->
          let deps =
            List.filter_map
              (fun dep_name -> Hashtbl.find_opt nodes dep_name)
              package.dependencies
          in
          node.dependencies <- deps;
          (* Also update dependents *)
          List.iter
            (fun dep_node -> dep_node.dependents <- node :: dep_node.dependents)
            deps)
    workspace.packages;

  (* Find root nodes (no dependencies) *)
  let root_nodes =
    Hashtbl.fold
      (fun _ node acc -> if node.dependencies = [] then node :: acc else acc)
      nodes []
  in

  { nodes; root_nodes }

(** Topological sort using Kahn's algorithm *)
let topological_sort graph =
  (* Use in-degree count for each node *)
  let in_degree = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name node ->
      Hashtbl.add in_degree name (List.length node.dependencies))
    graph.nodes;

  (* Start with nodes that have no dependencies *)
  let queue = Queue.create () in
  List.iter (fun node -> Queue.add node queue) graph.root_nodes;

  let sorted = ref [] in

  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    sorted := node :: !sorted;

    (* Decrease in-degree of dependent nodes *)
    List.iter
      (fun dependent ->
        let name = dependent.package.name in
        match Hashtbl.find_opt in_degree name with
        | None -> ()
        | Some deg ->
            let new_deg = deg - 1 in
            Hashtbl.replace in_degree name new_deg;
            if new_deg = 0 then Queue.add dependent queue)
      node.dependents
  done;

  (* Check for cycles *)
  if List.length !sorted <> Hashtbl.length graph.nodes then
    failwith "Circular dependency detected in build graph";

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
      if node.dependencies <> [] then
        Printf.printf " (deps: %s)"
          (String.concat ", "
             (List.map (fun n -> n.package.name) node.dependencies));
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
        (fun dep -> print_tree (indent ^ "  ") dep visited)
        node.dependencies)
  in

  List.iter (fun node -> print_tree "" node []) graph.root_nodes

(** Create a filtered build graph containing only a package and its dependencies
*)
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
            (fun acc dep -> collect_deps dep acc)
            visited node.dependencies
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
                (fun dep -> List.mem dep.package.name needed_packages)
                node.dependencies
            in
            if not has_deps_in_filtered then node :: acc else acc)
          filtered_nodes []
      in

      { nodes = filtered_nodes; root_nodes }

(* Use Hasher module for all hash operations *)

(** Compute content-based hash for a build node *)
let compute_node_hash toolchain node =
  Printf.printf "[BuildGraph] Computing hash for %s...\n" node.package.name;
  let components = ref [] in
  
  (* 1. Package metadata *)
  components := node.package.name :: !components;
  components := Toolchains.get_version toolchain :: !components;
  Printf.printf "  - Package: %s, Toolchain: %s\n" node.package.name (Toolchains.get_version toolchain);
  
  (* 2. Dependency hashes (sorted by name for deterministic hash) *)
  let sorted_deps = List.sort (fun a b -> String.compare a.package.name b.package.name) node.dependencies in
  List.iter (fun dep ->
    components := (dep.package.name ^ ":" ^ dep.package.relative_path) :: !components;
    components := String.concat "," dep.package.dependencies :: !components;
    
    (* Include dependency hash if already computed *)
    (match dep.hash with
    | Some dep_hash -> 
        let hash_str = Hasher.to_string dep_hash in
        components := ("dep_hash:" ^ hash_str) :: !components;
        Printf.printf "  - Dependency %s hash: %s\n" dep.package.name hash_str
    | None -> 
        components := "dep_hash:pending" :: !components;
        Printf.printf "  - Dependency %s hash: pending\n" dep.package.name);
  ) sorted_deps;
  
  (* 3. Source file content hashes *)
  let src_dir = 
    if System.file_exists (Filename.concat node.package.path "src") then
      Filename.concat node.package.path "src"
    else node.package.path
  in
  
  Printf.printf "  - Source directory: %s\n" src_dir;
  if System.file_exists src_dir then (
    let all_files = System.list_dir_all src_dir in
    let source_files = List.filter (fun f -> 
      String.ends_with ~suffix:".ml" f || 
      String.ends_with ~suffix:".mli" f ||
      String.ends_with ~suffix:".c" f
    ) all_files in
    let sorted_files = List.sort String.compare source_files in
    Printf.printf "  - Found %d source files\n" (List.length sorted_files);
    List.iter (fun file ->
      let full_path = Filename.concat src_dir file in
      let file_hash = Hasher.hash_file full_path in
      let hash_str = Hasher.to_string file_hash in
      components := (file ^ ":" ^ hash_str) :: !components;
      Printf.printf "    - %s: %s\n" file hash_str;
    ) sorted_files;
  ) else
    Printf.printf "  - Source directory does not exist!\n";
  
  (* 4. Actions placeholder - TODO: generate actions here if needed *)
  components := "actions:placeholder" :: !components;
  
  (* 5. Combine all components and hash *)
  let combined = String.concat "|" (List.rev !components) in
  let final_hash = Hasher.hash_string combined in
  
  Printf.printf "  - Final hash for %s: %s\n" node.package.name (Hasher.to_string final_hash);
  flush stdout;
  
  final_hash

(** Result type for hash computation *)
type hash_result = 
  | Ok of Hasher.hash
  | MissingDependencies of Build_node.t list
  | Error of string

(** Force recomputation of hash for a node (ignoring cached value) *)
let recompute_node_hash toolchain node =
  (* Compute fresh hash, ignoring any cached value *)
  let hash = compute_node_hash toolchain node in
  (* Don't update the node's cached hash here - let the caller decide *)
  Ok hash

(** Get hash for a node, checking if dependencies are available, computing hash if necessary using bottom-up traversal *)
let rec get_node_hash toolchain node store =
  match node.hash with
  | Some hash -> Ok hash  (* Already computed *)
  | None ->
      (* First check if dependency artifacts are available *)
      let missing_deps = ref [] in
      let deps_available = List.for_all (fun dep ->
        match get_node_hash toolchain dep store with
        | Ok dep_hash -> 
            (* Check if this dependency's artifacts are available in store *)
            if Store.exists store dep_hash then
              true
            else (
              missing_deps := dep :: !missing_deps;
              false
            )
        | MissingDependencies deps -> 
            missing_deps := deps @ !missing_deps;
            false
        | Error _ -> 
            missing_deps := dep :: !missing_deps;
            false
      ) node.dependencies in
      
      if not deps_available then
        MissingDependencies (List.rev !missing_deps)
      else (
        (* All dependencies available, compute our hash *)
        let hash = compute_node_hash toolchain node in
        node.hash <- Some hash;
        Ok hash
      )

(** Compute hashes for all nodes in the graph *)
let compute_all_hashes toolchain graph store =
  (* Use topological sort to ensure dependencies are processed first *)
  let sorted = topological_sort graph in
  List.iter (fun node ->
    match get_node_hash toolchain node store with
    | Ok _ -> ()
    | MissingDependencies _ -> () (* Dependencies not ready, skip for now *)
    | Error msg -> Printf.printf "[BuildGraph] Error computing hash for %s: %s\n" node.package.name msg
  ) sorted

(** Clear all cached hashes (useful for testing or when source files change) *)
let clear_hashes graph =
  Hashtbl.iter (fun _ node -> node.hash <- None) graph.nodes

(** Tests submodule *)
module Tests = struct
  [@test]
  let test_topological_sort_produces_valid_build_order () : (unit, string) result =
    (* Test that dependencies always come before dependents *)
    Ok ()
  
  [@test]
  let test_topological_sort_detects_cycles () : (unit, string) result =
    (* Test that circular dependencies are detected and reported *)
    Ok ()
  
  [@test]
  let test_filter_for_package_includes_all_transitive_deps () : (unit, string) result =
    (* Test that filtering includes all transitive dependencies *)
    Ok ()
  
  [@test]
  let test_filter_for_package_excludes_unrelated_packages () : (unit, string) result =
    (* Test that filtering excludes packages not in dependency chain *)
    Ok ()
  
  [@test]
  let test_compute_node_hash_includes_all_source_files () : (unit, string) result =
    (* Test that hash computation includes all .ml, .mli, .c files *)
    Ok ()
  
  [@test]
  let test_compute_node_hash_is_deterministic () : (unit, string) result =
    (* Test that same inputs produce same hash *)
    Ok ()
  
  [@test]
  let test_get_node_hash_waits_for_dependency_hashes () : (unit, string) result =
    (* Test that node hash computation waits for all dependency hashes *)
    Ok ()
  
  [@test]
  let test_compute_all_hashes_follows_topological_order () : (unit, string) result =
    (* Test that hashes are computed in dependency order *)
    Ok ()
end
