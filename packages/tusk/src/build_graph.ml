(** Build graph module - handles dependency graph construction and topological sorting *)

type node = {
  package : Workspace.package;
  mutable dependencies : node list;
  mutable dependents : node list;
}

type t = {
  nodes : (string, node) Hashtbl.t;
  root_nodes : node list;
}

(** Create a build graph from a workspace *)
let create workspace =
  let nodes = Hashtbl.create 16 in
  
  (* First pass: create all nodes *)
  List.iter (fun package ->
    let node = { 
      package; 
      dependencies = []; 
      dependents = [] 
    } in
    Hashtbl.add nodes package.Workspace.name node
  ) workspace.Workspace.packages;
  
  (* Second pass: link dependencies *)
  List.iter (fun package ->
    match Hashtbl.find_opt nodes package.Workspace.name with
    | None -> ()
    | Some node ->
        let deps = List.filter_map (fun dep_name ->
          Hashtbl.find_opt nodes dep_name
        ) package.dependencies in
        node.dependencies <- deps;
        (* Also update dependents *)
        List.iter (fun dep_node ->
          dep_node.dependents <- node :: dep_node.dependents
        ) deps
  ) workspace.packages;
  
  (* Find root nodes (no dependencies) *)
  let root_nodes = Hashtbl.fold (fun _ node acc ->
    if node.dependencies = [] then node :: acc else acc
  ) nodes [] in
  
  { nodes; root_nodes }

(** Topological sort using Kahn's algorithm *)
let topological_sort graph =
  (* Use in-degree count for each node *)
  let in_degree = Hashtbl.create 16 in
  Hashtbl.iter (fun name node ->
    Hashtbl.add in_degree name (List.length node.dependencies)
  ) graph.nodes;
  
  (* Start with nodes that have no dependencies *)
  let queue = Queue.create () in
  List.iter (fun node -> Queue.add node queue) graph.root_nodes;
  
  let sorted = ref [] in
  
  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    sorted := node :: !sorted;
    
    (* Decrease in-degree of dependent nodes *)
    List.iter (fun dependent ->
      let name = dependent.package.name in
      match Hashtbl.find_opt in_degree name with
      | None -> ()
      | Some deg ->
          let new_deg = deg - 1 in
          Hashtbl.replace in_degree name new_deg;
          if new_deg = 0 then Queue.add dependent queue
    ) node.dependents
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
  List.iteri (fun i node ->
    Printf.printf "%d. %s" (i + 1) node.package.name;
    if node.dependencies <> [] then
      Printf.printf " (deps: %s)" 
        (String.concat ", " (List.map (fun n -> n.package.name) node.dependencies));
    Printf.printf "\n"
  ) sorted;
  
  Printf.printf "\nDependency tree:\n";
  let rec print_tree indent node visited =
    if List.mem node.package.name visited then
      Printf.printf "%s%s (circular reference)\n" indent node.package.name
    else begin
      Printf.printf "%s%s\n" indent node.package.name;
      let visited = node.package.name :: visited in
      List.iter (fun dep ->
        print_tree (indent ^ "  ") dep visited
      ) node.dependencies
    end
  in
  
  List.iter (fun node ->
    print_tree "" node []
  ) graph.root_nodes