open Global
open Collections
open Iter


(** Application - Supervision tree management with dependency resolution *)

type t = {
  name: string;
  deps: t list;
  start: unit -> (Pid.t, exn) result;
  stop: Pid.t -> unit
}

(* Build dependency graph using SimpleGraph *)
let build_dep_graph apps =
  let graph = Graph.SimpleGraph.make () in
  
  (* Build a mapping from app to node using the graph itself *)
  let rec add_nodes_and_deps app visited_apps =
    
    (* Check if we already processed this app *)
    match HashMap.get visited_apps app.name with
    | Some node -> node
    | None ->
        (* Add the node for this app *)
        let node = Graph.SimpleGraph.add_node graph app in
        let _ = HashMap.insert visited_apps app.name node in
        
        (* Recursively add dependencies and create edges *)
        List.iter (fun dep ->
          let dep_node = add_nodes_and_deps dep visited_apps in
          Graph.SimpleGraph.add_edge node ~depends_on:dep_node
        ) app.deps;
        
        node
  in
  
  let visited_apps = HashMap.create () in
  List.iter (fun app -> ignore (add_nodes_and_deps app visited_apps)) apps;
  graph

(* Start applications in dependency order *)
let start_applications apps =
  let graph = build_dep_graph apps in
  match Graph.SimpleGraph.topo_sort graph with
  | Error _ids -> Error (Failure "Circular dependency detected in applications")
  | Ok sorted_nodes ->
      let started = Vector.create () in
      let rec start_all = function
        | [] -> Ok (Vector.into_iter started |> Iterator.to_list)
        | node :: rest ->
            let app = node.Graph.SimpleGraph.value in
            (match app.start () with
             | Ok pid ->
                 Vector.push started (app.name, pid, app);
                 start_all rest
             | Error e ->
                 (* Rollback: stop all started apps in reverse order *)
                 Vector.iter (fun (_, pid, a) ->
                   a.stop pid
                 ) started;
                 Error e)
      in
      match start_all sorted_nodes with
      | Ok apps_with_pids ->
          (* Strip the app from the tuple *)
          Ok (List.map (fun (name, pid, _) -> (name, pid)) apps_with_pids)
      | Error e -> Error e
