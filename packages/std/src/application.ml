open Global


(** Application - Supervision tree management with dependency resolution *)

module rec R : sig
  module type Spec = sig
    val name : string
    val deps : (module R.Spec) list
    val start : unit -> (Miniriot.Pid.t, exn) result
    val stop : Miniriot.Pid.t -> unit
  end
end = struct
  module type Spec = sig
    val name : string
    val deps : (module R.Spec) list
    val start : unit -> (Miniriot.Pid.t, exn) result
    val stop : Miniriot.Pid.t -> unit
  end
end
include R

type t = (module Spec)

(* Build dependency graph using SimpleGraph *)
let build_dep_graph apps =
  let graph = Graph.SimpleGraph.make () in
  
  (* Build a mapping from app to node using the graph itself *)
  let rec add_nodes_and_deps app visited_apps =
    let (module M : Spec) = app in
    
    (* Check if we already processed this app *)
    match Collections.HashMap.get visited_apps M.name with
    | Some node -> node
    | None ->
        (* Add the node for this app *)
        let node = Graph.SimpleGraph.add_node graph app in
        let _ = Collections.HashMap.insert visited_apps M.name node in
        
        (* Recursively add dependencies and create edges *)
        List.iter (fun dep ->
          let dep_node = add_nodes_and_deps dep visited_apps in
          Graph.SimpleGraph.add_edge node ~depends_on:dep_node
        ) M.deps;
        
        node
  in
  
  let visited_apps = Collections.HashMap.create () in
  List.iter (fun app -> ignore (add_nodes_and_deps app visited_apps)) apps;
  graph

(* Start applications in dependency order *)
let start_applications apps =
  let graph = build_dep_graph apps in
  match Graph.SimpleGraph.topo_sort graph with
  | exception (Graph.SimpleGraph.Cycle _ids) ->
      Error (Failure "Circular dependency detected in applications")
  | sorted_nodes ->
      let started = Collections.Vector.create () in
      let rec start_all = function
        | [] -> Ok (Collections.Vector.to_list started)
        | node :: rest ->
            let app = node.Graph.SimpleGraph.value in
            let (module M : Spec) = app in
            (match M.start () with
             | Ok pid ->
                 Collections.Vector.push started (M.name, pid, app);
                 start_all rest
             | Error e ->
                 (* Rollback: stop all started apps in reverse order *)
                 Collections.Vector.iter (fun (_, pid, a) ->
                   let (module A : Spec) = a in
                   A.stop pid
                 ) started;
                 Error e)
      in
      match start_all sorted_nodes with
      | Ok apps_with_pids ->
          (* Strip the app from the tuple *)
          Ok (List.map (fun (name, pid, _) -> (name, pid)) apps_with_pids)
      | Error e -> Error e
