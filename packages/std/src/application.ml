open Global
open Collections
open Iter

(** Application - Supervision tree management with dependency resolution *)
type t = {
  name: string;
  deps: t list;
  start: unit -> (Pid.t, exn) result;
  stop: Pid.t -> unit;
}

(* Build dependency graph using SimpleGraph *)

let build_dep_graph = fun apps ->
  let graph = Graph.SimpleGraph.make () in
  (* Build a mapping from app to node using the graph itself *)
  let rec add_nodes_and_deps app visited_apps =
    match HashMap.get visited_apps ~key:app.name with
    | Some node -> node
    | None ->
        (* Add the node for this app *)
        let node = Graph.SimpleGraph.add_node graph app in
        let _ = HashMap.insert visited_apps ~key:app.name ~value:node in
        (* Recursively add dependencies and create edges *)
        List.for_each
          app.deps
          ~fn:(fun dep ->
            let dep_node = add_nodes_and_deps dep visited_apps in
            Graph.SimpleGraph.add_edge node ~depends_on:dep_node);
        node
  in
  let visited_apps = HashMap.create () in
  List.for_each
    apps
    ~fn:(fun app ->
      let _ = add_nodes_and_deps app visited_apps in
      ());
  graph

(* Start applications in dependency order *)

let start_applications = fun apps ->
  let graph = build_dep_graph apps in
  match Graph.SimpleGraph.topo_sort graph with
  | Error _ids -> Error (Failure "Circular dependency detected in applications")
  | Ok sorted_nodes ->
      let started = Vector.create () in
      let rec start_all = fun __tmp1 ->
        match __tmp1 with
        | [] ->
            Ok (
              Vector.iter started
              |> Iterator.to_list
            )
        | node :: rest ->
            let app = Graph.SimpleGraph.value node in
            (
              match app.start () with
              | Ok pid ->
                  Vector.push started ~value:(app.name, pid, app);
                  start_all rest
              | Error e ->
                  (* Rollback: stop all started apps in reverse order *)
                  Vector.for_each started ~fn:(fun (_, pid, a) -> a.stop pid);
                  Error e
            )
      in
      match start_all sorted_nodes with
      | Ok apps_with_pids ->
          (* Strip the app from the tuple *)
          Ok (List.map apps_with_pids ~fn:(fun (name, pid, _) -> (name, pid)))
      | Error e -> Error e
