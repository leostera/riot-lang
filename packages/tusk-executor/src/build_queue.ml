open Std
open Std.Collections
open Tusk_model
open Tusk_planner

type package_node = Package_graph.package_node Graph.SimpleGraph.node

type t = {
  ready_queue : package_node Queue.t;
  later_queue : package_node Queue.t;
  busy_tasks : (Package.key, package_node) HashMap.t;
  completed : (Package.key, Package_builder.build_result) HashMap.t;
  mutable package_graph : Package_graph.t option;
}

let create () =
  {
    ready_queue = Queue.create ();
    later_queue = Queue.create ();
    busy_tasks = HashMap.create ();
    completed = HashMap.create ();
    package_graph = None;
  }

let get_package_key (node : package_node) = Package_graph.get_key node.value

let set_package_graph t package_graph = t.package_graph <- Some package_graph

let is_in_queue queue node_id =
  let found = ref false in
  Queue.iter
    (fun (node : package_node) ->
      if Graph.SimpleGraph.Node_id.eq node.id node_id then found := true)
    queue;
  !found

let dependencies_satisfied t (node : package_node) =
  match t.package_graph with
  | None -> false
  | Some package_graph ->
      List.for_all
        (fun dep ->
          match HashMap.get t.completed (Package_graph.get_key dep) with
          | Some { status = Package_builder.Built _ | Cached _ | Failed _; _ } -> true
          | _ -> false)
        (Package_graph.get_dependencies_for_node package_graph node)

let queue t (node : package_node) =
  let pkg_key = get_package_key node in
  match HashMap.get t.completed pkg_key with
  | Some { status = Failed _; _ } -> ()
  | _ ->
      if Option.is_some (HashMap.get t.busy_tasks pkg_key) then ()
      else if is_in_queue t.ready_queue node.id then ()
      else if is_in_queue t.later_queue node.id then ()
      else Queue.push t.ready_queue node

let requeue_with_deps t (node : package_node) ~(deps : package_node list) =
  let pkg_key = get_package_key node in
  let _ = HashMap.remove t.busy_tasks pkg_key in
  if not (is_in_queue t.later_queue node.id) then
    Queue.push t.later_queue node;
  List.iter (fun dep -> queue t dep) deps

let next t =
  let rec find_ready checked =
    match Queue.pop t.ready_queue with
    | None ->
        (* Ready queue exhausted, move checked items to later queue *)
        List.iter (fun node -> Queue.push t.later_queue node) checked;
        (* Transfer later to ready and try again if we haven't checked anything yet *)
        if checked = [] && not (Queue.is_empty t.later_queue) then (
          Queue.transfer ~src:t.later_queue ~dst:t.ready_queue;
          find_ready [])
        else None
    | Some node ->
        if dependencies_satisfied t node then (
          (* Dependencies satisfied, return this node *)
          List.iter (fun n -> Queue.push t.ready_queue n) checked;
          let pkg_key = get_package_key node in
          let _ = HashMap.insert t.busy_tasks pkg_key node in
          Some node)
        else
          (* Dependencies not satisfied, keep looking *)
          find_ready (node :: checked)
  in
  find_ready []

let mark_completed t result =
  let pkg_key = result.Package_builder.package_key in
  let _ = HashMap.remove t.busy_tasks pkg_key in
  let _ = HashMap.insert t.completed pkg_key result in
  ()

let mark_failed t (node : package_node) ~error =
  let pkg = Package_graph.get_package node.value in
  let pkg_key = Package_graph.get_key node.value in
  let _ = HashMap.remove t.busy_tasks pkg_key in

  let failed_result =
    Package_builder.
      {
        package_key = pkg_key;
        package = pkg;
        status = Failed (ExecutionFailed { message = error });
        duration = Time.Duration.zero;
      }
  in
  let _ = HashMap.insert t.completed pkg_key failed_result in
  ()

let stats queue =
  let ready = Queue.len queue.ready_queue in
  let waiting = Queue.len queue.later_queue in
  let busy = HashMap.len queue.busy_tasks in
  let completed = HashMap.len queue.completed in

  let failed =
    queue.completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter ~fn:(fun (_, result) ->
           match result.Package_builder.status with
           | Failed _ -> true
           | _ -> false)
    |> Iter.Iterator.count
  in
  let succeeded =
    queue.completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter ~fn:(fun (_, result) ->
           match result.Package_builder.status with
           | Built _ | Cached _ -> true
           | _ -> false)
    |> Iter.Iterator.count
  in

  (ready, waiting, busy, completed, succeeded, failed)

let is_complete t ~total_packages =
  let ready, waiting, busy, completed, _, _ = stats t in
  let all_queues_empty = ready = 0 && waiting = 0 && busy = 0 in
  let all_accounted_for = completed = total_packages in
  all_queues_empty && all_accounted_for

let get_result queue pkg_key = HashMap.get queue.completed pkg_key

let get_all_results queue =
  queue.completed
  |> HashMap.into_iter
  |> Iter.Iterator.map ~fn:(fun (_, value) -> value)
  |> Iter.Iterator.to_list
