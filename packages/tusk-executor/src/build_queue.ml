open Std
open Std.Collections
open Tusk_model
open Tusk_planner

type package_node = Package_graph.package_node Graph.SimpleGraph.node

type t = {
  ready_queue : package_node Queue.t;
  later_queue : package_node Queue.t;
  busy_tasks : (string, package_node) HashMap.t;
  completed : (string, Package_builder.build_result) HashMap.t;
}

let create () =
  {
    ready_queue = Queue.create ();
    later_queue = Queue.create ();
    busy_tasks = HashMap.create ();
    completed = HashMap.create ();
  }

let get_package_name (node : package_node) =
  let pkg_node = node.value in
  (Package_graph.get_package pkg_node).name

let is_in_queue queue node_id =
  let found = ref false in
  Queue.iter
    (fun (node : package_node) ->
      if Graph.SimpleGraph.Node_id.eq node.id node_id then found := true)
    queue;
  !found

let dependencies_satisfied t (node : package_node) =
  let pkg_node = node.value in
  let pkg = Package_graph.get_package pkg_node in
  List.for_all
    (fun (dep : Package.dependency) ->
      if dep.name = "stdlib" || dep.name = "unix" then true
      else
        match HashMap.get t.completed dep.name with
        | Some { status = Package_builder.Built _ | Cached _ | Failed _; _ } ->
            (* Dependency is done (built, cached, failed, or skipped) - dispatch package to worker.
               The worker/planner will handle marking it as skipped if deps failed. *)
            true
        | _ -> false)
    pkg.dependencies

let queue t (node : package_node) =
  let pkg_name = get_package_name node in
  match HashMap.get t.completed pkg_name with
  | Some { status = Failed _; _ } -> ()
  | _ ->
      if Option.is_some (HashMap.get t.busy_tasks pkg_name) then ()
      else if is_in_queue t.ready_queue node.id then ()
      else if is_in_queue t.later_queue node.id then ()
      else Queue.enqueue t.ready_queue node

let requeue_with_deps t (node : package_node) ~(deps : package_node list) =
  let pkg_name = get_package_name node in
  let _ = HashMap.remove t.busy_tasks pkg_name in
  if not (is_in_queue t.later_queue node.id) then
    Queue.enqueue t.later_queue node;
  List.iter (fun dep -> queue t dep) deps

let next t =
  let rec find_ready checked =
    match Queue.dequeue t.ready_queue with
    | None ->
        (* Ready queue exhausted, move checked items to later queue *)
        List.iter (fun node -> Queue.enqueue t.later_queue node) checked;
        (* Transfer later to ready and try again if we haven't checked anything yet *)
        if checked = [] && not (Queue.is_empty t.later_queue) then (
          Queue.transfer ~src:t.later_queue ~dst:t.ready_queue;
          find_ready [])
        else None
    | Some node ->
        if dependencies_satisfied t node then (
          (* Dependencies satisfied, return this node *)
          List.iter (fun n -> Queue.enqueue t.ready_queue n) checked;
          let pkg_name = get_package_name node in
          let _ = HashMap.insert t.busy_tasks pkg_name node in
          Some node)
        else
          (* Dependencies not satisfied, keep looking *)
          find_ready (node :: checked)
  in
  find_ready []

let mark_completed t result =
  let pkg_name = result.Package_builder.package.name in
  let _ = HashMap.remove t.busy_tasks pkg_name in
  let _ = HashMap.insert t.completed pkg_name result in
  ()

let mark_failed t (node : package_node) ~error =
  let pkg = Package_graph.get_package node.value in
  let pkg_name = pkg.name in
  let _ = HashMap.remove t.busy_tasks pkg_name in

  let failed_result =
    Package_builder.
      {
        package = pkg;
        status = Failed (ExecutionFailed { message = error });
        duration = Time.Duration.zero;
      }
  in
  let _ = HashMap.insert t.completed pkg_name failed_result in
  ()

let stats queue =
  let ready = Queue.len queue.ready_queue in
  let waiting = Queue.len queue.later_queue in
  let busy = HashMap.len queue.busy_tasks in
  let completed = HashMap.len queue.completed in

  let failed =
    HashMap.fold
      (fun _ result acc ->
        match result.Package_builder.status with
        | Failed _ -> acc + 1
        | _ -> acc)
      queue.completed 0
  in
  let succeeded =
    HashMap.fold
      (fun _ result acc ->
        match result.Package_builder.status with
        | Built _ | Cached _ -> acc + 1
        | _ -> acc)
      queue.completed 0
  in

  (ready, waiting, busy, completed, succeeded, failed)

let is_complete t ~total_packages =
  let ready, waiting, busy, completed, _, _ = stats t in
  let all_queues_empty = ready = 0 && waiting = 0 && busy = 0 in
  let all_accounted_for = completed = total_packages in
  all_queues_empty && all_accounted_for

let get_result queue pkg_name = HashMap.get queue.completed pkg_name

let get_all_results queue =
  let results = ref [] in
  HashMap.iter (fun _key value -> results := value :: !results) queue.completed;
  !results
