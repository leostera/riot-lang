(** Build queue management - Simple two-queue system *)

type t = {
  ready_queue : Build_node.t Queue.t; (* Tasks ready to build *)
  later_queue : Build_node.t Queue.t; (* Tasks to be consumed later *)
  busy_tasks : (Node_id.t, Build_node.t) Hashtbl.t;
      (* node_id -> currently building node *)
  build_results : Build_results.t; (* Reference to build results *)
}

(** Create a new build queue *)
let create build_results =
  {
    ready_queue = Queue.create ();
    later_queue = Queue.create ();
    busy_tasks = Hashtbl.create 32;
    build_results;
  }

(** Helper: Check if a package is already in a queue *)
let is_in_queue queue pkg_name =
  Queue.fold
    (fun found node -> found || node.Build_node.package.name = pkg_name)
    false queue

(** 1. Queue a task - add to ready queue if not busy/built/queued *)
let queue t node =
  let pkg_name = node.Build_node.package.name in
  let node_id = Node_id.of_package node.Build_node.package in

  (* Check if already built - but still queue for cache reporting *)
  (* Only skip if failed, as failures should not be retried automatically *)
  match Build_results.get_status t.build_results pkg_name with
  | Some (Failed _) -> () (* Failed packages don't get requeued *)
  | _ ->
      (* Check if busy *)
      if Hashtbl.mem t.busy_tasks node_id then ()
        (* Currently building, don't queue *)
      else if is_in_queue t.ready_queue pkg_name then ()
        (* Already in ready queue *)
      else if is_in_queue t.later_queue pkg_name then ()
        (* Already in later queue *)
      else
        (* Add to ready queue - even if already built, to report cache status *)
        Queue.add node t.ready_queue

(** 3. Requeue with deps - put task in later queue and queue all deps *)
let requeue_with_deps t node ~deps =
  let pkg_name = node.Build_node.package.name in
  let node_id = Node_id.of_package node.Build_node.package in

  (* Remove from busy tasks - this task is being requeued *)
  Hashtbl.remove t.busy_tasks node_id;

  (* Add task to later queue *)
  if not (is_in_queue t.later_queue pkg_name) then Queue.add node t.later_queue;

  (* Queue all dependencies *)
  Std.Log.debug "[BUILD_QUEUE] Queueing %d dependencies for %s"
    (List.length deps) pkg_name;
  List.iter
    (fun dep ->
      Std.Log.debug "[BUILD_QUEUE]   -> Queueing dep: %s"
        dep.Build_node.package.name;
      queue t dep)
    deps

(** Compatibility alias *)
let queue_with_deps = requeue_with_deps

(** Helper: Check if all dependencies of a node are built *)
let dependencies_satisfied t node =
  List.for_all
    (fun dep_id ->
      let dep_name = Node_id.to_string dep_id in
      match Build_results.get_status t.build_results dep_name with
      | Some (Build_results.Built _) -> true
      | _ -> false)
    node.Build_node.deps

(** 2. Next - get next ready task, checking dependencies before returning *)
let next t =
  (* Transfer any waiting tasks to ready queue *)
  if Queue.is_empty t.ready_queue && not (Queue.is_empty t.later_queue) then
    Queue.transfer t.later_queue t.ready_queue;

  (* Check ready queue for a task with satisfied dependencies *)
  let rec find_ready checked =
    if Queue.is_empty t.ready_queue then (
      (* No more tasks in ready queue - put checked items back in later queue *)
      List.iter (fun node -> Queue.add node t.later_queue) checked;
      None)
    else
      let node = Queue.take t.ready_queue in
      let pkg_name = node.Build_node.package.name in

      if dependencies_satisfied t node then (
        (* Found a task with satisfied dependencies - put checked items back and return *)
        List.iter (fun n -> Queue.add n t.ready_queue) checked;
        let node_id = Node_id.of_package node.Build_node.package in
        Hashtbl.add t.busy_tasks node_id node;
        Std.Log.debug "[BUILD_QUEUE] Returning %s (dependencies satisfied)"
          pkg_name;
        Some node)
      else (
        (* Dependencies not satisfied - check next task *)
        Std.Log.debug "[BUILD_QUEUE] %s has unsatisfied deps, checking next"
          pkg_name;
        find_ready (node :: checked))
  in
  find_ready []

(** 4. Mark as completed - remove from busy and mark in build results *)
let mark_as_completed t node ~artifact =
  let node_id = Node_id.of_package node.Build_node.package in
  (* Remove from busy tasks *)
  Hashtbl.remove t.busy_tasks node_id;
  (* Mark as completed in build results *)
  Build_results.mark_completed t.build_results node artifact;

  (* Move everything from later queue back to ready queue for re-checking *)
  Queue.transfer t.later_queue t.ready_queue

(** 5. Mark as failed - remove from busy and mark in build results *)
let mark_as_failed t node ~error =
  let node_id = Node_id.of_package node.Build_node.package in
  (* Remove from busy tasks *)
  Hashtbl.remove t.busy_tasks node_id;
  (* Mark as failed in build results *)
  Build_results.mark_failed t.build_results node ~error

(** Compatibility - old mark_done just removes from busy *)
let mark_done t node =
  let node_id = Node_id.of_package node.Build_node.package in
  Hashtbl.remove t.busy_tasks node_id

(** Get queue statistics *)
let get_stats t =
  let ready = Queue.length t.ready_queue in
  let later = Queue.length t.later_queue in
  let busy = Hashtbl.length t.busy_tasks in
  (ready, later, busy)
