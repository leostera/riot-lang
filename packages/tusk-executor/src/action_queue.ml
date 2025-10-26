open Std
open Std.Collections
open Std.Time
open Tusk_planner

type action_node = Action_node.t

type action_error =
  | ExecutionFailed of { message : string }
  | OutputsNotCreated of { missing : Path.t list }
  | DependenciesFailed of { failed : Graph.SimpleGraph.Node_id.t list }

type action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

type execution_result = {
  node_id : Graph.SimpleGraph.Node_id.t;
  status : action_status;
  duration : Duration.t;
  started_at : Instant.t;
  completed_at : Instant.t;
}

type t = {
  ready_queue : action_node Queue.t;
  later_queue : action_node Queue.t;
  busy_tasks : (Graph.SimpleGraph.Node_id.t, action_node) HashMap.t;
  completed : (Graph.SimpleGraph.Node_id.t, execution_result) HashMap.t;
}

let create () =
  {
    ready_queue = Queue.create ();
    later_queue = Queue.create ();
    busy_tasks = HashMap.create ();
    completed = HashMap.create ();
  }

let is_in_queue queue node_id =
  let found = ref false in
  Queue.iter
    (fun (node : action_node) ->
      if Graph.SimpleGraph.Node_id.eq node.id node_id then found := true)
    queue;
  !found

let dependencies_satisfied t (node : action_node) =
  Log.debug "[ACTION_QUEUE] Checking dependencies for node %s (has %d deps)"
    (Graph.SimpleGraph.Node_id.to_string node.id)
    (List.length node.deps);

  let result =
    List.for_all
      (fun dep_id ->
        match HashMap.get t.completed dep_id with
        | Some { status = Cached _ | Executed; _ } ->
            Log.debug "[ACTION_QUEUE]   dep %s -> SATISFIED"
              (Graph.SimpleGraph.Node_id.to_string dep_id);
            true
        | Some { status = Failed _ | Skipped; _ } ->
            Log.debug "[ACTION_QUEUE]   dep %s -> FAILED"
              (Graph.SimpleGraph.Node_id.to_string dep_id);
            false
        | None ->
            Log.debug "[ACTION_QUEUE]   dep %s -> NOT READY"
              (Graph.SimpleGraph.Node_id.to_string dep_id);
            false)
      node.deps
  in
  Log.debug "[ACTION_QUEUE] Node %s dependencies satisfied: %b"
    (Graph.SimpleGraph.Node_id.to_string node.id)
    result;
  result

let has_failed_dependencies t (node : action_node) =
  List.exists
    (fun dep_id ->
      match HashMap.get t.completed dep_id with
      | Some { status = Failed _ | Skipped; _ } -> true
      | _ -> false)
    node.deps

let queue t (node : action_node) =
  Log.debug "[ACTION_QUEUE] Attempting to queue node %s"
    (Graph.SimpleGraph.Node_id.to_string node.id);

  match HashMap.get t.completed node.id with
  | Some _ ->
      Log.debug "[ACTION_QUEUE]   -> SKIP (already completed)";
      ()
  | None ->
      if Option.is_some (HashMap.get t.busy_tasks node.id) then (
        Log.debug "[ACTION_QUEUE]   -> SKIP (busy)";
        ())
      else if is_in_queue t.ready_queue node.id then (
        Log.debug "[ACTION_QUEUE]   -> SKIP (already in ready queue)";
        ())
      else if is_in_queue t.later_queue node.id then (
        Log.debug "[ACTION_QUEUE]   -> SKIP (already in later queue)";
        ())
      else (
        Log.debug "[ACTION_QUEUE]   -> QUEUED to ready_queue";
        Queue.enqueue t.ready_queue node)

let requeue_with_deps t (node : action_node)
    ~(missing_deps : Graph.SimpleGraph.Node_id.t list)
    ~(all_nodes : action_node list) =
  Log.debug "[ACTION_QUEUE] Requeueing node %s with %d missing deps"
    (Graph.SimpleGraph.Node_id.to_string node.id)
    (List.length missing_deps);

  let _ = HashMap.remove t.busy_tasks node.id in

  if not (is_in_queue t.later_queue node.id) then
    Queue.enqueue t.later_queue node;

  List.iter
    (fun dep_id ->
      Log.debug "[ACTION_QUEUE]   -> Need to queue dep: %s"
        (Graph.SimpleGraph.Node_id.to_string dep_id);
      match
        List.find_opt
          (fun (n : action_node) -> Graph.SimpleGraph.Node_id.eq n.id dep_id)
          all_nodes
      with
      | Some dep_node -> queue t dep_node
      | None ->
          Log.warn "[ACTION_QUEUE]   -> Dep %s not found in graph!"
            (Graph.SimpleGraph.Node_id.to_string dep_id))
    missing_deps

let next t =
  Log.debug "[ACTION_QUEUE] next() called - ready: %d, later: %d, busy: %d"
    (Queue.len t.ready_queue) (Queue.len t.later_queue)
    (HashMap.len t.busy_tasks);

  if Queue.is_empty t.ready_queue && not (Queue.is_empty t.later_queue) then (
    Log.debug "[ACTION_QUEUE] Transferring from later to ready queue";
    let rec transfer () =
      match Queue.dequeue t.later_queue with
      | None -> ()
      | Some node ->
          Queue.enqueue t.ready_queue node;
          transfer ()
    in
    transfer ());

  let rec find_ready checked =
    match Queue.dequeue t.ready_queue with
    | None ->
        List.iter (fun node -> Queue.enqueue t.later_queue node) checked;
        None
    | Some node ->
        if has_failed_dependencies t node then (
          let now = Instant.now () in
          let skip_result =
            {
              node_id = node.id;
              status = Skipped;
              duration = Duration.zero;
              started_at = now;
              completed_at = now;
            }
          in
          let _ = HashMap.insert t.completed node.id skip_result in
          Log.info "[ACTION_QUEUE] Node %s: Skipped (failed dependencies)"
            (Graph.SimpleGraph.Node_id.to_string node.id);

          List.iter (fun n -> Queue.enqueue t.ready_queue n) checked;
          find_ready [])
        else if dependencies_satisfied t node then (
          List.iter (fun n -> Queue.enqueue t.ready_queue n) checked;
          let _ = HashMap.insert t.busy_tasks node.id node in
          Log.debug "[ACTION_QUEUE] Returning node %s (dependencies satisfied)"
            (Graph.SimpleGraph.Node_id.to_string node.id);
          Some node)
        else (
          Log.debug "[ACTION_QUEUE] Node %s not ready, checking next"
            (Graph.SimpleGraph.Node_id.to_string node.id);
          find_ready (node :: checked))
  in
  find_ready []

let mark_completed t result =
  let _ = HashMap.remove t.busy_tasks result.node_id in
  let _ = HashMap.insert t.completed result.node_id result in

  let rec transfer () =
    match Queue.dequeue t.later_queue with
    | None -> ()
    | Some node ->
        Queue.enqueue t.ready_queue node;
        transfer ()
  in
  transfer ()

let mark_failed t ~node_id ~error =
  let _ = HashMap.remove t.busy_tasks node_id in
  let now = Instant.now () in
  let failed_result =
    {
      node_id;
      status = Failed (ExecutionFailed { message = error });
      duration = Duration.zero;
      started_at = now;
      completed_at = now;
    }
  in
  let _ = HashMap.insert t.completed node_id failed_result in
  ()

let get_result t node_id = HashMap.get t.completed node_id

let stats t =
  let ready = Queue.len t.ready_queue in
  let later = Queue.len t.later_queue in
  let busy = HashMap.len t.busy_tasks in
  let completed = HashMap.len t.completed in

  let failed =
    HashMap.fold
      (fun _ result acc ->
        match result.status with Failed _ | Skipped -> acc + 1 | _ -> acc)
      t.completed 0
  in
  let succeeded =
    HashMap.fold
      (fun _ result acc ->
        match result.status with Cached _ | Executed -> acc + 1 | _ -> acc)
      t.completed 0
  in

  (ready, later, busy, completed, succeeded, failed)

let is_complete t ~total_nodes =
  let ready, later, busy, completed, _, _ = stats t in
  let all_queues_empty = ready = 0 && later = 0 && busy = 0 in
  let all_accounted_for = completed = total_nodes in
  all_queues_empty && all_accounted_for
