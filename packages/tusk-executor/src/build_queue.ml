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
  Log.debug "[BUILD_QUEUE] Checking dependencies for %s (has %d deps)" pkg.name
    (List.length pkg.dependencies);
  let result =
    List.for_all
      (fun (dep : Package.dependency) ->
        if dep.name = "stdlib" || dep.name = "unix" then (
          Log.debug "[BUILD_QUEUE]   %s -> SKIP (stdlib/unix)" dep.name;
          true)
        else
          match HashMap.get t.completed dep.name with
          | Some { status = Package_builder.Built _ | Cached _; _ } ->
              Log.debug "[BUILD_QUEUE]   %s -> SATISFIED" dep.name;
              true
          | _ ->
              Log.debug "[BUILD_QUEUE]   %s -> NOT SATISFIED" dep.name;
              false)
      pkg.dependencies
  in
  Log.debug "[BUILD_QUEUE] %s dependencies satisfied: %b" pkg.name result;
  result

let queue t (node : package_node) =
  let pkg_name = get_package_name node in

  Log.debug "[BUILD_QUEUE] Attempting to queue %s" pkg_name;
  match HashMap.get t.completed pkg_name with
  | Some { status = Failed _; _ } ->
      Log.debug "[BUILD_QUEUE]   -> SKIP (already failed)";
      ()
  | _ ->
      if Option.is_some (HashMap.get t.busy_tasks pkg_name) then (
        Log.debug "[BUILD_QUEUE]   -> SKIP (busy)";
        ())
      else if is_in_queue t.ready_queue node.id then (
        Log.debug "[BUILD_QUEUE]   -> SKIP (already in ready queue)";
        ())
      else if is_in_queue t.later_queue node.id then (
        Log.debug "[BUILD_QUEUE]   -> SKIP (already in later queue)";
        ())
      else (
        Log.debug "[BUILD_QUEUE]   -> QUEUED to ready_queue";
        Queue.enqueue t.ready_queue node)

let requeue_with_deps t (node : package_node) ~(deps : package_node list) =
  let pkg_name = get_package_name node in

  let _ = HashMap.remove t.busy_tasks pkg_name in

  if not (is_in_queue t.later_queue node.id) then
    Queue.enqueue t.later_queue node;

  Log.debug "[BUILD_QUEUE] Queueing %d dependencies for %s" (List.length deps)
    pkg_name;
  List.iter
    (fun dep ->
      Log.debug "[BUILD_QUEUE]   -> Queueing dep: %s" (get_package_name dep);
      queue t dep)
    deps

let next t =
  let ready_len = Queue.len t.ready_queue in
  let later_len = Queue.len t.later_queue in
  let busy_count = HashMap.len t.busy_tasks in

  Log.debug "[BUILD_QUEUE] next() called - ready: %d, later: %d, busy: %d"
    ready_len later_len busy_count;

  if ready_len < 0 || later_len < 0 then (
    Log.error
      "[BUILD_QUEUE] CORRUPTION DETECTED! ready=%d later=%d - queue lengths \
       cannot be negative!"
      ready_len later_len;
    None)
  else (
    if Queue.is_empty t.ready_queue && not (Queue.is_empty t.later_queue) then (
      Log.debug "[BUILD_QUEUE] Transferring from later to ready queue";
      let rec transfer () =
        match Queue.dequeue t.later_queue with
        | None -> ()
        | Some node ->
            Queue.enqueue t.ready_queue node;
            transfer ()
      in
      transfer ());

    let rec find_ready checked =
      let current_ready_len = Queue.len t.ready_queue in
      Log.debug "[BUILD_QUEUE] find_ready: ready_len=%d checked=%d"
        current_ready_len (List.length checked);

      if current_ready_len < 0 then (
        Log.error "[BUILD_QUEUE] Queue corruption in find_ready! ready_len=%d"
          current_ready_len;
        List.iter (fun node -> Queue.enqueue t.later_queue node) checked;
        None)
      else
        match Queue.dequeue t.ready_queue with
        | None ->
            Log.debug
              "[BUILD_QUEUE] Ready queue empty, moving %d checked items to \
               later"
              (List.length checked);
            List.iter (fun node -> Queue.enqueue t.later_queue node) checked;
            None
        | Some node ->
            let pkg_name = get_package_name node in
            if dependencies_satisfied t node then (
              Log.debug
                "[BUILD_QUEUE] Found ready task: %s, re-enqueuing %d checked \
                 items"
                pkg_name (List.length checked);
              List.iter (fun n -> Queue.enqueue t.ready_queue n) checked;
              let _ = HashMap.insert t.busy_tasks pkg_name node in
              Log.debug "[BUILD_QUEUE] Returning %s (dependencies satisfied)"
                pkg_name;
              Some node)
            else (
              Log.debug
                "[BUILD_QUEUE] %s has unsatisfied deps, adding to checked"
                pkg_name;
              find_ready (node :: checked))
    in
    find_ready [])

let mark_completed t result =
  let pkg_name = result.Package_builder.package.name in
  let _ = HashMap.remove t.busy_tasks pkg_name in
  let _ = HashMap.insert t.completed pkg_name result in

  let rec transfer () =
    match Queue.dequeue t.later_queue with
    | None -> ()
    | Some node ->
        Queue.enqueue t.ready_queue node;
        transfer ()
  in
  transfer ()

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
