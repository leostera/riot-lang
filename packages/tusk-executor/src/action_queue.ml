open Std
open Std.Collections
open Std.Time
open Tusk_planner

type action_node = Action_node.t

type action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of { missing: Path.t list }
  | DependenciesFailed of { failed: Graph.SimpleGraph.Node_id.t list }

type action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

type execution_result = {
  node_id: Graph.SimpleGraph.Node_id.t;
  status: action_status;
  duration: Duration.t;
  started_at: Instant.t;
  completed_at: Instant.t;
}

type t = {
  ready_queue: action_node Queue.t;
  later_queue: action_node Queue.t;
  busy_tasks: (Graph.SimpleGraph.Node_id.t, action_node) HashMap.t;
  completed: (Graph.SimpleGraph.Node_id.t, execution_result) HashMap.t;
}

let create = fun () ->
  {
    ready_queue = Queue.create ();
    later_queue = Queue.create ();
    busy_tasks = HashMap.create ();
    completed = HashMap.create ()
  }

let is_in_queue = fun queue node_id ->
  let found = ref false in
  Queue.iter
    (fun (node: action_node) ->
      if Graph.SimpleGraph.Node_id.eq node.id node_id then
        found := true)
    queue;
  !found

let dependencies_satisfied = fun t (node: action_node) ->
  List.for_all
    (fun dep_id ->
      match HashMap.get t.completed dep_id with
      | Some { status=Cached _ | Executed; _ } -> true
      | Some { status=Failed _ | Skipped; _ } -> false
      | None -> false)
    node.deps

let has_failed_dependencies = fun t (node: action_node) ->
  List.exists
    (fun dep_id ->
      match HashMap.get t.completed dep_id with
      | Some { status=Failed _ | Skipped; _ } -> true
      | _ -> false)
    node.deps

let queue = fun t (node: action_node) ->
  match HashMap.get t.completed node.id with
  | Some _ -> ()
  | None ->
      if Option.is_some (HashMap.get t.busy_tasks node.id) then
        ()
      else if is_in_queue t.ready_queue node.id then
        ()
      else if is_in_queue t.later_queue node.id then
        ()
      else
        Queue.push t.ready_queue node

let requeue_with_deps = fun t (node: action_node) ~(missing_deps:Graph.SimpleGraph.Node_id.t list) ~(all_nodes:action_node list) ->
  let _ = HashMap.remove t.busy_tasks node.id in
  if not (is_in_queue t.later_queue node.id) then
    Queue.push t.later_queue node;
  List.iter
    (fun dep_id ->
      match
        List.find_opt
          (fun (n: action_node) ->
            Graph.SimpleGraph.Node_id.eq n.id dep_id)
          all_nodes
      with
      | Some dep_node -> queue t dep_node
      | None -> Log.warn
        ("Action queue: dependency " ^ Graph.SimpleGraph.Node_id.to_string dep_id ^ " not found in graph"))
    missing_deps

let next = fun t ->
  if Queue.is_empty t.ready_queue && not (Queue.is_empty t.later_queue) then
    (
      let rec transfer () =
        match Queue.pop t.later_queue with
        | None -> ()
        | Some node ->
            Queue.push t.ready_queue node;
            transfer ()
      in
      transfer ()
    );
  let rec find_ready checked =
    match Queue.pop t.ready_queue with
    | None ->
        List.iter
          (fun node ->
            Queue.push t.later_queue node)
          checked;
        None
    | Some node ->
        if has_failed_dependencies t node then
          (
            let now = Instant.now () in
            let skip_result = {
              node_id = node.id;
              status = Skipped;
              duration = Duration.zero;
              started_at = now;
              completed_at = now;
            }
            in
            let _ = HashMap.insert t.completed node.id skip_result in
            List.iter
              (fun n ->
                Queue.push t.ready_queue n)
              checked;
            find_ready []
          )
        else if dependencies_satisfied t node then
          (
            List.iter
              (fun n ->
                Queue.push t.ready_queue n)
              checked;
            let _ = HashMap.insert t.busy_tasks node.id node in
            Some node
          )
        else
          find_ready (node :: checked)
  in
  find_ready []

let mark_completed = fun t result ->
  let _ = HashMap.remove t.busy_tasks result.node_id in
  let _ = HashMap.insert t.completed result.node_id result in
  let rec transfer () =
    match Queue.pop t.later_queue with
    | None -> ()
    | Some node ->
        Queue.push t.ready_queue node;
        transfer ()
  in
  transfer ()

let mark_failed = fun t ~node_id ~error ->
  let _ = HashMap.remove t.busy_tasks node_id in
  let now = Instant.now () in
  let failed_result = {
    node_id;
    status = Failed (ExecutionFailed { message = error });
    duration = Duration.zero;
    started_at = now;
    completed_at = now;
  }
  in
  let _ = HashMap.insert t.completed node_id failed_result in
  ()

let get_result = fun t node_id ->
  HashMap.get t.completed node_id

let stats = fun t ->
  let ready = Queue.len t.ready_queue in
  let later = Queue.len t.later_queue in
  let busy = HashMap.len t.busy_tasks in
  let completed = HashMap.len t.completed in
  let failed =
    t.completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter
      ~fn:(fun ((_, result)) ->
        match result.status with
        | Failed _
        | Skipped -> true
        | _ -> false)
    |> Iter.Iterator.count
  in
  let succeeded =
    t.completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter
      ~fn:(fun ((_, result)) ->
        match result.status with
        | Cached _
        | Executed -> true
        | _ -> false)
    |> Iter.Iterator.count
  in
  (ready, later, busy, completed, succeeded, failed)

let is_complete = fun t ~total_nodes ->
  let ready, later, busy, completed, _, _ = stats t in
  let all_queues_empty = ready = 0 && later = 0 && busy = 0 in
  let all_accounted_for = completed = total_nodes in
  all_queues_empty && all_accounted_for
