open Std

module DynamicWorkerPool = WorkerPool.DynamicWorkerPool
module ConcurrentHashMap = Collections.ConcurrentHashMap
module Queue = Collections.Queue

type context = {
  registry: Work_registry.t;
}

type execution =
  | Complete of Work_node.key list
  | RequeueWithDependencies of Work_node.key list

type task_result = {
  node: Work_node.t;
  outcome: (execution, Error.t) result;
}

type worker_result = {
  result: task_result;
  result_ref: task_result Ref.t;
}

type Message.t +=
  | WorkNodeResult of worker_result

type dispatcher_event =
  | WorkerReady of Work_node.t DynamicWorkerPool.worker
  | NodeResult of task_result

type state = {
  pool: Work_node.t DynamicWorkerPool.t;
  ready: Node_queue.t;
  idle_workers: Work_node.t DynamicWorkerPool.worker Queue.t;
  result_ref: task_result Ref.t;
  on_event: Event.t -> unit;
  registry: Work_registry.t;
  execute: context -> Work_node.t -> (execution, Error.t) result;
  mutable tasks_in_flight: int;
  mutable results: Summary.node_result list;
  mutable completed_count: int;
  mutable failed_count: int;
}

type registered_dependencies = {
  failed_dependency: Work_node.t option;
  pending_dependencies: Work_node.t list;
}

let queue_node = fun state node ->
  Node_queue.push state.ready node;
  state.on_event (Event.WorkQueued { node })

let record_result = fun state node status error ->
  state.results <- { Summary.node; status; error } :: state.results;
  match status with
  | Work_node.Completed -> state.completed_count <- Int.succ state.completed_count
  | Failed -> state.failed_count <- Int.succ state.failed_count
  | Pending
  | Running -> ()

let unsupported_key_error = fun key ->
  let key_name =
    match key with
    | Work_node.Intent _ -> "intent"
    | Work_node.Package _ -> "package"
    | Work_node.Module _ -> "module"
    | Work_node.Source _ -> "source"
    | Work_node.GoalKey _ -> "goal"
    | Work_node.PackageWorkKey _ -> "package-work"
    | Work_node.ToolchainReadyKey _ -> "toolchain-ready"
    | Work_node.SourceAnalysisKey _ -> "source-analysis"
    | Work_node.ModulePlanKey _ -> "module-plan"
    | Work_node.PackageFinalizeKey _ -> "package-finalize"
    | Work_node.ActionExecutionKey _ -> "action-execution"
  in
  Error.ExecutorInvariantViolated {
    message = "work key '" ^ key_name ^ "' has no executable work node kind yet";
  }

let canonical_node_for_key = fun state key ->
  match Work_registry.find state.registry key with
  | Some node -> Ok node
  | None -> (
      match Work_node.kind_of_key key with
      | Some kind ->
          Ok (Work_registry.intern state.registry ~key ~make:(fun () -> kind))
      | None -> Error (unsupported_key_error key)
    )

let canonical_nodes_for_keys = fun state keys ->
  let seen = ConcurrentHashMap.with_capacity ~size:(List.length keys) in
  let add_once node =
    ConcurrentHashMap.compute
      seen
      ~key:(Work_node.id node)
      ~fn:(fun current ->
        match current with
        | Some () -> ConcurrentHashMap.Abort false
        | None -> ConcurrentHashMap.Insert ((), true))
  in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | key :: rest -> (
        match canonical_node_for_key state key with
        | Ok node ->
            if add_once node then
              loop (node :: acc) rest
            else
              loop acc rest
        | Error error -> Error error
      )
  in
  loop [] keys

let dispatch_available = fun state ->
  let rec loop () =
    match Queue.pop state.idle_workers with
    | None -> ()
    | Some worker -> (
        match Node_queue.pop state.ready with
        | None -> Queue.push state.idle_workers ~value:worker
        | Some node ->
            if Work_node.compare_and_set_status node ~from:Pending ~to_:Running then (
              state.tasks_in_flight <- Int.succ state.tasks_in_flight;
              state.on_event (Event.WorkStarted { node });
              DynamicWorkerPool.send_task state.pool worker node;
              loop ()
            ) else (
              Queue.push state.idle_workers ~value:worker;
              loop ()
            )
      )
  in
  loop ()

let is_complete = fun state ->
  Int.equal state.tasks_in_flight 0 && Node_queue.is_empty state.ready

let rec fail_node = fun state node error ->
  match Work_node.status node with
  | Failed
  | Completed -> ()
  | Pending
  | Running ->
      Work_node.set_status node Failed;
      state.on_event (Event.WorkFailed { node; error });
      record_result state node Failed (Some error);
      settle_dependents state node

and settle_dependents = fun state node ->
  Work_node.dependents node
  |> List.for_each
    ~fn:(fun dependent_id ->
      match Work_registry.find_by_id state.registry dependent_id with
      | None -> ()
      | Some dependent -> (
          match Work_node.status dependent with
          | Pending ->
              (
                match Work_node.status node with
                | Failed ->
                    fail_node
                      state
                      dependent
                      (Error.DependencyFailed {
                        node = Work_node.id dependent;
                        dependency = Work_node.id node;
                      })
                | Completed ->
                    let remaining = Work_node.mark_dependency_completed dependent in
                    if Int.equal remaining 0 then
                      queue_node state dependent
                | Pending
                | Running -> ()
              )
          | Running
          | Completed
          | Failed -> ()
        ))

let register_dependencies = fun state node dependencies ->
  let node_id = Work_node.id node in
  let failed_dependency = ref None in
  let pending_count = ref 0 in
  let pending_dependencies = ref [] in
  List.for_each
    dependencies
    ~fn:(fun dependency ->
      if Work_node.add_dependency node (Work_node.id dependency) then (
        ignore (Work_node.add_dependent dependency node_id);
        match Work_node.status dependency with
        | Work_node.Pending
        | Running ->
            pending_count := Int.succ !pending_count;
            pending_dependencies := dependency :: !pending_dependencies
        | Completed -> ()
        | Failed -> failed_dependency := Some dependency
      ));
  Work_node.add_pending_dependencies node !pending_count;
  state.on_event (Event.WorkDependenciesRegistered { node; dependencies })
  |> fun () -> {
    failed_dependency = !failed_dependency;
    pending_dependencies = !pending_dependencies;
  }

let reconcile_registered_dependencies = fun node registered ->
  let failed_dependency =
    match registered.failed_dependency with
    | Some _ as failed -> failed
    | None ->
        List.find
          registered.pending_dependencies
          ~fn:(fun dependency -> Work_node.status dependency = Work_node.Failed)
  in
  registered.pending_dependencies
  |> List.for_each
    ~fn:(fun dependency ->
      if Work_node.status dependency = Work_node.Completed then
        ignore (Work_node.mark_dependency_completed node));
  failed_dependency

let complete_result = fun state result ->
  state.tasks_in_flight <- state.tasks_in_flight - 1;
  match result.outcome with
  | Ok (Complete spawned_keys) -> (
      match canonical_nodes_for_keys state spawned_keys with
      | Error error -> fail_node state result.node error
      | Ok spawned ->
          Work_node.set_status result.node Completed;
          state.on_event (Event.WorkCompleted { node = result.node });
          if not (List.is_empty spawned) then
            state.on_event (Event.WorkSpawned { node = result.node; spawned });
          List.for_each spawned ~fn:(queue_node state);
          record_result state result.node Completed None;
          settle_dependents state result.node
    )
  | Ok (RequeueWithDependencies dependency_keys) -> (
      match canonical_nodes_for_keys state dependency_keys with
      | Error error -> fail_node state result.node error
      | Ok dependencies ->
          let registered = register_dependencies state result.node dependencies in
          Work_node.set_status result.node Pending;
          state.on_event (Event.WorkRequeued { node = result.node });
          let failed_dependency = reconcile_registered_dependencies result.node registered in
          match failed_dependency with
          | Some dependency ->
              fail_node
                state
                result.node
                (Error.DependencyFailed {
                  node = Work_node.id result.node;
                  dependency = Work_node.id dependency;
                })
          | None ->
              List.for_each
                dependencies
                ~fn:(fun dependency ->
                  match Work_node.status dependency with
                  | Pending -> queue_node state dependency
                  | Running
                  | Completed
                  | Failed -> ());
              if Work_node.dependencies_ready result.node then
                queue_node state result.node
    )
  | Error error ->
      fail_node state result.node error

let rec loop = fun state ->
  dispatch_available state;
  if is_complete state then
    {
      Summary.results = List.reverse state.results;
      completed_count = state.completed_count;
      failed_count = state.failed_count;
    }
  else
    let selector: dispatcher_event selector = fun msg ->
      match msg with
      | DynamicWorkerPool.WorkerReady worker -> (
          match Ref.type_equal state.pool.task_ref (DynamicWorkerPool.get_worker_task_ref worker) with
          | Some Type.Equal -> Select (WorkerReady worker)
          | None -> Skip
        )
      | WorkNodeResult { result; result_ref } -> (
          match Ref.type_equal state.result_ref result_ref with
          | Some Type.Equal -> Select (NodeResult result)
          | None -> Skip
        )
      | _ -> Skip
    in
    match receive ~selector () with
    | WorkerReady worker ->
        Queue.push state.idle_workers ~value:worker;
        loop state
    | NodeResult result ->
        complete_result state result;
        loop state

let run = fun ?(parallelism = Thread.available_parallelism) ?(on_event = fun _ -> ()) ~seeds ~execute () ->
  match seeds with
  | [] -> { Summary.results = []; completed_count = 0; failed_count = 0 }
  | _ ->
      let owner = self () in
      let result_ref = Ref.make () in
      let max_seed_id =
        List.fold_left
          seeds
          ~init:0
          ~fn:(fun max_id seed ->
            Int.max max_id (Work_node.Node_id.to_int (Work_node.id seed)))
      in
      let registry = Work_registry.create ~next_id:max_seed_id () in
      let seeds = List.map seeds ~fn:(Work_registry.register registry) in
      let context = { registry } in
      let worker_fn = fun ~owner ~task:node ->
        let result =
          match execute context node with
          | Ok execution -> { node; outcome = Ok execution }
          | Error error -> { node; outcome = Error error }
          | exception exn ->
              {
                node;
                outcome = Error (Error.WorkerFailed { message = Exception.to_string exn });
              }
        in
        send owner (WorkNodeResult { result; result_ref })
      in
      let pool =
        DynamicWorkerPool.start
          ~concurrency:(Int.max 1 parallelism)
          ~owner
          ~worker_fn
          ()
      in
      let state = {
        pool;
        ready = Node_queue.create ();
        idle_workers = Queue.create ();
        result_ref;
        on_event;
        registry;
        execute;
        tasks_in_flight = 0;
        results = [];
        completed_count = 0;
        failed_count = 0;
      }
      in
      List.for_each seeds ~fn:(queue_node state);
      loop state
