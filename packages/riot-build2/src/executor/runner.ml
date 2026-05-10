open Std
open Std.Result.Syntax

module DynamicWorkerPool = WorkerPool.DynamicWorkerPool
module ConcurrentHashMap = Collections.ConcurrentHashMap
module Queue = Collections.Queue

type worker_task =
  | PlanNode of Work_node.t
  | ExecuteNode of Work_node.t

type plan_result = {
  node: Work_node.t;
  outcome: (Work_request.t list, Error.t) result;
}

type execute_result = {
  node: Work_node.t;
  outcome: (Work_result.t, Error.t) result;
}

type task_result = {
  task: worker_task;
  outcome: [
    | `Planned of (Work_request.t list, Error.t) result
    | `Executed of (Work_result.t, Error.t) result
  ];
}

type worker_result = {
  result: task_result;
  result_ref: task_result Ref.t;
}

type Message.t +=
  | WorkNodeResult of worker_result

type dispatcher_event =
  | WorkerReady of worker_task DynamicWorkerPool.worker
  | NodeResult of task_result

type state = {
  pool: worker_task DynamicWorkerPool.t;
  ready: Node_queue.t;
  idle_workers: worker_task DynamicWorkerPool.worker Queue.t;
  result_ref: task_result Ref.t;
  on_event: Event.t -> unit;
  registry: Work_registry.t;
  plan_dependencies: Work_registry.t -> Work_node.t -> (Work_request.t list, Error.t) result;
  execution_mode: Work_node.t -> Work_node.execution_mode;
  execute: Work_registry.t -> Work_node.t -> (Work_result.t, Error.t) result;
  mutable tasks_in_flight: int;
  mutable results: ExecutionSummary.node_result list;
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
  state.results <- { ExecutionSummary.node; status; error } :: state.results;
  match status with
  | Work_node.Completed -> state.completed_count <- Int.succ state.completed_count
  | Failed -> state.failed_count <- Int.succ state.failed_count
  | Unplanned
  | Planning
  | Waiting
  | Ready
  | Running -> ()

let unsupported_key_error = fun key ->
  let key_name =
    match key with
    | Work_node.Intent _ -> "intent"
    | Work_node.Package _ -> "package"
    | Work_node.Module _ -> "module"
    | Work_node.Source _ -> "source"
    | Work_node.GoalKey _ -> "goal"
    | Work_node.ToolchainReadyKey _ -> "toolchain-ready"
    | Work_node.SourceAnalysisKey _ -> "source-analysis"
    | Work_node.PackageArtifactKey _ -> "package-artifact"
    | Work_node.PackageFinalizeKey _ -> "package-finalize"
    | Work_node.ModulePlanKey _ -> "module-plan"
    | Work_node.ActionPlanKey _ -> "action-plan"
    | Work_node.OCamlLibraryKey _ -> "ocaml-library"
    | Work_node.OCamlArchiveKey _ -> "ocaml-archive"
    | Work_node.ActionExecutionKey _ -> "action-execution"
  in
  Error.ExecutorInvariantViolated {
    message = "work key '" ^ key_name ^ "' has no executable work node kind yet";
  }

let canonical_node_for_key = fun state key ->
  match Work_registry.find state.registry key with
  | Some node -> Ok node
  | None -> (
      match Work_node.kind_from_key key with
      | Some kind -> Ok (Work_registry.intern state.registry ~key ~make:(fun () -> kind))
      | None -> Error (unsupported_key_error key)
    )

let canonical_node_for_request = fun state request ->
  match request with
  | Work_request.Existing key -> canonical_node_for_key state key
  | Materialize kind ->
      let key = Work_node.key_from_kind kind in
      Ok (Work_registry.intern state.registry ~key ~make:(fun () -> kind))

let canonical_nodes_for_requests = fun state requests ->
  let seen = ConcurrentHashMap.with_capacity ~size:(List.length requests) in
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
    | request :: rest -> (
        match canonical_node_for_request state request with
        | Ok node ->
            if add_once node then
              loop (node :: acc) rest
            else
              loop acc rest
        | Error error -> Error error
      )
  in
  loop [] requests

let is_complete = fun state -> Int.equal state.tasks_in_flight 0 && Node_queue.is_empty state.ready

let rec fail_node = fun state node error ->
  match Work_node.status node with
  | Failed
  | Completed -> ()
  | Unplanned
  | Planning
  | Waiting
  | Ready
  | Running ->
      Work_node.mark_as_failed node;
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
          | Waiting -> (
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
                  if Int.equal remaining 0 then (
                    Work_node.mark_as_ready dependent;
                    queue_node state dependent
                  )
              | Unplanned
              | Planning
              | Waiting
              | Ready
              | Running -> ()
            )
          | Unplanned
          | Planning
          | Ready
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
        | Work_node.Unplanned
        | Planning
        | Waiting
        | Ready
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

let complete_node = fun state node ->
  match Work_node.status node with
  | Work_node.Completed
  | Failed -> ()
  | Unplanned
  | Planning
  | Waiting
  | Ready
  | Running ->
      Work_node.mark_as_completed node;
      state.on_event (Event.WorkCompleted { node });
      record_result state node Completed None;
      settle_dependents state node

let register_dependency_requests = fun state node dependency_requests ->
  if List.is_empty dependency_requests then
    Ok ()
  else
    (
      match canonical_nodes_for_requests state dependency_requests with
      | Error error -> Error error
      | Ok dependencies ->
          let registered = register_dependencies state node dependencies in
          let failed_dependency = reconcile_registered_dependencies node registered in
          match failed_dependency with
          | Some dependency ->
              Error (Error.DependencyFailed {
                node = Work_node.id node;
                dependency = Work_node.id dependency;
              })
          | None ->
              List.for_each
                dependencies
                ~fn:(fun dependency ->
                  match Work_node.status dependency with
                  | Unplanned -> queue_node state dependency
                  | Ready when Work_node.dependencies_ready dependency -> queue_node state dependency
                  | Planning
                  | Waiting
                  | Ready
                  | Running
                  | Completed
                  | Failed -> ());
              Ok ()
    )

type dispatch_result =
  | Dispatched
  | NotDispatched

let dispatch_ready_node = fun state worker node ->
  if not (Work_node.dependencies_ready node) then
    NotDispatched
  else
    match state.execution_mode node with
    | Work_node.Virtual ->
        complete_node state node;
        NotDispatched
    | Work_node.Concrete ->
        Work_node.mark_as_running node;
        state.tasks_in_flight <- Int.succ state.tasks_in_flight;
        state.on_event (Event.WorkStarted { node });
        DynamicWorkerPool.send_task state.pool worker (ExecuteNode node);
        Dispatched

let dispatch_plan_node = fun state worker node ->
  Work_node.mark_as_planning node;
  state.tasks_in_flight <- Int.succ state.tasks_in_flight;
  state.on_event (Event.WorkPlanningStarted { node });
  DynamicWorkerPool.send_task state.pool worker (PlanNode node);
  Dispatched

let prepare_node_for_dispatch = fun state worker node ->
  match Work_node.status node with
  | Work_node.Unplanned -> dispatch_plan_node state worker node
  | Ready -> dispatch_ready_node state worker node
  | Planning
  | Waiting -> NotDispatched
  | Running
  | Completed
  | Failed -> NotDispatched

let dispatch_available = fun state ->
  let rec loop () =
    match Queue.pop state.idle_workers with
    | None -> ()
    | Some worker -> (
        match Node_queue.pop state.ready with
        | None -> Queue.push state.idle_workers ~value:worker
        | Some node -> (
            match prepare_node_for_dispatch state worker node with
            | Dispatched -> loop ()
            | NotDispatched ->
                Queue.push state.idle_workers ~value:worker;
                loop ()
          )
      )
  in
  loop ()

let complete_plan_result = fun state (result: plan_result) ->
  state.tasks_in_flight <- state.tasks_in_flight - 1;
  match result.outcome with
  | Error error -> fail_node state result.node error
  | Ok dependency_requests -> (
      match register_dependency_requests state result.node dependency_requests with
      | Error error -> fail_node state result.node error
      | Ok () ->
          state.on_event (Event.WorkPlanningCompleted { node = result.node });
          if Work_node.dependencies_ready result.node then (
            Work_node.mark_as_ready result.node;
            queue_node state result.node
          )
          else
            Work_node.mark_as_waiting result.node
    )

let complete_execute_result = fun state (result: execute_result) ->
  state.tasks_in_flight <- state.tasks_in_flight - 1;
  match result.outcome with
  | Ok (Work_result.Complete spawned_requests) -> (
      match canonical_nodes_for_requests state spawned_requests with
      | Error error -> fail_node state result.node error
      | Ok spawned ->
          Work_node.mark_as_completed result.node;
          state.on_event (Event.WorkCompleted { node = result.node });
          if not (List.is_empty spawned) then
            state.on_event (Event.WorkSpawned { node = result.node; spawned });
          List.for_each spawned ~fn:(queue_node state);
          record_result state result.node Completed None;
          settle_dependents state result.node
    )
  | Ok (Work_result.RequeueWithDependencies dependency_requests) -> (
      match register_dependency_requests state result.node dependency_requests with
      | Error error -> fail_node state result.node error
      | Ok () ->
          state.on_event (Event.WorkRequeued { node = result.node });
          if Work_node.dependencies_ready result.node then (
            Work_node.mark_as_ready result.node;
            queue_node state result.node
          )
          else
            Work_node.mark_as_waiting result.node
    )
  | Error error -> fail_node state result.node error

let complete_result = fun state result ->
  match result.outcome with
  | `Planned outcome ->
      complete_plan_result state { node = (
        match result.task with
        | PlanNode node
        | ExecuteNode node -> node
      ); outcome }
  | `Executed outcome ->
      complete_execute_result state { node = (
        match result.task with
        | PlanNode node
        | ExecuteNode node -> node
      ); outcome }

let rec loop = fun state ->
  dispatch_available state;
  if is_complete state then
    {
      ExecutionSummary.results = List.reverse state.results;
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

let default_plan_dependencies = fun (_registry: Work_registry.t) (_node: Work_node.t) -> Ok []

let worker_fn registry result_ref plan_dependencies execute ~owner ~task =
  let result =
    match task with
    | PlanNode node ->
        let outcome =
          match plan_dependencies registry node with
          | Ok planned -> Ok planned
          | Error error -> Error error
          | exception exn ->
              Error (Error.WorkerFailed { message = Exception.to_string exn })
        in
        { task; outcome = `Planned outcome }
    | ExecuteNode node ->
        let outcome =
          match execute registry node with
          | Ok execution -> Ok execution
          | Error error -> Error error
          | exception exn ->
              Error (Error.WorkerFailed { message = Exception.to_string exn })
        in
        { task; outcome = `Executed outcome }
  in
  send owner (WorkNodeResult { result; result_ref })

let run_with_handlers = fun
  ?(plan_dependencies = default_plan_dependencies)
  ?(execution_mode = Work_node.execution_mode)
  ~config
  ~seeds
  ~execute
  () ->
  match seeds with
  | [] -> { ExecutionSummary.results = []; completed_count = 0; failed_count = 0 }
  | _ ->
      let owner = self () in
      let result_ref = Ref.make () in
      let max_seed_id =
        List.fold_left
          seeds
          ~init:0
          ~fn:(fun max_id seed -> Int.max max_id (Work_node.Node_id.to_int (Work_node.id seed)))
      in
      let registry = Work_registry.create ~next_id:max_seed_id () in
      let seeds = List.map seeds ~fn:(Work_registry.register registry) in
      let pool =
        DynamicWorkerPool.start
          ~concurrency:Build_config.(config.parallelism)
          ~owner
          ~worker_fn:(worker_fn registry result_ref plan_dependencies execute)
          ()
      in
      let state = {
        pool;
        ready = Node_queue.create ();
        idle_workers = Queue.create ();
        result_ref;
        on_event = config.on_event;
        registry;
        plan_dependencies;
        execution_mode;
        execute;
        tasks_in_flight = 0;
        results = [];
        completed_count = 0;
        failed_count = 0;
      }
      in
      List.for_each seeds ~fn:(queue_node state);
      loop state

let run = fun ~services ~seeds () ->
  run_with_handlers
    ~config:(Build_services.config services)
    ~plan_dependencies:(Build_services.plan_dependencies services)
    ~execute:(Build_services.execute_node services)
    ~seeds
    ()
