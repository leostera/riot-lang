open Std

module Queue = Collections.Queue
module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let executor_workspace =
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-executor-tests")
    ~packages:[]
    ()

let executor_config = fun ?parallelism ?on_event () ->
  Config.make
    ~workspace:executor_workspace
    ?parallelism
    ?on_event
    ()

let run_executor = fun ?parallelism ?on_event ~seeds ~execute () ->
  Executor.Runner.run_with_handlers_for_tests
    ~config:(executor_config ?parallelism ?on_event ())
    ~execution_mode:(fun _node -> Work_node.Concrete)
    ~seeds
    ~execute
    ()

let run_default_executor = fun ?parallelism ?on_event ?dependencies ~seeds ~execute () ->
  Executor.Runner.run_with_handlers_for_tests
    ~config:(executor_config ?parallelism ?on_event ())
    ?dependencies
    ~seeds
    ~execute
    ()

let node_id = fun value -> Work_node.Node_id.from_int value

let unexpected_node = fun node ->
  Error (Error.ExecutorInvariantViolated {
    message = "unexpected node in executor runner test: "
    ^ Work_node.Node_id.to_string (Work_node.id node);
  })

let sample_goal = fun ?(args = []) name ->
  Goal.RunBinary {
    package = Some (package "std");
    binary = Some name;
    args;
    profile = Riot_model.Profile.debug;
    target = target "x86_64-unknown-linux-gnu";
  }

let sample_seed = fun () ->
  Work_node.user_intent
    ~id:(node_id 1)
    (User_intent.run
      ~runnable:(User_intent.ByName "server")
      ~target:(target "x86_64-unknown-linux-gnu")
      ())

let sample_intent_seed = fun id ->
  Work_node.user_intent
    ~id:(node_id id)
    (User_intent.run
      ~runnable:(User_intent.ByName "server")
      ~target:(target "x86_64-unknown-linux-gnu")
      ())

let goal_node = fun id action -> Work_node.goal ~id:(node_id id) action

let mark_completed = Work_node.mark_as_completed

let event_kind_present = fun events ~fn ->
  events
  |> Queue.to_list
  |> List.any ~fn

let event_kind = fun __tmp1 ->
  match __tmp1 with
  | Event.WorkQueued _ -> "queued"
  | WorkStarted _ -> "started"
  | WorkCompleted _ -> "completed"
  | WorkFailed _ -> "failed"
  | WorkSpawned _ -> "spawned"
  | WorkDependenciesRegistered _ -> "dependencies"
  | WorkRequeued _ -> "requeued"

let event_node = fun __tmp1 ->
  match __tmp1 with
  | Event.WorkQueued { node }
  | WorkStarted { node }
  | WorkCompleted { node }
  | WorkFailed { node; _ }
  | WorkSpawned { node; _ }
  | WorkDependenciesRegistered { node; _ }
  | WorkRequeued { node } -> node

let event_ids_by_kind = fun events kind ->
  Queue.to_list events
  |> List.filter_map
    ~fn:(fun event ->
      if String.equal (event_kind event) kind then
        Some (
          Work_node.id (event_node event)
          |> Work_node.Node_id.to_int
        )
      else
        None)
  |> List.sort ~compare:Int.compare

let result_ids = fun summary ->
  summary.Executor.Summary.results
  |> List.map
    ~fn:(fun result ->
      Work_node.id result.Executor.Summary.node
      |> Work_node.Node_id.to_int)
  |> List.sort ~compare:Int.compare

let goal_key = fun action -> Work_node.GoalKey action

let find_goal_node = fun summary action ->
  summary.Executor.Summary.results
  |> List.find
    ~fn:(fun result ->
      match Work_node.kind result.Executor.Summary.node with
      | Work_node.Goal got -> got = action
      | _ -> false)
  |> Option.map ~fn:(fun result -> result.Executor.Summary.node)

let count_goal_runs = fun counter action ->
  fun node ->
    match Work_node.kind node with
    | Work_node.Goal got when got = action ->
        let _ = Sync.Atomic.fetch_and_add counter 1 in
        ()
    | _ -> ()

let test_empty_seeds_returns_empty_summary = fun _ctx ->
  let execute _context _node = Ok (Work_result.Complete []) in
  let summary = run_executor ~seeds:[] ~execute () in
  if
    Int.equal summary.Executor.Summary.completed_count 0
    && Int.equal summary.failed_count 0
    && List.is_empty summary.results
  then
    Ok ()
  else
    Error "expected empty executor run to return an empty summary"

let test_registry_interns_action_by_key = fun _ctx ->
  let registry = Work_registry.create () in
  let action = sample_goal "server" in
  let first = Work_registry.intern_goal registry action in
  let second = Work_registry.intern_goal registry action in
  if
    Work_node.Node_id.equal (Work_node.id first) (Work_node.id second)
    && Work_node.key first = Work_node.GoalKey action
  then
    Ok ()
  else
    Error "expected registry to return the same node for the same goal key"

let test_registry_finds_packages_and_modules = fun _ctx ->
  let registry = Work_registry.create () in
  let package_name = package "std" in
  let package_node =
    Work_registry.intern_package
      registry
      package_name
      ~make:(fun () -> Work_node.Goal (sample_goal "package"))
  in
  let module_node =
    Work_registry.intern_module
      registry
      ~package:(Some package_name)
      ~scope:(Some "lib")
      ~name:"Hello"
      ~make:(fun () -> Work_node.Goal (sample_goal "module"))
  in
  match (
    Work_registry.find_package registry package_name,
    Work_registry.find_module
      registry
      ~package:(Some package_name)
      ~scope:(Some "lib")
      ~name:"Hello"
  ) with
  | (Some found_package, Some found_module) when Work_node.Node_id.equal
    (Work_node.id package_node)
    (Work_node.id found_package)
  && Work_node.Node_id.equal (Work_node.id module_node) (Work_node.id found_module) -> Ok ()
  | _ -> Error "expected registry package and module lookups to return interned nodes"

let test_work_node_accepts_valid_status_transitions = fun _ctx ->
  let node = sample_seed () in
  Work_node.mark_as_running node;
  Work_node.mark_as_pending node;
  Work_node.mark_as_running node;
  Work_node.mark_as_completed node;
  if Work_node.status node = Work_node.Completed then
    Ok ()
  else
    Error "expected node to be completed after valid transition chain"

let test_work_node_rejects_invalid_status_transitions = fun _ctx ->
  let node = sample_seed () in
  mark_completed node;
  try
    Work_node.mark_as_running node;
    Error "expected completed node not to transition back to running"
  with
  | exception_ ->
      let message = Exception.to_string exception_ in
      if
        String.contains message "invalid work node transition"
        && String.contains message "Completed -> Running"
      then
        Ok ()
      else
        Error ("expected invalid transition panic, got: " ^ message)

let test_seed_node_id_is_preserved = fun _ctx ->
  let seed = sample_intent_seed 42 in
  let execute _context _node = Ok (Work_result.Complete []) in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match result_ids summary with
  | [ 42 ] when Work_node.Node_id.to_int (Work_node.id seed) = 42 -> Ok ()
  | _ -> Error "expected seed node id to be preserved in the summary"

let test_registry_nodes_get_fresh_ids_after_seed_ids = fun _ctx ->
  let seed = sample_intent_seed 100 in
  let child_action = sample_goal "child" in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok (Work_result.Complete [ goal_key child_action ])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match find_goal_node summary child_action with
  | Some child when Work_node.Node_id.to_int (Work_node.id child) > 100
  && result_ids summary = [ 100; Work_node.Node_id.to_int (Work_node.id child) ] -> Ok ()
  | Some _ -> Error "expected registry-created child id to be greater than seed ids"
  | None -> Error "expected child to be interned"

let test_complete_spawned_canonicalizes_returned_nodes = fun _ctx ->
  let action = sample_goal "canonical-child" in
  let events = Queue.create () in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok (Work_result.Complete [ goal_key action; goal_key action ])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary =
    run_executor
      ~parallelism:1
      ~on_event:(fun event -> Queue.push events ~value:event)
      ~seeds:[ sample_seed () ]
      ~execute
      ()
  in
  match find_goal_node summary action with
  | None -> Error "expected canonical node to be interned"
  | Some canonical ->
      let spawned_ids =
        Queue.to_list events
        |> List.filter_map
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Event.WorkSpawned { spawned; _ } ->
                Some (
                  List.map
                    spawned
                    ~fn:(fun node ->
                      Work_node.id node
                      |> Work_node.Node_id.to_int)
                )
            | _ -> None)
        |> List.concat
      in
      if
        Int.equal summary.Executor.Summary.completed_count 2
        && spawned_ids = [ Work_node.Node_id.to_int (Work_node.id canonical) ]
      then
        Ok ()
      else
        Error "expected spawned duplicate to be canonicalized through the registry"

let test_parallel_event_coverage_without_ordering = fun _ctx ->
  let first = goal_node 10 (sample_goal "first") in
  let second = goal_node 11 (sample_goal "second") in
  let events = Queue.create () in
  let execute _context _node = Ok (Work_result.Complete []) in
  let summary =
    run_executor
      ~parallelism:2
      ~on_event:(fun event -> Queue.push events ~value:event)
      ~seeds:[ first; second ]
      ~execute
      ()
  in
  if
    Int.equal summary.Executor.Summary.completed_count 2
    && event_ids_by_kind events "queued" = [ 10; 11 ]
    && event_ids_by_kind events "started" = [ 10; 11 ]
    && event_ids_by_kind events "completed" = [ 10; 11 ]
  then
    Ok ()
  else
    Error "expected parallel run to emit queued/started/completed coverage for both nodes"

let test_parallelism_one_event_sequence_is_deterministic = fun _ctx ->
  let action = sample_goal "child" in
  let events = Queue.create () in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok (Work_result.Complete [ goal_key action ])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let _summary =
    run_executor
      ~parallelism:1
      ~on_event:(fun event -> Queue.push events ~value:event)
      ~seeds:[ sample_seed () ]
      ~execute
      ()
  in
  let kinds =
    Queue.to_list events
    |> List.map ~fn:event_kind
  in
  let expected = [ "queued"; "started"; "completed"; "spawned"; "queued"; "started"; "completed"; ]
  in
  if kinds = expected then
    Ok ()
  else
    Error "expected deterministic event sequence under parallelism=1"

let test_virtual_node_declares_dependencies_and_completes_without_execution = fun _ctx ->
  let execute_calls = Sync.Atomic.make 0 in
  let child_action = sample_goal "virtual-child" in
  let seed = sample_seed () in
  let dependencies node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok [ goal_key child_action ]
    | Goal _ -> Ok []
    | _ -> unexpected_node node
  in
  let execute _context _node =
    ignore (Sync.Atomic.fetch_and_add execute_calls 1);
    Ok (Work_result.Complete [])
  in
  let summary =
    run_default_executor
      ~parallelism:1
      ~dependencies
      ~seeds:[ seed ]
      ~execute
      ()
  in
  match find_goal_node summary child_action with
  | None -> Error "expected virtual dependency to be interned"
  | Some child ->
      if
        Int.equal (Sync.Atomic.get execute_calls) 0
        && Int.equal summary.Executor.Summary.completed_count 2
        && Int.equal summary.failed_count 0
        && Work_node.status seed = Work_node.Completed
        && Work_node.status child = Work_node.Completed
      then
        Ok ()
      else
        Error "expected virtual nodes to complete after declared dependencies"

let test_virtual_parent_fails_when_declared_dependency_fails = fun _ctx ->
  let seed = sample_seed () in
  let linux = target "x86_64-unknown-linux-gnu" in
  let dependencies node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok [ Work_node.ToolchainReadyKey { target = linux } ]
    | ToolchainReady _ -> Ok []
    | _ -> unexpected_node node
  in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.ToolchainReady _ ->
        Error (Error.ToolchainFailed { target = linux; reason = "planned failure" })
    | _ -> unexpected_node node
  in
  let summary =
    run_default_executor
      ~parallelism:1
      ~dependencies
      ~seeds:[ seed ]
      ~execute
      ()
  in
  if
    Int.equal summary.Executor.Summary.failed_count 2
    && Int.equal summary.completed_count 0
    && Work_node.status seed = Work_node.Failed
  then
    Ok ()
  else
    Error "expected failed declared dependency to fail its virtual parent"

let test_requeue_with_dependencies_reruns_after_dependency_completes = fun _ctx ->
  let attempts = Sync.Atomic.make 0 in
  let events = Queue.create () in
  let dependency_action = sample_goal "dependency" in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [ goal_key dependency_action ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let seed = sample_seed () in
  let summary =
    run_executor
      ~parallelism:1
      ~on_event:(fun event -> Queue.push events ~value:event)
      ~seeds:[ seed ]
      ~execute
      ()
  in
  match find_goal_node summary dependency_action with
  | None -> Error "expected dependency to be interned"
  | Some dependency ->
      let seed_dependencies = Work_node.dependencies seed in
      let dependency_dependents = Work_node.dependents dependency in
      if not (Int.equal (Sync.Atomic.get attempts) 2) then
        Error "expected original node to run once, wait, then run again"
      else if
        not
          (List.any
            seed_dependencies
            ~fn:(fun id -> Work_node.Node_id.equal id (Work_node.id dependency)))
      then
        Error "expected original node to record dependency"
      else if
        not
          (List.any
            dependency_dependents
            ~fn:(fun id -> Work_node.Node_id.equal id (Work_node.id seed)))
      then
        Error "expected dependency to record original node as dependent"
      else if
        not
          (Int.equal summary.Executor.Summary.completed_count 2
          && Int.equal summary.failed_count 0
          && Work_node.status seed = Work_node.Completed
          && Int.equal (Work_node.pending_dependency_count seed) 0
          && Work_node.status dependency = Work_node.Completed)
      then
        Error "expected seed and dependency to complete"
      else if not
        (
          event_kind_present
            events
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Event.WorkDependenciesRegistered _ -> true
              | _ -> false)
        ) then
        Error "expected dependency registration event"
      else if not
        (
          event_kind_present
            events
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Event.WorkRequeued _ -> true
              | _ -> false)
        ) then
        Error "expected requeue event"
      else
        Ok ()

let test_multiple_dependencies_wait_for_all_to_complete = fun _ctx ->
  let attempts = Sync.Atomic.make 0 in
  let first_action = sample_goal "dep-a" in
  let second_action = sample_goal "dep-b" in
  let first_runs = Sync.Atomic.make 0 in
  let second_runs = Sync.Atomic.make 0 in
  let execute _context node =
    count_goal_runs first_runs first_action node;
    count_goal_runs second_runs second_action node;
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [ goal_key first_action; goal_key second_action ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary = run_executor ~parallelism:2 ~seeds:[ sample_seed () ] ~execute () in
  if
    Int.equal (Sync.Atomic.get attempts) 2
    && Int.equal (Sync.Atomic.get first_runs) 1
    && Int.equal (Sync.Atomic.get second_runs) 1
    && Int.equal summary.Executor.Summary.completed_count 3
  then
    Ok ()
  else
    Error "expected original node to rerun only after all dependencies completed"

let test_multiple_dependency_waves = fun _ctx ->
  let attempts = Sync.Atomic.make 0 in
  let first_action = sample_goal "wave-a" in
  let second_action = sample_goal "wave-b" in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [ goal_key first_action ])
        else if Int.equal attempt 1 then
          Ok (Work_result.RequeueWithDependencies [ goal_key second_action ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ sample_seed () ] ~execute () in
  if
    Int.equal (Sync.Atomic.get attempts) 3
    && Int.equal summary.Executor.Summary.completed_count 3
    && Int.equal summary.failed_count 0
  then
    Ok ()
  else
    Error "expected multiple dependency waves before final completion"

let test_completed_dependency_requeues_node_immediately = fun _ctx ->
  let attempts = Sync.Atomic.make 0 in
  let dependency_action = sample_goal "already-done" in
  let dependency = goal_node 50 dependency_action in
  mark_completed dependency;
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [ goal_key dependency_action ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ sample_seed (); dependency ] ~execute () in
  if
    Int.equal (Sync.Atomic.get attempts) 2
    && Int.equal summary.Executor.Summary.completed_count 1
    && Int.equal summary.failed_count 0
  then
    Ok ()
  else
    Error "expected already-completed dependency to immediately requeue dependent"

let test_duplicate_dependencies_do_not_duplicate_edges_or_runs = fun _ctx ->
  let attempts = Sync.Atomic.make 0 in
  let dependency_runs = Sync.Atomic.make 0 in
  let dependency_action = sample_goal "duplicate-dependency" in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [
            goal_key dependency_action;
            goal_key dependency_action;
          ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal action ->
        if action = dependency_action then
          ignore (Sync.Atomic.fetch_and_add dependency_runs 1);
        Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let seed = sample_seed () in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match find_goal_node summary dependency_action with
  | None -> Error "expected dependency to be interned"
  | Some dependency ->
      if
        Int.equal (Sync.Atomic.get dependency_runs) 1
        && Int.equal (List.length (Work_node.dependencies seed)) 1
        && Int.equal (List.length (Work_node.dependents dependency)) 1
      then
        Ok ()
      else
        Error "expected duplicate dependencies to deduplicate edges and execution"

let test_failed_dependency_fails_dependent = fun _ctx ->
  let dependency_action = sample_goal "dependency" in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok (Work_result.RequeueWithDependencies [ goal_key dependency_action ])
    | Work_node.Goal _ -> Error (Error.IntentPlanningFailed { reason = "dependency failed" })
    | _ -> unexpected_node node
  in
  let seed = sample_seed () in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match find_goal_node summary dependency_action with
  | None -> Error "expected dependency to be interned"
  | Some dependency ->
      if
        Int.equal summary.Executor.Summary.failed_count 2
        && Int.equal summary.completed_count 0
        && Work_node.status dependency = Work_node.Failed
        && Work_node.status seed = Work_node.Failed
      then
        Ok ()
      else
        Error "expected failed dependency to fail the dependent node"

let test_independent_work_continues_after_failure = fun _ctx ->
  let fail_action = sample_goal "fail" in
  let ok_action = sample_goal "ok" in
  let fail_node = goal_node 10 fail_action in
  let ok_node = goal_node 11 ok_action in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.Goal action when action = fail_action ->
        Error (Error.IntentPlanningFailed { reason = "planned failure" })
    | _ -> Ok (Work_result.Complete [])
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ fail_node; ok_node ] ~execute () in
  if
    Int.equal summary.Executor.Summary.failed_count 1
    && Int.equal summary.completed_count 1
    && Work_node.status fail_node = Work_node.Failed
    && Work_node.status ok_node = Work_node.Completed
  then
    Ok ()
  else
    Error "expected independent work to continue after another node fails"

let test_returned_error_is_preserved_in_summary = fun _ctx ->
  let expected = Error.IntentPlanningFailed { reason = "intent failed" } in
  let execute _context _node = Error expected in
  let summary = run_executor ~parallelism:1 ~seeds:[ sample_seed () ] ~execute () in
  match summary.Executor.Summary.results with
  | [ result ] when result.Executor.Summary.error = Some expected -> Ok ()
  | _ -> Error "expected returned Error.t to be preserved in summary"

let test_worker_exception_is_materialized_as_error = fun _ctx ->
  let execute _context _node = raise (Failure "boom") in
  let seed = sample_seed () in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match summary.Executor.Summary.results with
  | [ result ] ->
      let error = result.Executor.Summary.error in
      (
        match error with
        | Some (Error.WorkerFailed { message }) when String.contains message "boom" -> Ok ()
        | Some _ -> Error "expected worker failure error"
        | None -> Error "expected failed result to carry an error"
      )
  | _ -> Error "expected one failed result"

let test_no_node_remains_running_after_return = fun _ctx ->
  let dependency_action = sample_goal "state-dependency" in
  let attempts = Sync.Atomic.make 0 in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ ->
        let attempt = Sync.Atomic.fetch_and_add attempts 1 in
        if Int.equal attempt 0 then
          Ok (Work_result.RequeueWithDependencies [ goal_key dependency_action ])
        else
          Ok (Work_result.Complete [])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let seed = sample_seed () in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match find_goal_node summary dependency_action with
  | None -> Error "expected dependency to be interned"
  | Some dependency ->
      if
        Work_node.status seed != Work_node.Running
        && Work_node.status dependency != Work_node.Running
      then
        Ok ()
      else
        Error "expected no node to remain Running after runner returns"

let test_non_pending_seed_is_not_executed = fun _ctx ->
  let calls = Sync.Atomic.make 0 in
  let seed = sample_seed () in
  mark_completed seed;
  let execute _context _node =
    ignore (Sync.Atomic.fetch_and_add calls 1);
    Ok (Work_result.Complete [])
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  if
    Int.equal (Sync.Atomic.get calls) 0
    && Int.equal summary.Executor.Summary.completed_count 0
    && Int.equal summary.failed_count 0
  then
    Ok ()
  else
    Error "expected non-pending queued seed not to execute"

let test_unsupported_spawned_key_fails_node = fun _ctx ->
  let seed = sample_seed () in
  let execute _context node =
    match Work_node.kind node with
    | Work_node.UserIntent _ -> Ok (Work_result.Complete [ Work_node.Package (package "std") ])
    | Work_node.Goal _ -> Ok (Work_result.Complete [])
    | _ -> unexpected_node node
  in
  let summary = run_executor ~parallelism:1 ~seeds:[ seed ] ~execute () in
  match summary.Executor.Summary.results with
  | [ result ] ->
      let error = result.Executor.Summary.error in
      (
        match error with
        | Some (Error.ExecutorInvariantViolated _) when Work_node.status seed = Work_node.Failed ->
            Ok ()
        | Some _ -> Error "expected unsupported key to materialize an invariant violation"
        | None -> Error "expected unsupported key to fail the source node"
      )
  | _ -> Error "expected unsupported spawned key to produce one failed result"

let tests =
  Test.[
    case "empty seeds return an empty summary" test_empty_seeds_returns_empty_summary;
    case "registry interns goals by deterministic key" test_registry_interns_action_by_key;
    case "registry finds packages and modules" test_registry_finds_packages_and_modules;
    case
      "work node accepts valid status transitions"
      test_work_node_accepts_valid_status_transitions;
    case
      "work node rejects invalid status transitions"
      test_work_node_rejects_invalid_status_transitions;
    case "seed node id is preserved" test_seed_node_id_is_preserved;
    case
      "registry nodes get fresh ids after seed ids"
      test_registry_nodes_get_fresh_ids_after_seed_ids;
    case
      "complete spawned canonicalizes returned nodes"
      test_complete_spawned_canonicalizes_returned_nodes;
    case
      "parallel event coverage does not require ordering"
      test_parallel_event_coverage_without_ordering;
    case
      "parallelism one event sequence is deterministic"
      test_parallelism_one_event_sequence_is_deterministic;
    case
      "virtual node declares dependencies and completes without execution"
      test_virtual_node_declares_dependencies_and_completes_without_execution;
    case
      "virtual parent fails when declared dependency fails"
      test_virtual_parent_fails_when_declared_dependency_fails;
    case
      "requeue with dependencies reruns after dependency completes"
      test_requeue_with_dependencies_reruns_after_dependency_completes;
    case
      "multiple dependencies wait for all to complete"
      test_multiple_dependencies_wait_for_all_to_complete;
    case "multiple dependency waves" test_multiple_dependency_waves;
    case
      "completed dependency requeues node immediately"
      test_completed_dependency_requeues_node_immediately;
    case
      "duplicate dependencies do not duplicate edges or runs"
      test_duplicate_dependencies_do_not_duplicate_edges_or_runs;
    case "failed dependency fails dependent" test_failed_dependency_fails_dependent;
    case "independent work continues after failure" test_independent_work_continues_after_failure;
    case "returned error is preserved in summary" test_returned_error_is_preserved_in_summary;
    case "worker exception is materialized as error" test_worker_exception_is_materialized_as_error;
    case "no node remains running after return" test_no_node_remains_running_after_return;
    case "non-pending seed is not executed" test_non_pending_seed_is_not_executed;
    case "unsupported spawned key fails node" test_unsupported_spawned_key_fails_node;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_executor_runner_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
