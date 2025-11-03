open Std
open Std.Collections

open Tusk_model
open Tusk_planner
open Telemetry_events

type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
  package_graph : Package_graph.t;
}

(* Message types - simplified unidirectional flow *)
type Message.t +=
  | TaskCompleted of {
      worker_pid : Pid.t;
      result : Package_builder.build_result;
    }
  | AssignTask of Build_queue.package_node

(* Coordinator state with explicit worker tracking *)
type coordinator_state = {
  packages : Package.t list;
  queue : Build_queue.t;
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, Build_queue.package_node) Hashtbl.t;
  all_workers : Pid.t list;
  total_packages : int;
  completed_count : int ref;
  package_graph : Package_graph.t;
}

(* Worker implementation - purely reactive, no WorkerReady *)
let rec worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph =
  match receive_any () with
  | AssignTask node ->
      (* Execute the build *)
      let package = Package_graph.get_package node.value in
      let result =
        Package_builder.build ~workspace ~toolchain ~store ~package_graph
          ~package
      in

      (* Report completion *)
      send coordinator (TaskCompleted { worker_pid = self (); result });

      (* Continue waiting for work *)
      worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph
  | _ ->
      (* Ignore other messages *)
      worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph

(* Try to assign all available work to idle workers *)
let rec drain_work_queue state =
  match Queue.dequeue state.idle_workers with
  | None -> () (* No idle workers *)
  | Some worker_pid -> (
      match Build_queue.next state.queue with
      | None ->
          (* No work available, put worker back *)
          Queue.enqueue state.idle_workers worker_pid
      | Some node ->
          (* Assign work *)
          (* Log.debug "Assigning %s to worker %a" 
            (Package_graph.get_package node.value).Package.name
            Pid.pp worker_pid; *)
          Hashtbl.add state.busy_workers worker_pid node;
          send worker_pid (AssignTask node);

          (* Continue trying to assign more work *)
          drain_work_queue state)

(* Main coordinator loop *)
let rec coordinator_loop state =
  (* Step 1: Try to assign all available work *)
  drain_work_queue state;

  (* Step 2: Check if we're done *)
  if !(state.completed_count) = state.total_packages then
    handle_build_completed state
  else
    (* Step 3: Wait for a worker to complete *)
    wait_for_completion state

and wait_for_completion state =
  match receive_any () with
  | TaskCompleted { worker_pid; result } ->
      (* Update build queue *)
      Build_queue.mark_completed state.queue result;
      incr state.completed_count;

      (* Log the result *)
      Log.info "Package %s: %s (%dms)" result.package.Package.name
        (match result.status with
        | Cached _ -> "cached"
        | Built _ -> "built"
        | Failed _ -> "failed")
        (Time.Duration.to_millis result.duration);

      (* Move worker from busy to idle *)
      Hashtbl.remove state.busy_workers worker_pid;
      Queue.enqueue state.idle_workers worker_pid;

      (* CRITICAL: New work may now be available due to this completion *)
      (* Loop back to drain_work_queue which will assign it *)
      coordinator_loop state
  | _ ->
      (* Ignore other messages *)
      wait_for_completion state

and handle_build_completed state =
  let succeeded = ref 0 in
  let failed = ref 0 in
  let cached = ref 0 in

  (* Count results *)
  List.iter
    (fun pkg ->
      match Build_queue.get_result state.queue pkg.Package.name with
      | Some result -> (
          match result.Package_builder.status with
          | Cached _ -> incr cached
          | Built _ -> incr succeeded
          | Failed _ -> incr failed)
      | None -> ())
    state.packages;

  Log.info
    "Workspace build: all done, completed=%d succeeded=%d failed=%d total=%d"
    !(state.completed_count) !succeeded !failed state.total_packages;

  (* Return results *)
  {
    results =
      List.filter_map
        (fun (pkg : Package.t) -> Build_queue.get_result state.queue pkg.name)
        state.packages;
    total_duration = Time.Duration.zero;
    (* Will be computed by caller *)
    cached_count = !cached;
    built_count = !succeeded;
    failed_count = !failed;
    package_graph = state.package_graph;
  }

(* Initialize and start the build *)
let init ~workspace ~toolchain ~store ~package_graph ~packages ~concurrency =
  let coordinator_pid = self () in

  (* Create build queue *)
  let queue = Build_queue.create () in
  Package_graph.iter_nodes package_graph ~fn:(fun node ->
      Build_queue.queue queue node);

  (* Spawn workers - they start idle, waiting for work *)
  let workers =
    List.make ~len:concurrency ~fn:(fun _ ->
        spawn (fun () ->
            worker_loop ~coordinator:coordinator_pid ~workspace ~toolchain
              ~store ~package_graph))
  in

  (* Initialize coordinator state *)
  let idle_workers = Queue.create () in
  List.iter (fun w -> Queue.enqueue idle_workers w) workers;

  let state =
    {
      packages;
      queue;
      idle_workers;
      busy_workers = Hashtbl.create concurrency;
      all_workers = workers;
      total_packages = List.length packages;
      completed_count = ref 0;
      package_graph;
    }
  in

  (* Start coordinator loop *)
  coordinator_loop state

(* Public API - same interface as original coordinator *)
let build_workspace ~workspace ~toolchain ~store ~target ~concurrency =
  let start = Time.Instant.now () in

  match Tusk_planner.plan_workspace ~workspace ~target ~load_errors:[] with
  | Error err -> Error err
  | Ok { packages; package_graph; _ } -> (
      Telemetry.emit
        (WorkspaceStarted { target; package_count = List.length packages });

      Log.info "Building %d packages with %d workers" (List.length packages)
        concurrency;

      match Package_graph.topological_sort package_graph with
      | exception Package_graph.Cycle_detected cycle ->
          Error (Workspace_planner.CycleDetected { cycle })
      | nodes ->
          let packages = List.map Package_graph.get_package nodes in

          (* Run coordinator to completion *)
          let result =
            init ~workspace ~toolchain ~store ~package_graph ~packages
              ~concurrency
          in

          (* Calculate total duration *)
          let total_duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in

          Ok { result with total_duration })
