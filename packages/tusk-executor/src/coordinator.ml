open Std
open Std.Collections
open Std.Sync
open Std.Sync.Cell

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

type Message.t +=
  | TaskCompleted of {
      worker_pid : Pid.t;
      result : Package_builder.build_result;
    }
  | AssignTask of Build_queue.package_node

type coordinator_state = {
  packages : Package_graph.package_node list;
  queue : Build_queue.t;
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, Build_queue.package_node) HashMap.t;
  total_packages : int;
  mutable completed_count : int;
  package_graph : Package_graph.t;
}

let rec worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph
    ~build_ctx =
  match receive_any () with
  | AssignTask node ->
      let package = Package_graph.get_package node.value in
      let result =
        Package_builder.build ~workspace ~toolchain ~store ~package_graph
          ~package_key:(Package_graph.get_key node.value) ~package ~build_ctx
      in
      send coordinator (TaskCompleted { worker_pid = self (); result });
      worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph
        ~build_ctx
  | _ ->
      worker_loop ~coordinator ~workspace ~toolchain ~store ~package_graph
        ~build_ctx

let rec drain_work_queue state =
  match Queue.pop state.idle_workers with
  | None -> ()
  | Some worker_pid -> (
      match Build_queue.next state.queue with
      | None ->
          Queue.push state.idle_workers worker_pid
      | Some node ->
          let _ = HashMap.insert state.busy_workers worker_pid node in
          send worker_pid (AssignTask node);
          drain_work_queue state)

let rec coordinator_loop state =
  drain_work_queue state;

  if state.completed_count = state.total_packages then
    handle_build_completed state
  else wait_for_completion state

and wait_for_completion state =
  match receive_any () with
  | TaskCompleted { worker_pid; result } ->
      Build_queue.mark_completed state.queue result;
      state.completed_count <- state.completed_count + 1;
      let status =
        match result.status with
        | Cached _ -> "cached"
        | Built _ -> "built"
        | Failed _ -> "failed"
      in

      Log.info
        ("Package " ^ result.package.Package.name ^ ": " ^ status ^ " ("
        ^ Int.to_string (Time.Duration.to_millis result.duration)
        ^ "ms)");

      let _ = HashMap.remove state.busy_workers worker_pid in
      Queue.push state.idle_workers worker_pid;
      coordinator_loop state
  | _ -> wait_for_completion state

and handle_build_completed state =
  let succeeded = ref 0 in
  let failed = ref 0 in
  let cached = ref 0 in

  List.iter
    (fun pkg_node ->
      match
        Build_queue.get_result state.queue (Package_graph.get_key pkg_node)
      with
      | Some result -> (
          match result.Package_builder.status with
          | Cached _ -> cached := !cached + 1
          | Built _ -> succeeded := !succeeded + 1
          | Failed _ -> failed := !failed + 1)
      | None -> ())
    state.packages;

  Log.info
    ("Workspace build: all done, completed="
    ^ Int.to_string state.completed_count ^ " succeeded="
    ^ Int.to_string !succeeded ^ " failed=" ^ Int.to_string !failed
    ^ " total=" ^ Int.to_string state.total_packages);

  {
    results =
      List.filter_map
        (fun pkg_node ->
          Build_queue.get_result state.queue (Package_graph.get_key pkg_node))
        state.packages;
    total_duration = Time.Duration.zero;
    cached_count = !cached;
    built_count = !succeeded;
    failed_count = !failed;
    package_graph = state.package_graph;
  }

let init ~workspace ~toolchain ~store ~package_graph ~packages ~concurrency
    ~build_ctx =
  let coordinator_pid = self () in
  let queue = Build_queue.create () in
  Build_queue.set_package_graph queue package_graph;
  Package_graph.iter_nodes package_graph ~fn:(fun node -> Build_queue.queue queue node);

  let workers =
    List.make ~len:concurrency ~fn:(fun _ ->
        spawn (fun () ->
            worker_loop ~coordinator:coordinator_pid ~workspace ~toolchain
              ~store ~package_graph ~build_ctx))
  in
  let idle_workers = Queue.create () in
  List.iter (fun worker -> Queue.push idle_workers worker) workers;

  let state =
    {
      packages;
      queue;
      idle_workers;
      busy_workers = HashMap.create ();
      total_packages = List.length packages;
      completed_count = 0;
      package_graph;
    }
  in
  coordinator_loop state

let build_workspace ~workspace ~toolchain ~store ~target ~scope ~concurrency
    ~build_ctx ~session_id =
  let start = Time.Instant.now () in

  match Tusk_planner.plan_workspace ~workspace ~target ~scope ~load_errors:[] with
  | Error err -> Error err
  | Ok { packages; package_graph; _ } -> (
      Telemetry.emit
        (WorkspaceStarted
           { session_id; target; package_count = List.length packages });

      Log.info
        ("Building " ^ Int.to_string (List.length packages)
        ^ " packages with " ^ Int.to_string concurrency ^ " workers");

      match Package_graph.topological_sort package_graph with
      | exception Package_graph.Cycle_detected cycle ->
          Error (Workspace_planner.CycleDetected { cycle })
      | nodes ->
          let result =
            init ~workspace ~toolchain ~store ~package_graph ~packages:nodes
              ~concurrency ~build_ctx
          in
          let total_duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in
          Ok { result with total_duration })
