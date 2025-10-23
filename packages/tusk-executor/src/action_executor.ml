open Std
open Std.Collections
open Std.Time
open Miniriot
open Tusk_planner
open Tusk_store
module G = Graph.SimpleGraph

type action_error =
  | ExecutionFailed of { message : string }
  | OutputsNotCreated of { missing : Path.t list }
  | DependenciesFailed of { failed : G.Node_id.t list }

type action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

type execution_result = {
  node_id : G.Node_id.t;
  status : action_status;
  duration : Duration.t;
  started_at : Instant.t;
  completed_at : Instant.t;
}

type deps_check_result =
  | AllDepsBuilt
  | SomeDepsNotReady of { missing : G.Node_id.t list }
  | SomeDepsFailed of { failed : G.Node_id.t list }

type t = { completed : (G.Node_id.t, execution_result) HashMap.t }
type task = Action_node.t

type Message.t +=
  | TaskCompleted of execution_result
  | TaskNeedsDeps of { task : task; missing : G.Node_id.t list }

let hash_action (action : Action.t) =
  let json = Action.to_json action in
  let content = Data.Json.to_string json in
  Crypto.hash_string content

let check_dependencies completed (node : Action_node.t) =
  let failed = Cell.create [] in
  let missing = Cell.create [] in

  List.iter
    (fun dep_id ->
      match HashMap.get completed dep_id with
      | Some { status = Cached _ | Executed; _ } -> ()
      | Some { status = Failed _ | Skipped; _ } ->
          Cell.set failed (dep_id :: Cell.get failed)
      | None -> Cell.set missing (dep_id :: Cell.get missing))
    node.deps;

  match (Cell.get failed, Cell.get missing) with
  | _ :: _, _ -> SomeDepsFailed { failed = Cell.get failed }
  | [], _ :: _ -> SomeDepsNotReady { missing = Cell.get missing }
  | [], [] -> AllDepsBuilt

let make_flags_absolute sandbox_dir flags =
  List.map
    (fun flag ->
      match flag with
      | Tusk_toolchain.Ocamlc.Impl path ->
          Tusk_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)
    flags

let get_action_outputs action = Action.outputs action

let run_action ocamlc sandbox_dir action =
  match action with
  | Action.CompileInterface { source; outputs = output :: _; includes; flags }
    ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_interface ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.CompileImplementation
      { source; outputs = output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_impl ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.GenerateInterface { source; outputs = output :: _; includes; flags }
    ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.generate_interface ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.CompileC { source; outputs = output :: _ } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      Tusk_toolchain.Ocamlc.compile_c ocamlc ~cwd:sandbox_dir ~includes:[]
        ~output:abs_output abs_source
  | Action.CreateLibrary { outputs = output :: _; objects; includes } ->
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      Tusk_toolchain.Ocamlc.create_library ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~output:abs_output abs_objects
  | Action.CreateExecutable
      { outputs = output :: _; objects; libraries; includes } ->
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in
      let abs_libraries = List.map (Path.join sandbox_dir) libraries in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      Tusk_toolchain.Ocamlc.create_executable ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~libs:abs_libraries ~output:abs_output
        abs_objects
  | Action.CompileInterface { outputs = []; _ }
  | Action.CompileImplementation { outputs = []; _ }
  | Action.GenerateInterface { outputs = []; _ }
  | Action.CompileC { outputs = []; _ }
  | Action.CreateLibrary { outputs = []; _ }
  | Action.CreateExecutable { outputs = []; _ } ->
      Tusk_toolchain.Ocamlc.Failed "Action has no outputs"
  | Action.CopyFile { source; destination } -> (
      let src_path = Path.join sandbox_dir source in
      let dst_path = Path.join sandbox_dir destination in
      match Fs.copy ~src:src_path ~dst:dst_path with
      | Ok () -> Tusk_toolchain.Ocamlc.Success "Copied"
      | Error _ ->
          Tusk_toolchain.Ocamlc.Failed
            (format "Copy failed: %s -> %s" (Path.to_string source)
               (Path.to_string destination)))
  | Action.WriteFile { destination; content } -> (
      let dest_path = Path.join sandbox_dir destination in
      Log.debug "WriteFile: writing to %s (%d bytes)" (Path.to_string dest_path)
        (String.length content);
      match Fs.write content dest_path with
      | Ok () ->
          Log.debug "WriteFile: success - file written to %s"
            (Path.to_string dest_path);
          Tusk_toolchain.Ocamlc.Success "Written"
      | Error (SystemError msg) ->
          Log.error "WriteFile: failed - %s" msg;
          Tusk_toolchain.Ocamlc.Failed
            (format "Write failed: %s - %s" (Path.to_string destination) msg))

let execute_actions toolchain store sandbox_dir actions =
  let ocamlc = Tusk_toolchain.ocamlc toolchain in
  let rec execute_next = function
    | [] -> Ok ()
    | action :: rest -> (
        Log.debug "Executing action: %s" (Action.to_string action);
        let result = run_action ocamlc sandbox_dir action in
        match result with
        | Tusk_toolchain.Ocamlc.Success _ -> execute_next rest
        | Tusk_toolchain.Ocamlc.Failed err ->
            Error (format "Action failed: %s\n%s" (Action.to_string action) err)
        )
  in
  execute_next actions

let verify_outputs outputs =
  let missing =
    List.filter
      (fun out ->
        match Fs.exists out with Ok true -> false | Ok false | Error _ -> true)
      outputs
  in
  if List.length missing > 0 then Error missing else Ok ()

let execute_node ~completed ~store toolchain sandbox_dir (node : Action_node.t)
    =
  let start = Instant.now () in
  Log.info "Worker %s: Checking deps for node %s"
    (Pid.to_string (self ()))
    (G.Node_id.to_string node.id);

  match check_dependencies completed node with
  | SomeDepsFailed { failed } ->
      let now = Instant.now () in
      let duration = Instant.duration_since ~earlier:start now in
      Log.warn "Node %s: Skipped due to failed dependencies: %s"
        (G.Node_id.to_string node.id)
        (String.concat ", "
           (List.map (fun id -> G.Node_id.to_string id) failed));
      {
        node_id = node.id;
        status = Skipped;
        duration;
        started_at = start;
        completed_at = now;
      }
  | SomeDepsNotReady { missing } ->
      Log.debug "Node %s: Dependencies not ready: %s"
        (G.Node_id.to_string node.id)
        (String.concat ", "
           (List.map (fun id -> G.Node_id.to_string id) missing));
      raise
        (Failure
           (format "Node %s has unready deps - coordinator should not dispatch"
              (G.Node_id.to_string node.id)))
  | AllDepsBuilt -> (
      Log.info "Worker %s: STARTING node %s execution"
        (Pid.to_string (self ()))
        (G.Node_id.to_string node.id);

      let actions = node.value.actions in
      let outputs = node.value.outs in
      let sources = node.value.srcs in

      let action_hashes = List.map hash_action actions in
      let combined_hash =
        Crypto.hash_string
          (String.concat ";" (List.map Crypto.Digest.hex action_hashes))
      in

      match Store.get store combined_hash with
      | Some _artifact -> (
          Log.info "Action node %s: CACHE HIT - promoting outputs"
            (G.Node_id.to_string node.id);

          Telemetry.emit
            Telemetry_events.(
              CacheHit
                {
                  package = node.value.package;
                  action = node;
                  hash = combined_hash;
                });

          let promote_result =
            Store.promote store combined_hash ~target_dir:sandbox_dir
          in
          let completed_at = Instant.now () in
          let duration = Instant.duration_since ~earlier:start completed_at in
          match promote_result with
          | Ok () ->
              {
                node_id = node.id;
                status = Cached combined_hash;
                duration;
                started_at = start;
                completed_at;
              }
          | Error msg ->
              {
                node_id = node.id;
                status = Failed (ExecutionFailed { message = msg });
                duration;
                started_at = start;
                completed_at;
              })
      | None -> (
          Log.info "Action node %s: CACHE MISS - executing actions"
            (G.Node_id.to_string node.id);

          Telemetry.emit
            Telemetry_events.(
              CacheMiss
                {
                  package = node.value.package;
                  action = node;
                  hash = combined_hash;
                });
          Telemetry.emit
            Telemetry_events.(
              ActionStarted { package = node.value.package; action = node });

          (* Copy source files into sandbox *)
          let copy_result =
            List.fold_left
              (fun acc src_path ->
                match acc with
                | Error _ -> acc
                | Ok () ->
                    let pkg_dir = node.value.package.path in
                    let abs_src = Path.join pkg_dir src_path in
                    let abs_dst = Path.join sandbox_dir src_path in
                    (match Path.parent abs_dst with
                    | Some dst_dir -> (
                        match Fs.create_dir_all dst_dir with
                        | Ok () | Error _ -> ())
                    | None -> ());
                    Fs.copy ~src:abs_src ~dst:abs_dst)
              (Ok ()) sources
          in

          match copy_result with
          | Error (SystemError msg) ->
              let completed_at = Instant.now () in
              let duration =
                Instant.duration_since ~earlier:start completed_at
              in
              {
                node_id = node.id;
                status =
                  Failed
                    (ExecutionFailed
                       { message = format "Failed to copy sources: %s" msg });
                duration;
                started_at = start;
                completed_at;
              }
          | Ok () -> (
              match execute_actions toolchain store sandbox_dir actions with
              | Error msg ->
                  let completed_at = Instant.now () in
                  let duration =
                    Instant.duration_since ~earlier:start completed_at
                  in

                  Telemetry.emit
                    Telemetry_events.(
                      ActionFailed
                        {
                          package = node.value.package;
                          action = node;
                          error = msg;
                        });

                  {
                    node_id = node.id;
                    status = Failed (ExecutionFailed { message = msg });
                    duration;
                    started_at = start;
                    completed_at;
                  }
              | Ok () -> (
                  let abs_outputs = List.map (Path.join sandbox_dir) outputs in
                  match verify_outputs abs_outputs with
                  | Error missing ->
                      let completed_at = Instant.now () in
                      let duration =
                        Instant.duration_since ~earlier:start completed_at
                      in
                      {
                        node_id = node.id;
                        status = Failed (OutputsNotCreated { missing });
                        duration;
                        started_at = start;
                        completed_at;
                      }
                  | Ok () -> (
                      let save_result =
                        Store.save store ~package:"_action_cache"
                          ~hash:combined_hash ~sandbox_dir ~outs:outputs
                      in
                      let completed_at = Instant.now () in
                      let duration =
                        Instant.duration_since ~earlier:start completed_at
                      in
                      (* NOTE: ActionCompleted needs artifact, but we don't have it here yet
                         Will fix after Store.save returns the artifact *)

                      match save_result with
                      | Ok _artifact ->
                          {
                            node_id = node.id;
                            status = Executed;
                            duration;
                            started_at = start;
                            completed_at;
                          }
                      | Error msg ->
                          Log.warn "Failed to save action to cache: %s" msg;
                          {
                            node_id = node.id;
                            status = Executed;
                            duration;
                            started_at = start;
                            completed_at;
                          })))))

type Message.t +=
  | Task of {
      task : task;
      completed : (G.Node_id.t, execution_result) HashMap.t;
    }
  | WorkerReady of Pid.t

let worker_loop ~owner ~store toolchain ~sandbox_dir () =
  Log.info "Worker %s starting, sending WorkerReady to %s"
    (Pid.to_string (self ()))
    (Pid.to_string owner);
  send owner (WorkerReady (self ()));
  let rec loop () =
    receive_any () |> function
    | Task { task; completed } -> (
        Log.info "Worker %s received task %s"
          (Pid.to_string (self ()))
          (G.Node_id.to_string task.id);
        match
          Fun.protect
            (fun () ->
              execute_node ~completed ~store toolchain sandbox_dir task)
            ~finally:(fun () -> ())
        with
        | result ->
            send owner (TaskCompleted result);
            send owner (WorkerReady (self ()));
            loop ()
        | exception exn ->
            let error = format "Exception: %s" (Printexc.to_string exn) in
            let now = Instant.now () in
            let duration = Duration.zero in
            send owner
              (TaskCompleted
                 {
                   node_id = task.id;
                   status = Failed (ExecutionFailed { message = error });
                   duration;
                   started_at = now;
                   completed_at = now;
                 });
            send owner (WorkerReady (self ()));
            loop ())
    | _ -> loop ()
  in
  loop ()

let execute ~action_graph ~sandbox ~store toolchain ~concurrency =
  let sandbox_dir = Sandbox.get_dir sandbox in
  let completed = HashMap.create () in
  let all_nodes = Action_graph.nodes action_graph in
  let pending = Cell.create all_nodes in
  let total_nodes = List.length all_nodes in

  Log.info "Starting action executor: total_nodes=%d concurrency=%d" total_nodes
    concurrency;
  List.iter
    (fun (node : Action_node.t) ->
      Log.info "  Node %s: %d actions, %d deps"
        (G.Node_id.to_string node.id)
        (List.length node.value.actions)
        (List.length node.deps))
    all_nodes;

  let all_done () =
    let pending_list = Cell.get pending in
    List.length pending_list = 0
    && HashMap.to_list completed |> List.length = total_nodes
  in

  let coordinator_pid = self () in

  let rec spawn_workers n =
    if n > 0 then (
      let worker_pid =
        spawn (fun () ->
            worker_loop ~owner:coordinator_pid ~store toolchain ~sandbox_dir ())
      in
      Log.info "Spawned worker %s" (Pid.to_string worker_pid);
      spawn_workers (n - 1))
  in

  Log.info "Spawning %d workers..." concurrency;
  spawn_workers concurrency;
  Log.info "All workers spawned, entering coordinator loop";

  let rec coordinator_loop () =
    if all_done () then (
      Log.info "Action executor: all done, completed=%d total=%d"
        (HashMap.to_list completed |> List.length)
        total_nodes;
      { completed })
    else (
      Log.debug "Coordinator waiting for messages...";
      match receive_any () with
      | WorkerReady worker -> (
          let pending_list = Cell.get pending in

          (* Check for tasks that should be skipped due to failed deps *)
          let skippable_tasks =
            List.filter
              (fun node ->
                match check_dependencies completed node with
                | SomeDepsFailed _ -> true
                | _ -> false)
              pending_list
          in

          (* Mark skippable tasks as skipped immediately *)
          List.iter
            (fun (skip_node : Action_node.t) ->
              let now = Time.Instant.now () in
              let _ =
                HashMap.insert completed skip_node.id
                  {
                    node_id = skip_node.id;
                    status = Skipped;
                    duration = Time.Duration.zero;
                    started_at = now;
                    completed_at = now;
                  }
              in
              Log.info "Action node %s: Skipped (failed dependencies)"
                (G.Node_id.to_string skip_node.id))
            skippable_tasks;

          (* Remove skipped tasks from pending *)
          let pending_list =
            List.filter
              (fun (node : Action_node.t) ->
                not
                  (List.exists
                     (fun (skip : Action_node.t) -> skip.id = node.id)
                     skippable_tasks))
              pending_list
          in
          Cell.set pending pending_list;

          (* Now check for ready tasks *)
          let ready_tasks =
            List.filter
              (fun node ->
                match check_dependencies completed node with
                | AllDepsBuilt -> true
                | _ -> false)
              pending_list
          in
          Log.debug "WorkerReady: pending=%d ready=%d skipped=%d"
            (List.length pending_list) (List.length ready_tasks)
            (List.length skippable_tasks);
          match ready_tasks with
          | task :: _ ->
              Log.info "Dispatching task %s to worker"
                (G.Node_id.to_string task.id);
              Cell.set pending (List.filter (fun n -> n <> task) pending_list);
              send worker (Task { task; completed });
              coordinator_loop ()
          | [] ->
              Log.debug "No ready tasks, worker idle";
              coordinator_loop ())
      | TaskCompleted result ->
          let _ = HashMap.insert completed result.node_id result in
          Log.info "Action node %s completed: %s (%dms)"
            (G.Node_id.to_string result.node_id)
            (match result.status with
            | Cached _ -> "cached"
            | Executed -> "executed"
            | Failed _ -> "failed"
            | Skipped -> "skipped")
            (Duration.to_millis result.duration);
          (match result.status with
          | Failed (ExecutionFailed { message }) ->
              Log.error "Action failed: %s" message
          | Failed (OutputsNotCreated { missing }) ->
              Log.error "Expected outputs not created: %s"
                (String.concat ", " (List.map Path.to_string missing))
          | Failed (DependenciesFailed { failed }) ->
              Log.error "Action dependencies failed: %s"
                (String.concat ", "
                   (List.map (fun id -> G.Node_id.to_string id) failed))
          | Skipped -> Log.warn "Action skipped due to failed dependencies"
          | _ -> ());
          coordinator_loop ()
      | _ -> coordinator_loop ())
  in

  coordinator_loop ()
