(** Build worker - executes build tasks in sandboxes *)

open Miniriot

(** Main worker loop *)
let main server_pid worker_id () =
  let worker_pid = self () in

  let rec worker_loop () =
    (* Tell server we're ready for work *)
    send server_pid (Worker_pool.Worker (Worker_pool.WorkerReady worker_pid));

    (* Wait for a task *)
    match receive_any () with
    | Worker_pool.Task task ->
        handle_task server_pid worker_pid task;
        worker_loop ()
    | _ ->
        (* Ignore other messages *)
        worker_loop ()
  and handle_task server_pid worker_pid task =
    let { Worker_pool.node; workspace; session_id } = task in
    let pkg_name = node.Build_node.package.name in

    (* Log that we're starting *)
    Log.package_started ?sid:session_id ~package:pkg_name;

    (* Create build graph for planning *)
    let build_graph = Build_graph.create workspace node.Build_node.toolchain in

    (* FIXME: the store should know this already! we shouldn't have to compute this path here. *)
    let target_dir =
      let root = Std.Path.to_string workspace.root in
      let debug_dir = Filename.concat (Filename.concat root "target") "debug" in
      let out_dir = Filename.concat debug_dir "out" in
      Filename.concat out_dir
        (Std.Path.to_string node.Build_node.package.relative_path)
    in

    (* Step 1: Try to plan the node *)
    match Build_planner.plan_node ~graph:build_graph ~node () with
    | Error err ->
        (* Planning error *)
        send server_pid
          (Worker_pool.Worker
             (Worker_pool.TaskFailed
                {
                  worker = worker_pid;
                  node = task.Worker_pool.node;
                  error = Printf.sprintf "Planning failed: %s" err;
                }))
    | Ok (Build_planner.MissingDependencies { deps; _ }) ->
        (* Can't plan yet - missing dependencies *)
        send server_pid
          (Worker_pool.Worker
             (Worker_pool.RequeueWithDependencies
                { worker = worker_pid; node = task.Worker_pool.node; deps }))
    | Ok (Build_planner.Planned planned_node) -> (
        (* Step 2: Check if we have a cached artifact for this node *)
        let store =
          Store.create ~root_dir:(Std.Path.to_string workspace.root)
        in

        match Store.get store planned_node with
        | Some artifact -> (
            (* We have a cached artifact - promote it and we're done *)
            Log.cache_hit ?sid:session_id ~package:pkg_name
              ~hash:
                (match planned_node.Build_node.spec with
                | Planned { hash; _ } -> Hasher.to_string hash
                | _ -> "unknown");

            match Store.promote store artifact ~target_dir with
            | Ok () ->
                send server_pid
                  (Worker_pool.Worker
                     (Worker_pool.TaskCompleted
                        {
                          worker = worker_pid;
                          node = task.Worker_pool.node;
                          artifact;
                        }))
            | Error err ->
                send server_pid
                  (Worker_pool.Worker
                     (Worker_pool.TaskFailed
                        {
                          worker = worker_pid;
                          node = task.Worker_pool.node;
                          error =
                            Printf.sprintf "Failed to promote from cache: %s"
                              err;
                        })))
        | None -> (
            (* No cache - need to build in sandbox *)
            Log.cache_miss ?sid:session_id ~package:pkg_name
              ~hash:
                (match planned_node.Build_node.spec with
                | Planned { hash; _ } -> Hasher.to_string hash
                | _ -> "unknown");

            (* Create sandbox *)
            let sandbox = Sandbox.create ~node:planned_node ~workspace in

            (* Run actions in sandbox *)
            let result =
              Sandbox.run_actions ~sandbox ~node:planned_node ~session_id
            in

            (* Handle result *)
            match result with
            | Ok outs -> (
                (* Save to store *)
                let sandbox_dir = Sandbox.get_sandbox_dir sandbox in
                match Store.save store planned_node ~sandbox_dir ~outs with
                | Ok artifact -> (
                    (* Promote the artifact to the target directory *)
                    match Store.promote store artifact ~target_dir with
                    | Ok () ->
                        (* Clean up sandbox *)
                        Sandbox.cleanup sandbox;

                        send server_pid
                          (Worker_pool.Worker
                             (Worker_pool.TaskCompleted
                                {
                                  worker = worker_pid;
                                  node = task.Worker_pool.node;
                                  artifact;
                                }))
                    | Error err ->
                        (* Clean up sandbox *)
                        Sandbox.cleanup sandbox;

                        send server_pid
                          (Worker_pool.Worker
                             (Worker_pool.TaskFailed
                                {
                                  worker = worker_pid;
                                  node = task.Worker_pool.node;
                                  error =
                                    Printf.sprintf
                                      "Failed to promote artifacts: %s" err;
                                })))
                | Error err ->
                    (* Clean up sandbox *)
                    Sandbox.cleanup sandbox;

                    send server_pid
                      (Worker_pool.Worker
                         (Worker_pool.TaskFailed
                            {
                              worker = worker_pid;
                              node = task.Worker_pool.node;
                              error =
                                Printf.sprintf "Failed to save artifacts: %s"
                                  err;
                            })))
            | Error error_msg ->
                (* Clean up sandbox *)
                Sandbox.cleanup sandbox;

                send server_pid
                  (Worker_pool.Worker
                     (Worker_pool.TaskFailed
                        {
                          worker = worker_pid;
                          node = task.Worker_pool.node;
                          error = error_msg;
                        }))))
  in

  worker_loop ()
