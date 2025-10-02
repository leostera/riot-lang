(** Build worker - executes build tasks in sandboxes *)

open Std
open Miniriot
open Core

let selector msg =
  match msg with Worker_pool_types.Task task -> `select task | _ -> `skip

let rec worker_loop (ctx : Worker_pool_types.ctx) =
  let worker_pid = self () in
  (* Tell server we're ready for work *)
  send ctx.server_pid
    (Worker_pool_types.Worker (Worker_pool_types.WorkerReady worker_pid));
  let task = receive ~selector () in
  handle_task ctx task

and handle_task ctx task =
  let Worker_pool_types.{ node; session_id } = task in
  let pkg_name = Build_node.(node.package.name) in

  (* Log that we're starting *)
  Tusk_log.package_started ~session_id ~package:pkg_name;

  (* Step 1: Create sandbox first *)
  let sandbox = Sandbox.create ~node ~workspace:ctx.workspace in

  (* Step 2: Try to plan the node with the sandbox *)
  (* Pass build_results so Build_planner knows which deps are already built *)
  match
    Build_planner.plan_node ~graph:ctx.build_graph ~node
      ~build_results:ctx.build_results ~workspace:ctx.workspace ~session_id
      ~sandbox ()
  with
  | exception exn -> handle_planning_exception ctx task exn
  | Error err -> handle_planning_error ctx task err
  | Ok (Build_planner.MissingDependencies { deps; _ }) ->
      handle_missing_deps ctx task deps
  | Ok (Build_planner.Skipped { node = skipped_node; reason }) ->
      handle_skipped_node ctx task skipped_node reason
  | Ok (Build_planner.Planned planned_node) ->
      handle_planned_node ctx task planned_node sandbox

and handle_planning_exception ctx task exn =
  (* Planning exception *)
  send ctx.server_pid
    (Worker_pool_types.Worker
       (Worker_pool_types.TaskFailed
          {
            worker = self ();
            node = task.Worker_pool_types.node;
            error =
              format "Planning failed: %s" (Exception.to_string exn);
          }));
  worker_loop ctx

and handle_planning_error ctx task err =
  (* Planning error *)
  send ctx.server_pid
    (Worker_pool_types.Worker
       (Worker_pool_types.TaskFailed
          {
            worker = self ();
            node = task.Worker_pool_types.node;
            error = format "Planning failed: %s" err;
          }));
  worker_loop ctx

and handle_missing_deps ctx task deps =
  (* Dependencies not ready, request requeue *)
  send ctx.server_pid
    (Worker_pool_types.Worker
       (Worker_pool_types.RequeueWithDependencies
          { worker = self (); node = task.Worker_pool_types.node; deps }));
  worker_loop ctx

and handle_skipped_node ctx task skipped_node reason =
  let Worker_pool_types.{ session_id; _ } = task in
  let pkg_name = Build_node.(skipped_node.package.name) in

  (* Convert planner skip reason to event skip reason *)
  let event_reason =
    match reason with
    | Build_planner.DependenciesFailed dep_errors ->
        Event.DependenciesFailed dep_errors
  in

  (* Log that the package was skipped with the proper event *)
  Tusk_log.log
    (Event.create ~session_id ~level:Info
       (PackageSkipped { package = pkg_name; reason = event_reason }));

  (* Format the skip reason for build_results *)
  let reason_str =
    match reason with
    | Build_planner.DependenciesFailed dep_errors ->
        format "Dependencies failed: %s" (String.concat ", " dep_errors)
  in

  (* Mark as failed in build results with skip reason *)
  Build_results.mark_failed ctx.build_results skipped_node ~error:reason_str;

  (* Send TaskFailed message to server *)
  send ctx.server_pid
    (Worker_pool_types.Worker
       (Worker_pool_types.TaskFailed
          {
            worker = self ();
            node = task.Worker_pool_types.node;
            error = reason_str;
          }));
  worker_loop ctx

and handle_planned_node ctx task planned_node sandbox =
  (* Step 2: Check if we have a cached artifact for this node *)
  match Store.get ctx.store planned_node with
  | Some artifact -> handle_cache_hit ctx task planned_node artifact
  | None -> do_build ctx task planned_node sandbox

and do_build ctx task planned_node sandbox =
  let Worker_pool_types.{ node; session_id } = task in
  let pkg_name = Build_node.(node.package.name) in
  (* No cache - need to build in sandbox *)
  Tusk_log.cache_miss ~session_id ~package:pkg_name
    ~hash:
      (match planned_node.Build_node.spec with
      | Planned { hash; _ } -> Std.Crypto.Digest.hex hash
      | _ -> "unknown");

  (* Use sandbox from planner (already contains copied sources) *)
  (* Run actions in sandbox *)
  let result =
    Sandbox.run_actions ~sandbox ~store:ctx.store ~build_graph:ctx.build_graph
      ~build_results:ctx.build_results ~node:planned_node ~session_id
  in

  (* Handle result *)
  match result with
  | Ok outs ->
      (* Save to store *)
      let artifact =
        Store.save ctx.store planned_node ~outs
          ~sandbox_dir:(Sandbox.get_sandbox_dir sandbox)
        |> Result.expect ~msg:"Could not save artifact!"
      in
      let target_dir =
        Std.Path.(
          ctx.workspace.root / Path.v "target" / Path.v "debug" / Path.v "out"
          / Path.v "packages"
          / Path.v planned_node.package.name)
      in
      let () =
        Store.promote ctx.store artifact ~target_dir
        |> Result.expect ~msg:"Could not promote artifact!"
      in
      Sandbox.cleanup sandbox;

      send ctx.server_pid
        (Worker_pool_types.Worker
           (Worker_pool_types.TaskCompleted
              { worker = self (); node = planned_node; artifact }));

      worker_loop ctx
  | Error error_msg ->
      Sandbox.cleanup sandbox;

      send ctx.server_pid
        (Worker_pool_types.Worker
           (Worker_pool_types.TaskFailed
              {
                worker = self ();
                node = task.Worker_pool_types.node;
                error = error_msg;
              }));
      worker_loop ctx

and handle_cache_hit ctx task planned_node artifact =
  let Worker_pool_types.{ node; session_id } = task in
  let pkg_name = Build_node.(node.package.name) in
  (* We have a cached artifact *)
  (* Only log cache hit once per package *)
  Tusk_log.cache_hit ~session_id ~package:pkg_name
    ~hash:
      (match planned_node.Build_node.spec with
      | Planned { hash; _ } -> Std.Crypto.Digest.hex hash
      | _ -> "unknown");

  let target_dir =
    Std.Path.(
      ctx.workspace.root / Path.v "target" / Path.v "debug" / Path.v "out"
      / Path.v "packages"
      / Path.v planned_node.package.name)
  in
  let () =
    Store.promote ctx.store artifact ~target_dir
    |> Result.expect ~msg:"Could not promote artifact!"
  in
  (* No sandbox to cleanup in cache hit case *)
  send ctx.server_pid
    (Worker_pool_types.Worker
       (Worker_pool_types.TaskCompleted
          { worker = self (); node = planned_node; artifact }));
  worker_loop ctx

(** Main worker loop *)
let main ctx () = worker_loop ctx
