(** Build server - Miniriot process that orchestrates builds *)

open Miniriot

type target =
  | All
  | Package of string

(** Internal server messages *)
type request = | Build of { client_pid : Pid.t; target: target } | Ping | ScanWorkspace of { client_pid: Pid.t; current_dir: Path.t } (* optional target package to filter for *)

type response = | Pong | BuildCompleted

type Message.t +=
  | ServerRequest of request
  | ServerResponse of response


type state = {
  active_build_graph : Build_graph.t; (* Graph for current build (full or filtered) *)
  build_graph : Build_graph.t; (* Full workspace build graph *)
  build_queue : Build_queue.t; (* Two-queue system for dependency ordering *)
  build_results : Build_results.t;
  build_stats : Build_stats.t; (* Stats of the build *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
  worker_pool : Worker_pool.t; (* Handle to the worker pool *)
  workspace : Workspace.t;
}
(** Server state *)

(**
  this is the main server loop where we'll handle all the incoming requests
    from clients, be it the CLI, MCP, LSP, or direct RPC communication.

    None of these handlers can really block the loop, so we gotta handle and
    dispatch, except restart and shutdown
*)
let rec loop state = 
 
  match receive ~selector () with
  | Ping { client_pid } -> handle_ping state client_pid
  | Build { client_pid; target } -> handle_build state client_pid target

(** 
    Handler for the ping message.

*)
and handle_ping client_pid state = 
  send client_pid (ServerResponse Pong);
  loop state


(** 
    Handler for the build message.

*)
and handle_build state client_pid target =
  let server_pid = self () in
  let _ = spawn (fun () ->

    (* 1. on every build we refresh the workspace *)
    let workspace = Workspace_manager.scan state.workspace.root |> Result.unwrap in

    (* 2. compute and queue the target build graph (this could be the whole build graph or a subset) *)
    let target_graph = 
        let build_graph = Build_graph.create workspace state.toolchain |> Result.unwrap in
        match target with
        | All -> state.build_graph
        | Package pkg -> Build_graph.filter_for_package build_graph pkg
    in 
    List.iter (Build_queue.add state.build_queue) (Build_graph.to_list target_graph);

    (* 3. create a worker pool to execute this build *)
    let build_pid = self () in
    let worker_pool = Worker_pool.start ~workers ~provider:build_pid () in

    (* 4. enter the build loop *)
    let selector msg = 
      match msg with
      | Worker_pool.Worker msg -> `select msg
      | _ -> `skip
    in

    let rec build_loop () =
      if Build_results.all_done state.build_results then ()
      else match receive ~selector () with
      | Worker_pool.TaskCompleted {worker; task; artifact} ->
        Build_results.mark_completed state.build_results task artifact;
        build_loop ()
      | Worker_pool.TaskFailed {worker; task; error} -> 
        Build_results.mark_failed state.build_results task error;
        build_loop ()
      | Worker_pool.WorkerReady worker ->
        let () = match Build_queue.next state.build_queue with
          | None -> ()
          | Some task-> Worker_pool.send_task worker task
        in
        build_loop ()
      | Worker_pool.RequeueWithDependencies { worker;task; deps} ->
        Build_queue.queue_with_deps state.build_queue task deps;
        build_loop ()
    in

    Fun.protect 
      ~finally:(fun () -> 
        let stats =
        send client_pid (ServerResponse (BuildCompleted stats))
      )
      (fun () -> build_loop ())
  ) in
  loop state
  



let start_tcp_server ~server ~port =
  spawn @@ fun () ->
  let addr = Addr.(tcp loopback port) in
  let jsonrpc_server = Tusk_jsonrpc.Server.create server in
  let handler ~req stream =
    let reply msg = TcpServer.send stream (msg ^ "\n") |> Result.unwrap in
    Jsonrpc.Server.handle_message jsonrpc_server reply req
  in
  TcpServer.listen addr ~handler

(** Main server loop *)
let init ~current_dir ~workers ~port =
  let server_pid = self () in
  let workspace = Workspace_manager.scan current_dir |> Result.unwrap in
  let toolchain = Toolchains.ready_toolchains workspace |> Result.unwrap in
  let build_graph = Build_graph.create workspace toolchain |> Result.unwrap in
  let build_results = Build_results.create () in
  let build_queue = Build_queue.create build_results in
  let tcp_listener = start_tcp_server ~server ~port in
  let build_stats = Build_stats.empty in

  let state = {
  workspace; toolchain; build_graph; build_results; build_Queue; worker_pool; tcp_listener; build_stats;
  }

  loop state
    

(** Start the server with TCP listener for RPC. This function makes the
    current
  process _become_ the Tusk server and spin up a sepaarate riot process for
  the listening in to tcp requests *)
let start () =
  let server_pid = self () in
  let current_dir = Std.Env.current_dir () |> Result.unwrap in
  let workers = Std.available_parallelism () in
  let port = 9753 in

  init ~current_dir ~workers ~port
