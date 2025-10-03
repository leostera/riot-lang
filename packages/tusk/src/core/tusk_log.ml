open Std
open Miniriot

(** Logging system for tusk - event distribution and handlers *)

type handler = { session_id : Session_id.t; pid : Pid.t }

type command =
  | Log of Event.t
  | AddHandler of handler
  | RemoveHandler of Session_id.t

(** Extend Miniriot's Message.t with logger messages *)
type Message.t += Logger of command | Event of Event.t

type logger_state = {
  handlers : (Session_id.t, handler) Hashtbl.t;
  stdout_handler : bool;
}

(** Global logger PID - set during init *)
let logger_pid = ref None

(** Logger actor process *)
let rec logger_loop state =
  let selector msg =
    match msg with Logger event -> `select event | _ -> `skip
  in

  (* Receive and handle messages *)
  match receive ~selector () with
  | Log event ->
      (* Log to stdout if handler exists *)
      if state.stdout_handler then println "%s" (Event.to_string event);

      (* Send to session-specific RPC handlers *)
      (match Hashtbl.find_opt state.handlers event.session_id with
      | Some handler -> send handler.pid (Event event)
      | None -> ());

      logger_loop state
  | AddHandler handler ->
      Hashtbl.replace state.handlers handler.session_id handler;
      logger_loop state
  | RemoveHandler sid ->
      Hashtbl.remove state.handlers sid;
      logger_loop state

(** Initialize the logger process *)
let init () =
  match !logger_pid with
  | Some pid -> pid (* Already initialized *)
  | None ->
      let pid =
        spawn (fun () ->
            let initial_state =
              {
                handlers = Hashtbl.create 16;
                stdout_handler = true;
                (* Default to stdout output *)
              }
            in
            logger_loop initial_state)
      in
      logger_pid := Some pid;
      pid

(** Main logging function - sends to logger process *)
let log event =
  match !logger_pid with
  | Some pid -> send pid (Logger (Log event))
  | None ->
      (* Fallback if logger not initialized *)
      println "%s" (Event.to_string event)

(** Convenience logging functions *)

(** Build lifecycle logging functions *)
let build_started ~session_id ~packages ~total_modules ~workers =
  let event =
    Event.create ~session_id ~level:Info
      (BuildStarted { packages; total_modules; workers })
  in
  log event

let build_complete ~session_id ~duration_ms ~results =
  let succeeded =
    List.filter_map
      (fun r -> if r.Event.success then Some r.Event.package else None)
      results
  in
  let failed =
    List.filter_map
      (fun r -> if not r.Event.success then Some r.Event.package else None)
      results
  in
  let event =
    Event.create ~session_id ~level:Info
      (BuildComplete { duration_ms; results; succeeded; failed })
  in
  log event

let package_started ~session_id ~package =
  let event =
    Event.create ~session_id ~level:Info (PackageStarted { package })
  in
  log event

let package_complete ~session_id result =
  let event = Event.create ~session_id ~level:Info (PackageComplete result) in
  log event

let compile_error ~session_id ~package error =
  let event =
    Event.create ~session_id ~level:Error (CompileError { package; error })
  in
  log event

(** Cache event logging *)
let cache_hit ~session_id ~package ~hash =
  let event =
    Event.create ~session_id ~level:Info (CacheHit { package; hash })
  in
  log event

let cache_miss ~session_id ~package ~hash =
  let event =
    Event.create ~session_id ~level:Info (CacheMiss { package; hash })
  in
  log event

let cache_stored ~session_id ~package ~hash ~artifacts =
  let event =
    Event.create ~session_id ~level:Debug
      (CacheStored { package; hash; artifacts })
  in
  log event

(** Hash computation logging *)
let computing_hash ~session_id ~package =
  let event =
    Event.create ~session_id ~level:Debug (ComputingHash { package })
  in
  log event

let hash_computed ~session_id ~package ~hash ~duration_ms =
  let event =
    Event.create ~session_id ~level:Info (HashComputed { package; hash })
  in
  log event

(** Store event logging *)
let store_creating ~session_id () =
  let event = Event.create ~session_id ~level:Info StoreCreating in
  log event

let store_created ~session_id ~duration_ms =
  let event =
    Event.create ~session_id ~level:Info (StoreCreated { duration_ms })
  in
  log event

(** Worker event logging *)
let worker_pool_creating ~session_id ~workers =
  let event =
    Event.create ~session_id ~level:Info (WorkerPoolCreating { workers })
  in
  log event

let worker_pool_created ~session_id ~workers ~duration_ms =
  let event =
    Event.create ~session_id ~level:Info
      (WorkerPoolCreated { workers; duration_ms })
  in
  log event

let worker_pool_started ~session_id ~workers =
  let event =
    Event.create ~session_id ~level:Debug (WorkerPoolStarted { workers })
  in
  log event

let worker_started ~session_id ~worker_id =
  let event =
    Event.create ~session_id ~level:Debug (WorkerStarted { worker_id })
  in
  log event

let worker_assigned ~session_id ~worker_id ~package =
  let event =
    Event.create ~session_id ~level:Debug
      (WorkerAssigned { worker_id; package })
  in
  log event

let worker_idle ~session_id ~worker_id =
  let event =
    Event.create ~session_id ~level:Debug (WorkerIdle { worker_id })
  in
  log event

(** Server event logging *)
let server_started ~session_id ~pid =
  let event = Event.create ~session_id ~level:Info (ServerStarted { pid }) in
  log event

let server_scanning ~session_id ~root =
  let event = Event.create ~session_id ~level:Debug (ServerScanning { root }) in
  log event

let server_restarted ~session_id ~packages ~toolchain =
  let event =
    Event.create ~session_id ~level:Info
      (ServerRestarted { packages; toolchain })
  in
  log event

let workspace_empty ~session_id () =
  let event = Event.create ~session_id ~level:Info WorkspaceEmpty in
  log event

let workspace_scanning ~session_id () =
  let event = Event.create ~session_id ~level:Info WorkspaceScanning in
  log event

let workspace_scanned ~session_id ~packages ~duration_ms =
  let event =
    Event.create ~session_id ~level:Info
      (WorkspaceScanned { packages; duration_ms })
  in
  log event

let build_graph_creating ~session_id () =
  let event = Event.create ~session_id ~level:Info BuildGraphCreating in
  log event

let build_graph_created ~session_id ~nodes ~duration_ms =
  let event =
    Event.create ~session_id ~level:Info
      (BuildGraphCreated { nodes; duration_ms })
  in
  log event

let server_shutdown ~session_id () =
  let event = Event.create ~session_id ~level:Info ServerShutdown in
  log event

(** Queue event logging *)
let queue_package ~session_id ~package ~queue_type =
  let event =
    Event.create ~session_id ~level:Debug (QueuePackage { package; queue_type })
  in
  log event

let queue_stats ~session_id ~ready ~waiting ~busy =
  let event =
    Event.create ~session_id ~level:Debug (QueueStats { ready; waiting; busy })
  in
  log event

(** Dependency event logging *)
let dependency_missing ~session_id ~package ~missing =
  let event =
    Event.create ~session_id ~level:Debug
      (DependencyMissing { package; missing })
  in
  log event

let dependency_satisfied ~session_id ~package =
  let event =
    Event.create ~session_id ~level:Debug (DependencySatisfied { package })
  in
  log event

(** Compilation event logging *)
let compiling_interface ~session_id ~package ~file =
  let event =
    Event.create ~session_id ~level:Debug (CompilingInterface { package; file })
  in
  log event

let compiling_implementation ~session_id ~package ~file =
  let event =
    Event.create ~session_id ~level:Debug
      (CompilingImplementation { package; file })
  in
  log event

let linking_library ~session_id ~package ~output =
  let event =
    Event.create ~session_id ~level:Debug (LinkingLibrary { package; output })
  in
  log event

let linking_executable ~session_id ~package ~output =
  let event =
    Event.create ~session_id ~level:Debug
      (LinkingExecutable { package; output })
  in
  log event

(** Handler management *)
let add_rpc_handler ~session_id ~client =
  match !logger_pid with
  | Some pid -> send pid (Logger (AddHandler { session_id; pid = client }))
  | None -> ()

let add_stdout_handler () =
  (* Stdout handler is enabled by default in initial_state *)
  ()

let remove_handler ~session_id =
  match !logger_pid with
  | Some pid -> send pid (Logger (RemoveHandler session_id))
  | None -> ()
