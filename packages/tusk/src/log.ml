(** Structured logging system for tusk - simplified version *)

type session_id = Session_id.t
type format = Human | Json | Quiet
type level = Error | Warn | Info | Debug | Trace

type build_error = {
  package : string;
  file : string;
  line : int;
  column : int option;
  message : string;
  hint : string option;
}

type build_result = {
  package : string;
  success : bool;
  duration_ms : int;
  modules_compiled : int;
  cache_hits : int;
  cache_misses : int;
  errors : build_error list;
}

type log_event =
  (* Build lifecycle *)
  | BuildStarted of {
      packages : string list;
      total_modules : int;
      workers : int;
    }
  | BuildComplete of {
      duration_ms : int;
      results : build_result list;
      succeeded : string list;
      failed : string list;
    }
  | PackageStarted of { package : string }
  | PackageComplete of build_result
  | CompileError of build_error
  (* Cache events *)
  | CacheHit of { package : string; hash : string }
  | CacheMiss of { package : string; hash : string }
  | CacheStored of { package : string; hash : string; artifacts : string list }
  (* Worker pool events *)
  | WorkerPoolStarted of { workers : int }
  | WorkerStarted of { worker_id : Worker_id.t }
  | WorkerAssigned of { worker_id : Worker_id.t; package : string }
  | WorkerIdle of { worker_id : Worker_id.t }
  (* Server events *)
  | ServerStarted of { pid : string }
  | ServerScanning of { root : string }
  | ServerReady of { packages : int; toolchain : string }
  | ServerShutdown
  (* Build queue events *)
  | QueuePackage of { package : string; queue_type : [ `Ready | `Waiting ] }
  | QueueStats of { ready : int; waiting : int; busy : int }
  (* Dependency events *)
  | DependencyMissing of { package : string; missing : string list }
  | DependencySatisfied of { package : string }
  (* Compilation events *)
  | CompilingInterface of { package : string; file : string }
  | CompilingImplementation of { package : string; file : string }
  | LinkingLibrary of { package : string; output : string }
  | LinkingExecutable of { package : string; output : string }
  (* Hash computation *)
  | ComputingHash of { package : string }
  | HashComputed of { package : string; hash : string }
  (* File operations *)
  | CopyingFile of { source : string; dest : string }
  | WritingFile of { path : string }
  | CreatingDirectory of { path : string }
  (* RPC/MCP events *)
  | RpcRequestReceived of { session_id : Session_id.t; request_type : string }
  | RpcResponseSent of { session_id : Session_id.t; success : bool }
  | McpToolCall of {
      session_id : Session_id.t;
      tool : string;
      args : string; (* JSON string *)
    }
  (* Generic messages - only for legacy/transition *)
  | Info of string
  | Debug of string
  | Warn of string
  | Error of string

(** Get current timestamp as milliseconds since Unix epoch *)
let get_timestamp_ms () =
  int_of_float (Unix.gettimeofday () *. 1000.)

(** Format timestamp for display *)
let format_timestamp timestamp_ms =
  let timestamp_s = float_of_int timestamp_ms /. 1000. in
  let tm = Unix.localtime timestamp_s in
  Printf.sprintf "%02d:%02d:%02d.%03d"
    tm.tm_hour tm.tm_min tm.tm_sec
    (timestamp_ms mod 1000)

(** Get the machine-readable event name from log_event *)
let event_name = function
  | BuildStarted _ -> "tusk.build.started"
  | BuildComplete _ -> "tusk.build.completed" 
  | PackageStarted _ -> "tusk.build.package.started"
  | PackageComplete _ -> "tusk.build.package.completed"
  | CompileError _ -> "tusk.build.compile.error"
  | CacheHit _ -> "tusk.build.cache.hit"
  | CacheMiss _ -> "tusk.build.cache.miss"
  | CacheStored _ -> "tusk.build.cache.stored"
  | WorkerPoolStarted _ -> "tusk.build.workers.started"
  | WorkerStarted _ -> "tusk.build.worker.started"
  | WorkerAssigned _ -> "tusk.build.worker.assigned"
  | WorkerIdle _ -> "tusk.build.worker.idle"
  | ServerStarted _ -> "tusk.server.started"
  | ServerScanning _ -> "tusk.server.scanning"
  | ServerReady _ -> "tusk.server.ready"
  | ServerShutdown -> "tusk.server.shutdown"
  | QueuePackage _ -> "tusk.build.queue.package"
  | QueueStats _ -> "tusk.build.queue.stats"
  | DependencyMissing _ -> "tusk.build.dependency.missing"
  | DependencySatisfied _ -> "tusk.build.dependency.satisfied"
  | CompilingInterface _ -> "tusk.build.compile.interface"
  | CompilingImplementation _ -> "tusk.build.compile.implementation"
  | LinkingLibrary _ -> "tusk.build.link.library"
  | LinkingExecutable _ -> "tusk.build.link.executable"
  | ComputingHash _ -> "tusk.build.hash.computing"
  | HashComputed _ -> "tusk.build.hash.computed"
  | CopyingFile _ -> "tusk.file.copy"
  | WritingFile _ -> "tusk.file.write"
  | CreatingDirectory _ -> "tusk.file.mkdir"
  | RpcRequestReceived _ -> "tusk.rpc.request.received"
  | RpcResponseSent _ -> "tusk.rpc.response.sent"
  | McpToolCall _ -> "tusk.mcp.tool.call"
  | Info _ -> "tusk.info"
  | Debug _ -> "tusk.debug"
  | Warn _ -> "tusk.warn"
  | Error _ -> "tusk.error"

(** Level constants to avoid name collision with log_event constructors *)
let level_info = (Info : level)
let level_error = (Error : level) 
let level_warn = (Warn : level)
let level_debug = (Debug : level)

(** Get the log level from log_event *)
let event_level = function
  | BuildStarted _ | BuildComplete _ | PackageStarted _ | PackageComplete _ 
  | CacheHit _ | CacheMiss _ | ServerStarted _ | ServerReady _ 
  | CompilingInterface _ | CompilingImplementation _ 
  | LinkingLibrary _ | LinkingExecutable _ | HashComputed _ -> level_info
  | CompileError _ | (Error _) -> level_error
  | (Warn _) -> level_warn  
  | (Debug _) -> level_debug
  | _ -> level_info

(** Get the human-readable message from log_event (without timestamp) *)
let event_message = function
  | BuildStarted { packages; total_modules; workers } ->
      Printf.sprintf "Building %d packages (%d modules) with %d workers"
        (List.length packages) total_modules workers
  | BuildComplete { duration_ms; succeeded; failed } ->
      Printf.sprintf "Build completed in %.2fs: %d succeeded, %d failed"
        (float_of_int duration_ms /. 1000.)
        (List.length succeeded) (List.length failed)
  | PackageStarted { package } -> 
      Printf.sprintf "Building %s..." package
  | PackageComplete res ->
      Printf.sprintf "%s completed (%.2fs, %d modules, %d/%d cache hits)"
        res.package
        (float_of_int res.duration_ms /. 1000.)
        res.modules_compiled res.cache_hits
        (res.cache_hits + res.cache_misses)
  | CompileError error ->
      Printf.sprintf "%s:%d:%s - %s" error.file error.line
        (match error.column with Some c -> string_of_int c | None -> "0")
        error.message
  | CacheHit { package; _ } -> Printf.sprintf "Cached %s" package
  | CacheMiss { package; _ } -> Printf.sprintf "Cache miss: %s" package
  | CacheStored { package; artifacts } ->
      Printf.sprintf "Cached %s (%d artifacts)" package (List.length artifacts)
  | WorkerPoolStarted { workers } ->
      Printf.sprintf "Started %d workers" workers
  | WorkerStarted { worker_id } ->
      Printf.sprintf "Worker %s started" (Worker_id.to_string worker_id)
  | WorkerAssigned { worker_id; package } ->
      Printf.sprintf "Worker %s building %s" (Worker_id.to_string worker_id) package
  | WorkerIdle { worker_id } -> 
      Printf.sprintf "Worker %s idle" (Worker_id.to_string worker_id)
  | ServerStarted { pid } -> Printf.sprintf "Server started (pid: %s)" pid
  | ServerScanning { root } -> Printf.sprintf "Scanning workspace: %s" root
  | ServerReady { packages; toolchain } ->
      Printf.sprintf "Ready with %d packages (toolchain: %s)" packages toolchain
  | ServerShutdown -> "Server shutting down"
  | QueuePackage { package; queue_type } ->
      let typ = match queue_type with `Ready -> "ready" | `Waiting -> "waiting" in
      Printf.sprintf "Queued %s (%s)" package typ
  | QueueStats { ready; waiting; busy } ->
      Printf.sprintf "Queue: %d ready, %d waiting, %d busy" ready waiting busy
  | DependencyMissing { package; missing } ->
      Printf.sprintf "%s missing dependencies: %s" package (String.concat ", " missing)
  | DependencySatisfied { package } ->
      Printf.sprintf "%s dependencies satisfied" package
  | CompilingInterface { package; file } ->
      Printf.sprintf "Compiling interface %s in %s" file package
  | CompilingImplementation { package; file } ->
      Printf.sprintf "Compiling implementation %s in %s" file package
  | LinkingLibrary { package; output } ->
      Printf.sprintf "Linking library %s -> %s" package output
  | LinkingExecutable { package; output } ->
      Printf.sprintf "Linking executable %s -> %s" package output
  | ComputingHash { package } ->
      Printf.sprintf "Computing hash for %s" package
  | HashComputed { package; hash } ->
      Printf.sprintf "Hash computed for %s: %s" package hash
  | CopyingFile { source; dest } ->
      Printf.sprintf "Copying %s -> %s" source dest
  | WritingFile { path } ->
      Printf.sprintf "Writing file %s" path
  | CreatingDirectory { path } ->
      Printf.sprintf "Creating directory %s" path
  | RpcRequestReceived { request_type; _ } ->
      Printf.sprintf "RPC request received: %s" request_type
  | RpcResponseSent { success; _ } ->
      Printf.sprintf "RPC response sent: %s" (if success then "success" else "error")
  | McpToolCall { tool; _ } ->
      Printf.sprintf "MCP tool call: %s" tool
  | Info msg -> msg
  | Debug msg -> msg
  | Warn msg -> msg
  | Error msg -> msg


(** Convert log event to human-readable string with timestamp *)
let event_to_string = function
  | BuildStarted { packages; total_modules; workers } ->
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] 🔨 Building %d packages (%d modules) with %d workers"
        timestamp (List.length packages) total_modules workers
  | BuildComplete { duration_ms; succeeded; failed } ->
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      let status = if failed = [] then "✅" else "❌" in
      Printf.sprintf "[%s] %s Build completed in %.2fs: %d succeeded, %d failed"
        timestamp status
        (float_of_int duration_ms /. 1000.)
        (List.length succeeded) (List.length failed)
  | PackageStarted { package } -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s]   📦 Building %s..." timestamp package
  | PackageComplete res ->
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      let status = if res.success then "✓" else "✗" in
      Printf.sprintf "[%s]   %s %s (%.2fs, %d modules, %d/%d cache hits)" timestamp status
        res.package
        (float_of_int res.duration_ms /. 1000.)
        res.modules_compiled res.cache_hits
        (res.cache_hits + res.cache_misses)
  | CompileError error ->
      Printf.sprintf "  ❌ %s:%d:%s - %s" error.file error.line
        (match error.column with Some c -> string_of_int c | None -> "0")
        error.message
  | CacheHit { package; hash } -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s]   ⚡ Cached %s" timestamp package
  | CacheMiss { package; hash } -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s]   💾 Cache miss: %s" timestamp package
  | CacheStored { package; hash; artifacts } ->
      Printf.sprintf "  💾 Cached %s (%d artifacts)" package
        (List.length artifacts)
  | WorkerPoolStarted { workers } ->
      Printf.sprintf "  ⚙️ Started %d workers" workers
  | WorkerStarted { worker_id } ->
      Printf.sprintf "  ⚙️ Worker %s started" (Worker_id.to_string worker_id)
  | WorkerAssigned { worker_id; package } ->
      Printf.sprintf "  ⚙️ Worker %s building %s" (Worker_id.to_string worker_id) package
  | WorkerIdle { worker_id } -> Printf.sprintf "  ⚙️ Worker %s idle" (Worker_id.to_string worker_id)
  | ServerStarted { pid } -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] 🚀 Server started (pid: %s)" timestamp pid
  | ServerScanning { root } -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] 🔍 Scanning workspace: %s" timestamp root
  | ServerReady { packages; toolchain } ->
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] ✅ Server ready (%d packages, toolchain: %s)" timestamp packages
        toolchain
  | ServerShutdown -> "🛑 Server shutting down"
  | QueuePackage { package; queue_type } ->
      let queue_str =
        match queue_type with `Ready -> "ready" | `Waiting -> "waiting"
      in
      Printf.sprintf "  🔄 Queueing %s (%s)" package queue_str
  | QueueStats { ready; waiting; busy } ->
      Printf.sprintf "  📊 Queue: %d ready, %d waiting, %d busy" ready waiting
        busy
  | DependencyMissing { package; missing } ->
      Printf.sprintf "  ⏳ %s waiting for: %s" package
        (String.concat ", " missing)
  | DependencySatisfied { package } ->
      Printf.sprintf "  ✅ Dependencies satisfied for %s" package
  | CompilingInterface { package; file } ->
      Printf.sprintf "  ⚙️ Compiling interface %s:%s" package file
  | CompilingImplementation { package; file } ->
      Printf.sprintf "  ⚙️ Compiling implementation %s:%s" package file
  | LinkingLibrary { package; output } ->
      Printf.sprintf "  🔗 Linking library %s -> %s" package output
  | LinkingExecutable { package; output } ->
      Printf.sprintf "  🔗 Linking executable %s -> %s" package output
  | ComputingHash { package } ->
      Printf.sprintf "  🔢 Computing hash for %s" package
  | HashComputed { package; hash } ->
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s]   🔢 Hash computed for %s: %s" timestamp package
        (String.sub hash 0 (min 8 (String.length hash)))
  | Info msg -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] [INFO] %s" timestamp msg
  | Debug msg -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] [DEBUG] %s" timestamp msg
  | Warn msg -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] [WARN] %s" timestamp msg
  | Error msg -> 
      let timestamp = format_timestamp (get_timestamp_ms ()) in
      Printf.sprintf "[%s] [ERROR] %s" timestamp msg
  | _ -> "" (* Fallback for any remaining unimplemented events *)

(** Format event according to format type *)
let format_event format event =
  match format with
  | Json ->
      failwith
        "JSON formatting should be handled by the handler, not the core log \
         module"
  | Human -> event_to_string event
  | Quiet -> ""

(** Handler types for different log outputs *)
type handler =
  | File of { path : string; format : format }
  | Rpc of {
      session_id : Session_id.t;
      client : Miniriot.Pid.t;
      format : format;
    }
  | Stdout of { format : format }
  | Memory of { session_id : Session_id.t; buffer : Buffer.t }

(** Extend Miniriot's Message.t with logger messages *)
type Miniriot.Message.t +=
  | Log of Session_id.t option * log_event
  | AddHandler of handler
  | RemoveHandler of Session_id.t
  | GetLogs of Session_id.t * format * Miniriot.Pid.t
  | LoggerShutdown
  | LogsRetrieved of string
  | BuildEvent of string

type logger_state = {
  handlers : (Session_id.t, handler) Hashtbl.t;
  memory_logs : (Session_id.t, log_event list) Hashtbl.t;
  stdout_handler : format option;
}
(** Logger process state *)

(** Global logger PID - set during init *)
let logger_pid = ref None

(** Logger actor process *)
let rec logger_loop state =
  let open Miniriot in
  (* Create selector function for message handling *)
  let selector msg =
    match msg with
    | Log (sid, event) -> `select (`log (sid, event))
    | AddHandler h -> `select (`add_handler h)
    | RemoveHandler sid -> `select (`remove_handler sid)
    | GetLogs (sid, fmt, client) -> `select (`get_logs (sid, fmt, client))
    | LoggerShutdown -> `select `shutdown
    | _ -> `skip
  in

  (* Receive and handle messages *)
  match receive ~selector () with
  | `log (sid, event) ->
      (* Log to stdout if handler exists *)
      (match state.stdout_handler with
      | Some format ->
          let output = format_event format event in
          if output <> "" then Printf.printf "%s\n" output;
          flush stdout
      | None -> ());

      (* Log to session-specific handlers *)
      (match sid with
      | Some session_id -> (
          (* Store in memory logs *)
          let logs =
            try Hashtbl.find state.memory_logs session_id with Not_found -> []
          in
          Hashtbl.replace state.memory_logs session_id (event :: logs);

          (* Send to RPC handler if exists *)
          try
            match Hashtbl.find state.handlers session_id with
            | Rpc { client; format; _ } ->
                (* Send log event to RPC client *)
                let response =
                  Rpc.BuildEvent { session_id; log_event = (event : log_event) }
                in
                send client (Rpc.ServerResponse response)
            | _ -> ()
          with Not_found -> ())
      | None -> ());

      logger_loop state
  | `add_handler handler ->
      let new_state =
        match handler with
        | Stdout { format } -> { state with stdout_handler = Some format }
        | Rpc { session_id; _ } | Memory { session_id; _ } ->
            Hashtbl.replace state.handlers session_id handler;
            state
        | File _ -> state (* TODO: implement file handler *)
      in
      logger_loop new_state
  | `remove_handler sid ->
      Hashtbl.remove state.handlers sid;
      Hashtbl.remove state.memory_logs sid;
      logger_loop state
  | `get_logs (sid, format, client) ->
      let logs =
        try List.rev (Hashtbl.find state.memory_logs sid) with Not_found -> []
      in
      let formatted =
        String.concat "\n" (List.map (format_event format) logs)
      in
      send client (LogsRetrieved formatted);
      logger_loop state
  | `shutdown ->
      (* Clean shutdown *)
      Process.Normal

(** Initialize the logger process *)
let init () =
  let open Miniriot in
  match !logger_pid with
  | Some pid -> pid (* Already initialized *)
  | None ->
      let pid =
        spawn (fun () ->
            let initial_state =
              {
                handlers = Hashtbl.create 16;
                memory_logs = Hashtbl.create 16;
                stdout_handler = Some Human;
                (* Default to human-readable stdout *)
              }
            in
            logger_loop initial_state)
      in
      logger_pid := Some pid;
      pid

(** Main logging function - sends to logger process *)
let log ?sid event =
  match !logger_pid with
  | Some pid ->
      Miniriot.send pid (Log (sid, event))
  | None ->
      (* Fallback if logger not initialized *)
      Printf.printf "%s\n" (event_to_string event);
      flush stdout

(** Convenience logging functions *)
let info ?sid msg = log ?sid (Info msg)

let debug ?sid msg = log ?sid (Debug msg)
let warn ?sid msg = log ?sid (Warn msg)
let error ?sid msg = log ?sid (Error msg)

(** Build lifecycle logging functions *)
let build_started ?sid ~packages ~total_modules ~workers =
  log ?sid (BuildStarted { packages; total_modules; workers })

let build_complete ?sid ~duration_ms ~results =
  let succeeded =
    List.filter_map
      (fun r -> if r.success then Some r.package else None)
      results
  in
  let failed =
    List.filter_map
      (fun r -> if not r.success then Some r.package else None)
      results
  in
  log ?sid (BuildComplete { duration_ms; results; succeeded; failed })

let package_started ?sid ~package = log ?sid (PackageStarted { package })
let package_complete ?sid result = log ?sid (PackageComplete result)
let compile_error ?sid error = log ?sid (CompileError error)

(** Cache event logging *)
let cache_hit ?sid ~package ~hash = log ?sid (CacheHit { package; hash })

let cache_miss ?sid ~package ~hash = log ?sid (CacheMiss { package; hash })

let cache_stored ?sid ~package ~hash ~artifacts =
  log ?sid (CacheStored { package; hash; artifacts })

(** Hash computation logging *)
let hash_computed ?sid ~package ~hash =
  log ?sid (HashComputed { package; hash })

(** Worker event logging *)
let worker_pool_started ?sid ~workers = log ?sid (WorkerPoolStarted { workers })

let worker_started ?sid ~worker_id = log ?sid (WorkerStarted { worker_id })

let worker_assigned ?sid ~worker_id ~package =
  log ?sid (WorkerAssigned { worker_id; package })

let worker_idle ?sid ~worker_id = log ?sid (WorkerIdle { worker_id })

(** Server event logging *)
let server_started ?sid ~pid = log ?sid (ServerStarted { pid })

let server_scanning ?sid ~root = log ?sid (ServerScanning { root })

let server_ready ?sid ~packages ~toolchain =
  log ?sid (ServerReady { packages; toolchain })

let server_shutdown ?sid () = log ?sid ServerShutdown

(** Queue event logging *)
let queue_package ?sid ~package ~queue_type =
  log ?sid (QueuePackage { package; queue_type })

let queue_stats ?sid ~ready ~waiting ~busy =
  log ?sid (QueueStats { ready; waiting; busy })

(** Dependency event logging *)
let dependency_missing ?sid ~package ~missing =
  log ?sid (DependencyMissing { package; missing })

let dependency_satisfied ?sid ~package =
  log ?sid (DependencySatisfied { package })

(** Compilation event logging *)
let compiling_interface ?sid ~package ~file =
  log ?sid (CompilingInterface { package; file })

let compiling_implementation ?sid ~package ~file =
  log ?sid (CompilingImplementation { package; file })

let linking_library ?sid ~package ~output =
  log ?sid (LinkingLibrary { package; output })

let linking_executable ?sid ~package ~output =
  log ?sid (LinkingExecutable { package; output })

(** Handler management *)
let add_rpc_handler ~sid ~client ~format =
  match !logger_pid with
  | Some pid ->
      Miniriot.send pid (AddHandler (Rpc { session_id = sid; client; format }))
  | None ->
      ()

let add_stdout_handler ~format =
  match !logger_pid with
  | Some pid -> Miniriot.send pid (AddHandler (Stdout { format }))
  | None -> ()

let remove_handler ~sid =
  match !logger_pid with
  | Some pid -> Miniriot.send pid (RemoveHandler sid)
  | None -> ()

(** Retrieve logs for a session *)
let get_session_logs ~sid ~format =
  match !logger_pid with
  | Some pid ->
      let open Miniriot in
      send pid (GetLogs (sid, format, self ()));
      (* Wait for response *)
      let selector msg =
        match msg with LogsRetrieved logs -> `select logs | _ -> `skip
      in
      receive ~selector ()
  | None -> ""
