(** Structured logging system for tusk - simplified version *)

type session_id = string

type format = Human | Json | Quiet

type level = Error | Warn | Info | Debug | Trace

type build_error = {
  package: string;
  file: string;
  line: int;
  column: int option;
  message: string;
  hint: string option;
}

type build_result = {
  package: string;
  success: bool;
  duration_ms: int;
  modules_compiled: int;
  cache_hits: int;
  cache_misses: int;
  errors: build_error list;
}

type log_event = 
  (* Build lifecycle *)
  | BuildStarted of { 
      packages: string list; 
      total_modules: int;
      workers: int;
    }
  | BuildComplete of { 
      duration_ms: int;
      results: build_result list;
      succeeded: string list;
      failed: string list;
    }
  | PackageStarted of { 
      package: string;
    }
  | PackageComplete of build_result
  | CompileError of build_error
  
  (* Cache events *)
  | CacheHit of { 
      package: string; 
      hash: string;
    }
  | CacheMiss of { 
      package: string; 
      hash: string;
    }
  | CacheStored of {
      package: string;
      hash: string;
      artifacts: string list;
    }
  
  (* Worker pool events *)
  | WorkerPoolStarted of { 
      workers: int;
    }
  | WorkerStarted of {
      worker_id: int;
    }
  | WorkerAssigned of {
      worker_id: int;
      package: string;
    }
  | WorkerIdle of {
      worker_id: int;
    }
  
  (* Server events *)
  | ServerStarted of {
      pid: string;
    }
  | ServerScanning of {
      root: string;
    }
  | ServerReady of {
      packages: int;
      toolchain: string;
    }
  | ServerShutdown
  
  (* Build queue events *)
  | QueuePackage of {
      package: string;
      queue_type: [`Ready | `Waiting];
    }
  | QueueStats of {
      ready: int;
      waiting: int;
      busy: int;
    }
  
  (* Dependency events *)
  | DependencyMissing of {
      package: string;
      missing: string list;
    }
  | DependencySatisfied of {
      package: string;
    }
  
  (* Compilation events *)
  | CompilingInterface of {
      package: string;
      file: string;
    }
  | CompilingImplementation of {
      package: string;
      file: string;
    }
  | LinkingLibrary of {
      package: string;
      output: string;
    }
  | LinkingExecutable of {
      package: string;
      output: string;
    }
  
  (* Hash computation *)
  | ComputingHash of {
      package: string;
    }
  | HashComputed of {
      package: string;
      hash: string;
    }
  
  (* File operations *)
  | CopyingFile of {
      source: string;
      dest: string;
    }
  | WritingFile of {
      path: string;
    }
  | CreatingDirectory of {
      path: string;
    }
  
  (* RPC/MCP events *)
  | RpcRequestReceived of {
      session_id: string;
      request_type: string;
    }
  | RpcResponseSent of {
      session_id: string;
      success: bool;
    }
  | McpToolCall of {
      session_id: string;
      tool: string;
      args: string; (* JSON string *)
    }
  
  (* Generic messages - only for legacy/transition *)
  | Info of string
  | Debug of string
  | Warn of string
  | Error of string

(** Generate a new session ID *)
let create_session () =
  Printf.sprintf "session-%d-%d" 
    (Unix.getpid ()) 
    (int_of_float (Unix.gettimeofday () *. 1000.))

(** Convert build error to JSON *)
let build_error_to_json (error : build_error) =
  Json.Object [
    ("package", Json.String error.package);
    ("file", Json.String error.file);
    ("line", Json.Int error.line);
    ("column", match error.column with Some c -> Json.Int c | None -> Json.Null);
    ("message", Json.String error.message);
    ("hint", match error.hint with Some h -> Json.String h | None -> Json.Null);
  ]

(** Convert build result to JSON *)
let build_result_to_json res =
  Json.Object [
    ("package", Json.String res.package);
    ("success", Json.Bool res.success);
    ("duration_ms", Json.Int res.duration_ms);
    ("modules_compiled", Json.Int res.modules_compiled);
    ("cache_hits", Json.Int res.cache_hits);
    ("cache_misses", Json.Int res.cache_misses);
    ("errors", Json.Array (List.map build_error_to_json res.errors));
  ]

(** Convert log event to JSON *)
let event_to_json = function
  | BuildStarted { packages; total_modules; workers } ->
      Json.Object [
        ("type", Json.String "build_started");
        ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
        ("total_modules", Json.Int total_modules);
        ("workers", Json.Int workers);
      ]
  | BuildComplete { duration_ms; results; succeeded; failed } ->
      Json.Object [
        ("type", Json.String "build_complete");
        ("duration_ms", Json.Int duration_ms);
        ("results", Json.Array (List.map build_result_to_json results));
        ("succeeded", Json.Array (List.map (fun p -> Json.String p) succeeded));
        ("failed", Json.Array (List.map (fun p -> Json.String p) failed));
      ]
  | PackageStarted { package } ->
      Json.Object [
        ("type", Json.String "package_started");
        ("package", Json.String package);
      ]
  | PackageComplete result ->
      Json.Object [
        ("type", Json.String "package_complete");
        ("result", build_result_to_json result);
      ]
  | CompileError error ->
      Json.Object [
        ("type", Json.String "compile_error");
        ("error", build_error_to_json error);
      ]
  | CacheHit { package; hash } ->
      Json.Object [
        ("type", Json.String "cache_hit");
        ("package", Json.String package);
        ("hash", Json.String hash);
      ]
  | CacheMiss { package; hash } ->
      Json.Object [
        ("type", Json.String "cache_miss");
        ("package", Json.String package);
        ("hash", Json.String hash);
      ]
  | _ -> Json.Object []  (* TODO: implement rest *)

(** Convert log event to human-readable string *)
let event_to_string = function
  | BuildStarted { packages; total_modules; workers } ->
      Printf.sprintf "🔨 Building %d packages (%d modules) with %d workers" 
        (List.length packages) total_modules workers
  | BuildComplete { duration_ms; succeeded; failed } ->
      let status = if failed = [] then "✅" else "❌" in
      Printf.sprintf "%s Build completed in %.2fs: %d succeeded, %d failed"
        status (float_of_int duration_ms /. 1000.) 
        (List.length succeeded) (List.length failed)
  | PackageStarted { package } ->
      Printf.sprintf "  📦 Building %s..." package
  | PackageComplete res ->
      let status = if res.success then "✓" else "✗" in
      Printf.sprintf "  %s %s (%.2fs, %d modules, %d/%d cache hits)"
        status res.package (float_of_int res.duration_ms /. 1000.)
        res.modules_compiled res.cache_hits (res.cache_hits + res.cache_misses)
  | CompileError error ->
      Printf.sprintf "  ❌ %s:%d:%s - %s"
        error.file error.line
        (match error.column with Some c -> string_of_int c | None -> "0")
        error.message
  | Info msg -> Printf.sprintf "[INFO] %s" msg
  | Debug msg -> Printf.sprintf "[DEBUG] %s" msg
  | Warn msg -> Printf.sprintf "[WARN] %s" msg
  | Error msg -> Printf.sprintf "[ERROR] %s" msg
  | _ -> ""  (* TODO: implement rest *)

(** Format event according to format type *)
let format_event format event =
  match format with
  | Json -> Json.to_string (event_to_json event)
  | Human -> event_to_string event
  | Quiet -> ""

(** Handler types for different log outputs *)
type handler =
  | File of { path: string; format: format }
  | Rpc of { session_id: session_id; client: Miniriot.Pid.t; format: format }
  | Stdout of { format: format }
  | Memory of { session_id: session_id; buffer: Buffer.t }

(** Extend Miniriot's Message.t with logger messages *)
type Miniriot.Message.t +=
  | Log of session_id option * log_event
  | AddHandler of handler
  | RemoveHandler of session_id  
  | GetLogs of session_id * format * Miniriot.Pid.t
  | LoggerShutdown
  | LogsRetrieved of string
  | LogOutput of string

(** Logger process state *)
type logger_state = {
  handlers: (session_id, handler) Hashtbl.t;
  memory_logs: (session_id, log_event list) Hashtbl.t;
  stdout_handler: format option;
}

(** Global logger PID - set during init *)
let logger_pid = ref None

(** Logger actor process *)
let rec logger_loop state =
  let open Miniriot in
  (* Create selector function for message handling *)
  let selector msg = match msg with
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
      | Some session_id ->
          (* Store in memory logs *)
          let logs = 
            try Hashtbl.find state.memory_logs session_id
            with Not_found -> []
          in
          Hashtbl.replace state.memory_logs session_id (event :: logs);
          
          (* Send to RPC handler if exists *)
          (try
            match Hashtbl.find state.handlers session_id with
            | Rpc { client; format; _ } ->
                let formatted = format_event format event in
                send client (LogOutput formatted)
            | _ -> ()
          with Not_found -> ())
      | None -> ());
      
      logger_loop state
      
  | `add_handler handler ->
      let new_state = match handler with
        | Stdout { format } ->
            { state with stdout_handler = Some format }
        | Rpc { session_id; _ } | Memory { session_id; _ } ->
            Hashtbl.replace state.handlers session_id handler;
            state
        | File _ -> state  (* TODO: implement file handler *)
      in
      logger_loop new_state
      
  | `remove_handler sid ->
      Hashtbl.remove state.handlers sid;
      Hashtbl.remove state.memory_logs sid;
      logger_loop state
      
  | `get_logs (sid, format, client) ->
      let logs = 
        try List.rev (Hashtbl.find state.memory_logs sid)
        with Not_found -> []
      in
      let formatted = String.concat "\n" 
        (List.map (format_event format) logs) in
      send client (LogsRetrieved formatted);
      logger_loop state
      
  | `shutdown ->
      (* Clean shutdown *)
      Process.Normal

(** Initialize the logger process *)
let init () =
  let open Miniriot in
  match !logger_pid with
  | Some pid -> pid  (* Already initialized *)
  | None ->
      let pid = spawn (fun () ->
        let initial_state = {
          handlers = Hashtbl.create 16;
          memory_logs = Hashtbl.create 16;
          stdout_handler = Some Human;  (* Default to human-readable stdout *)
        } in
        logger_loop initial_state;
        Process.Normal
      ) in
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
  let succeeded = List.filter_map (fun r -> if r.success then Some r.package else None) results in
  let failed = List.filter_map (fun r -> if not r.success then Some r.package else None) results in
  log ?sid (BuildComplete { duration_ms; results; succeeded; failed })

let package_started ?sid ~package =
  log ?sid (PackageStarted { package })

let package_complete ?sid result =
  log ?sid (PackageComplete result)

let compile_error ?sid error =
  log ?sid (CompileError error)

(** Cache event logging *)
let cache_hit ?sid ~package ~hash =
  log ?sid (CacheHit { package; hash })

let cache_miss ?sid ~package ~hash =
  log ?sid (CacheMiss { package; hash })

let cache_stored ?sid ~package ~hash ~artifacts =
  log ?sid (CacheStored { package; hash; artifacts })

(** Worker event logging *)
let worker_pool_started ?sid ~workers =
  log ?sid (WorkerPoolStarted { workers })

let worker_started ?sid ~worker_id =
  log ?sid (WorkerStarted { worker_id })

let worker_assigned ?sid ~worker_id ~package =
  log ?sid (WorkerAssigned { worker_id; package })

let worker_idle ?sid ~worker_id =
  log ?sid (WorkerIdle { worker_id })

(** Server event logging *)
let server_started ?sid ~pid =
  log ?sid (ServerStarted { pid })

let server_scanning ?sid ~root =
  log ?sid (ServerScanning { root })

let server_ready ?sid ~packages ~toolchain =
  log ?sid (ServerReady { packages; toolchain })

let server_shutdown ?sid () =
  log ?sid ServerShutdown

(** Queue event logging *)
let queue_package ?sid ~package ~queue_type =
  log ?sid (QueuePackage { package; queue_type })

let queue_stats ?sid ~ready ~waiting ~busy =
  log ?sid (QueueStats { ready; waiting; busy })

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
  | None -> ()

let add_stdout_handler ~format =
  match !logger_pid with
  | Some pid -> 
      Miniriot.send pid (AddHandler (Stdout { format }))
  | None -> ()

let remove_handler ~sid =
  match !logger_pid with
  | Some pid -> 
      Miniriot.send pid (RemoveHandler sid)
  | None -> ()

(** Retrieve logs for a session *)
let get_session_logs ~sid ~format =
  match !logger_pid with
  | Some pid ->
      let open Miniriot in
      send pid (GetLogs (sid, format, self ()));
      (* Wait for response *)
      let selector msg = match msg with
        | LogsRetrieved logs -> `select logs
        | _ -> `skip
      in
      receive ~selector ()
  | None -> ""