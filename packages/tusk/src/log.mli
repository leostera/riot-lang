(** Actor-based structured logging system for tusk *)

open Miniriot

type session_id = Session_id.t
(** Unique session identifier *)

(** Output format for log messages *)
type format = Human | Json | Quiet

(** Log severity levels *)
type level = Error | Warn | Info | Debug | Trace

type build_error = {
  package : string;
  file : string;
  line : int;
  column : int option;
  message : string;
  hint : string option;
}
(** Build error details *)

type build_result = {
  package : string;
  success : bool;
  duration_ms : int;
  modules_compiled : int;
  cache_hits : int;
  cache_misses : int;
  errors : build_error list;
}
(** Package build result *)

(** Structured log events - ALL log messages as typed values *)
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
  | RpcRequestReceived of { session_id : session_id; request_type : string }
  | RpcResponseSent of { session_id : session_id; success : bool }
  | McpToolCall of {
      session_id : session_id;
      tool : string;
      args : string; (* JSON string *)
    }
  (* Generic messages - only for legacy/transition *)
  | Info of string
  | Debug of string
  | Warn of string
  | Error of string

val init : unit -> Pid.t
(** Initialize the logger process Must be called once at server startup *)

val log : ?sid:session_id -> log_event -> unit
(** Main logging function *)

val info : ?sid:session_id -> string -> unit
(** Convenience logging functions *)

val debug : ?sid:session_id -> string -> unit
val warn : ?sid:session_id -> string -> unit
val error : ?sid:session_id -> string -> unit

val build_started :
  ?sid:session_id ->
  packages:string list ->
  total_modules:int ->
  workers:int ->
  unit
(** Log build lifecycle events *)

val build_complete :
  ?sid:session_id -> duration_ms:int -> results:build_result list -> unit

val package_started : ?sid:session_id -> package:string -> unit
val package_complete : ?sid:session_id -> build_result -> unit
val compile_error : ?sid:session_id -> build_error -> unit

val cache_hit : ?sid:session_id -> package:string -> hash:string -> unit
(** Log cache events *)

val cache_miss : ?sid:session_id -> package:string -> hash:string -> unit

val cache_stored :
  ?sid:session_id ->
  package:string ->
  hash:string ->
  artifacts:string list ->
  unit

val hash_computed : ?sid:session_id -> package:string -> hash:string -> unit
(** Log hash computation events *)

val worker_pool_started : ?sid:session_id -> workers:int -> unit
(** Log worker events *)

val worker_started : ?sid:session_id -> worker_id:Worker_id.t -> unit
val worker_assigned : ?sid:session_id -> worker_id:Worker_id.t -> package:string -> unit
val worker_idle : ?sid:session_id -> worker_id:Worker_id.t -> unit

val server_started : ?sid:session_id -> pid:string -> unit
(** Log server events *)

val server_scanning : ?sid:session_id -> root:string -> unit
val server_ready : ?sid:session_id -> packages:int -> toolchain:string -> unit
val server_shutdown : ?sid:session_id -> unit -> unit

val queue_package :
  ?sid:session_id -> package:string -> queue_type:[ `Ready | `Waiting ] -> unit
(** Log queue events *)

val queue_stats :
  ?sid:session_id -> ready:int -> waiting:int -> busy:int -> unit

val dependency_missing :
  ?sid:session_id -> package:string -> missing:string list -> unit
(** Log dependency events *)

val dependency_satisfied : ?sid:session_id -> package:string -> unit

val compiling_interface :
  ?sid:session_id -> package:string -> file:string -> unit
(** Log compilation events *)

val compiling_implementation :
  ?sid:session_id -> package:string -> file:string -> unit

val linking_library : ?sid:session_id -> package:string -> output:string -> unit

val linking_executable :
  ?sid:session_id -> package:string -> output:string -> unit

val add_rpc_handler : sid:session_id -> client:Pid.t -> format:format -> unit
(** Handler management *)

val add_stdout_handler : format:format -> unit
val remove_handler : sid:session_id -> unit

val get_session_logs : sid:session_id -> format:format -> string
(** Retrieve logs for a session *)

(** Convert events to different formats *)

val event_to_string : log_event -> string
val format_event : format -> log_event -> string

(** Structured event processing functions *)
val get_timestamp_ms : unit -> int
val format_timestamp : int -> string
val event_name : log_event -> string  
val event_level : log_event -> level
val event_message : log_event -> string
