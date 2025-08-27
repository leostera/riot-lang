(** Logging system for tusk - event distribution to handlers *)

open Miniriot

(** Message type for log events *)
type Message.t += Event of Event.t

val init : unit -> Pid.t
(** Initialize the logger process. Must be called once at server startup *)

val log : Event.t -> unit
(** Main logging function - sends events to logger process *)

val add_rpc_handler : session_id:Session_id.t -> client:Pid.t -> unit
(** Handler management *)

val add_stdout_handler : unit -> unit
val remove_handler : session_id:Session_id.t -> unit

val build_started :
  session_id:Session_id.t ->
  packages:string list ->
  total_modules:int ->
  workers:int ->
  unit
(** Build lifecycle logging *)

val build_complete :
  session_id:Session_id.t ->
  duration_ms:int ->
  results:Event.build_result list ->
  unit

val package_started : session_id:Session_id.t -> package:string -> unit
val package_complete : session_id:Session_id.t -> Event.build_result -> unit
val compile_error : session_id:Session_id.t -> Event.build_error -> unit

val cache_hit : session_id:Session_id.t -> package:string -> hash:string -> unit
(** Cache events *)

val cache_miss :
  session_id:Session_id.t -> package:string -> hash:string -> unit

val cache_stored :
  session_id:Session_id.t ->
  package:string ->
  hash:string ->
  artifacts:string list ->
  unit

val hash_computed :
  session_id:Session_id.t -> package:string -> hash:string -> unit
(** Hash computation *)

val worker_pool_started : session_id:Session_id.t -> workers:int -> unit
(** Worker pool events *)

val worker_started : session_id:Session_id.t -> worker_id:Worker_id.t -> unit

val worker_assigned :
  session_id:Session_id.t -> worker_id:Worker_id.t -> package:string -> unit

val worker_idle : session_id:Session_id.t -> worker_id:Worker_id.t -> unit

val server_started : session_id:Session_id.t -> pid:string -> unit
(** Server events *)

val server_scanning : session_id:Session_id.t -> root:string -> unit

val server_restarted :
  session_id:Session_id.t -> packages:int -> toolchain:string -> unit

val server_shutdown : session_id:Session_id.t -> unit -> unit

val workspace_empty : session_id:Session_id.t -> unit -> unit
(** Workspace events *)

val workspace_scanning : session_id:Session_id.t -> unit -> unit

val workspace_scanned :
  session_id:Session_id.t -> packages:int -> duration_ms:int -> unit

val build_graph_creating : session_id:Session_id.t -> unit -> unit
(** Build graph events *)

val build_graph_created :
  session_id:Session_id.t -> nodes:int -> duration_ms:int -> unit

val queue_package :
  session_id:Session_id.t ->
  package:string ->
  queue_type:[ `Ready | `Waiting ] ->
  unit
(** Queue events *)

val queue_stats :
  session_id:Session_id.t -> ready:int -> waiting:int -> busy:int -> unit

val dependency_missing :
  session_id:Session_id.t -> package:string -> missing:string list -> unit
(** Dependency events *)

val dependency_satisfied : session_id:Session_id.t -> package:string -> unit

val compiling_interface :
  session_id:Session_id.t -> package:string -> file:string -> unit
(** Compilation events *)

val compiling_implementation :
  session_id:Session_id.t -> package:string -> file:string -> unit

val linking_library :
  session_id:Session_id.t -> package:string -> output:string -> unit

val linking_executable :
  session_id:Session_id.t -> package:string -> output:string -> unit
