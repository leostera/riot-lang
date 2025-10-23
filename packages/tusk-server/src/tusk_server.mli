open Std
open Miniriot
open Tusk_model
module Protocol = Protocol
module Server_manager = Server_manager

type t
type build_request = Protocol.target

type build_event =
  | Started of { session_id : string; started_at : Time.Instant.t }
  | PackageStarted of { package : string }
  | PackageCompleted of {
      package : string;
      status : [ `built | `cached | `failed ];
      duration_ms : int;
    }
  | Completed of {
      session_id : string;
      completed_at : Time.Instant.t;
      total_duration_ms : int;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }
  | Failed of { session_id : string; error : string }
  | CycleDetected of { cycle : string list }

type server_config = {
  workspace : Workspace.t;
  toolchain : Tusk_toolchain.t;
  store : Tusk_store.Store.t;
  concurrency : int;
}

val start :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  concurrency:int ->
  t

val shutdown : t -> unit

val build :
  t -> build_request -> on_event:(build_event -> unit) -> (unit, string) result

val start_with_listener : unit -> (unit, exn) result
