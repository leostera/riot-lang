open Std
open Tusk_model

type t

val start :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  concurrency:int ->
  t

val shutdown : t -> unit

type build_request = BuildAll | BuildPackage of string

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

val build :
  t -> build_request -> on_event:(build_event -> unit) -> (unit, string) result
