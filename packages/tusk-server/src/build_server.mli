(** Build server - Handles build execution in a spawned process *)

open Tusk_model

val start :
  workspace:Workspace.t ->
  toolchain:Tusk_toolchain.t ->
  store:Tusk_store.Store.t ->
  concurrency:int ->
  session_id:Session_id.t ->
  client_pid:Miniriot.Pid.t ->
  target:Protocol.target ->
  unit
(** Start a build in a spawned worker process. This function returns immediately
    after spawning the worker. The worker will send results directly to the
    client_pid. *)
