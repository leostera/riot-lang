(** Build server - Process that orchestrates builds *)

open Miniriot
open Core
open Model

(* FIXME: inside tusk_server.ml this is just an alias to Pid.t *)
type t

val start : unit -> t
(** Start the build server and return its PID *)

val start_with_listener : unit -> (unit, Process.exit_reason) result
(** Start the server with TCP listener for RPC. This function makes the current
    process _become_ the Tusk server *)

(* FIXME: the following functions are equivalent to

  send server_pid msg;
  let recv_loop () =
    let selector = (* selector for `msg` *) in
    match receive ~selector () with
    | ...
  in
  recv_loop ()

  implement them and use them, so all the logic for sending/receiving messages to the tusk_server lives within the tusk_server.ml file

  also remove this fixme comment once done implementing them
*)

(* scans the workspace *)
val scan_workspace : t -> (Workspace.t, Error.t) result

(* shutsdown the server *)
val shutdown : t -> (unit, Error.t) result

(* builds the entire workspace

   NOTE for Claude: this function will block the client claler until the build
   is completed, so internally it will send the BuildAll message, and wait for
   all the necessary messages in a wait loop or whatever, but only really
   return when it receives a BuildCompleted or BuildFailed message 

   *)
val build_all : t -> (Build_results.t, Error.t) result

(* builds a specific package 


    NOTE for Claude: same as `build_all` but for a specific package

   *)
val build_package : name:string -> (Build_results.t, Error.t) result
