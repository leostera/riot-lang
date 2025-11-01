open Std

(** I/O loop for handling terminal input *)

type t = Pid.t
(** Handle to an IO loop process *)

type Message.t += 
  | Input of Event.t
  | IoStarted of Pid.t
  | Shutdown
(** Message types *)

val start : unit -> t
(** Start an IO loop process. Sends IoStarted message to parent. 
    Handles non-TTY environments gracefully. *)
