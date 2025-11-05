open Std

(** I/O loop for handling terminal input *)

type t = Pid.t
(** Handle to an IO loop process *)

type Message.t += 
  | Input of Event.t
  | IoStarted of Pid.t
  | Shutdown
(** Message types *)

val start : tty:Tty.t -> unit -> t
(** Start an IO loop process with a TTY handle. Sends IoStarted message to parent.
    
    @param tty The TTY handle to use for reading input *)
