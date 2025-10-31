(** Process effects for cooperative scheduling *)

module Exception : sig
exception Receive_timeout
(** Raised when a receive operation times out *)

exception Syscall_timeout
(** Raised when a syscall operation times out *)
end

val yield : unit -> unit
(** Yield control to the scheduler, allowing other processes to run *)

val receive_any : ?timeout:float -> unit -> Message.t
(** Receive any message from the process mailbox. Optionally timeout after the
    specified duration in seconds. Raises [Receive_timeout] on timeout. *)

type 'msg selector = Message.t -> [ `select of 'msg | `skip ]

val receive : selector:'a selector -> ?timeout:float -> unit -> 'a
(** Receive a message using a selector function. Optionally timeout after the
    specified duration in seconds. Raises [Receive_timeout] on timeout. *)

val exit : unit -> (unit, Process.exit_reason) result
(** Exit the current process normally *)

val sleep : int -> unit
(** Sleep for the specified duration (currently just yields) *)

val syscall :
  ?timeout:float ->
  name:string ->
  interest:Kernel.Async.Interest.t ->
  source:Kernel.Async.Source.t ->
  (unit -> 'a) ->
  'a
(** Perform a system call with I/O polling support *)
