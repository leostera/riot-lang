(** Process effects for cooperative scheduling *)

val yield : unit -> unit
(** Yield control to the scheduler, allowing other processes to run *)

val receive_any : unit -> Message.t
(** Receive any message from the process mailbox *)

type 'msg selector = Message.t -> [ `select of 'msg | `skip ]

val receive : selector:'a selector -> unit -> 'a
(** Receive a message using a selector function *)

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
