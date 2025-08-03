(** Miniriot - Minimal single-core actor runtime *)

module Pid : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end

module Message : sig
  type t = ..
end

module Process : sig
  type exit_reason = Normal | Exception of exn
end

(** Example message type *)
type Message.t += Exit

(** Run the main function as the initial process.
    Can only be called once per process - subsequent calls will raise Failure. *)
val run : main:(unit -> Process.exit_reason) -> int

(** Spawn a new process *)
val spawn : (unit -> Process.exit_reason) -> Pid.t

(** Get the current process PID *)
val self : unit -> Pid.t

(** Send a message to a process *)
val send : Pid.t -> Message.t -> unit

(** Yield control to the scheduler *)
val yield : unit -> unit

(** Receive any message *)
val receive : unit -> Message.t

(** Selective receive with a selector function *)
val selective_receive : (Message.t -> [`select of 'msg | `skip]) -> 'msg

(** Exit normally *)
val exit : unit -> Process.exit_reason

(** Sleep (currently just yields) *)
val sleep : float -> unit

(** Enable debug tracing *)
val enable_trace : unit -> unit

(** Disable debug tracing *)
val disable_trace : unit -> unit