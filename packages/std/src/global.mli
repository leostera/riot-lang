(** Global functions *)

val available_parallelism : unit -> int
(** Get the number of available CPU cores for parallelism *)

val cpu_count : unit -> int
(** Get the number of CPU cores (alias for available_parallelism) *)

val os_type : unit -> string
(** Get the OS type *)

val time : unit -> float
(** Get current time as float *)

val gettimeofday : unit -> float
(** Get current time with microsecond precision *)

val time_ms : unit -> int
(** Get current time in milliseconds *)

val panic : string -> 'a
(** Raise a panic exception with the given message *)

(** Date and time utilities *)
module Datetime : sig
  val now : unit -> float
  val localtime : float -> Unix.tm
  val gmtime : float -> Unix.tm
end

(** Process status types *)
module Process : sig
  type status = Exited of int | Signaled of int | Stopped of int

  val of_unix_status : Unix.process_status -> status
end

(** File types *)
module File : sig
  type kind = Regular | Directory | Character | Block | Link | Fifo | Socket

  val kind_of_unix : Unix.file_kind -> kind
end
