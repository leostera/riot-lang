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
