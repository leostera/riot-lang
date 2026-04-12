type t = exn = ..
type raw_backtrace
type raw_backtrace_entry = private int
val to_string: exn -> string

val get_raw_backtrace: unit -> raw_backtrace

val raw_backtrace_to_string: raw_backtrace -> string

val record_backtrace: bool -> unit

val backtrace_status: unit -> bool

val get_callstack: int -> raw_backtrace
