type raw_backtrace = Kernel.Exception.raw_backtrace
val to_string: exn -> string

val get_raw_backtrace: unit -> raw_backtrace

val raw_backtrace_to_string: raw_backtrace -> string

val record_backtrace: bool -> unit
