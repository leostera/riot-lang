type level = Trace | Debug | Info | Warn | Error

val set_level : level -> unit
val get_level : unit -> level

val trace : ('a, unit, string, unit) format4 -> 'a
val debug : ('a, unit, string, unit) format4 -> 'a
val info : ('a, unit, string, unit) format4 -> 'a
val warn : ('a, unit, string, unit) format4 -> 'a
val error : ('a, unit, string, unit) format4 -> 'a
