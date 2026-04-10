exception Sys__Not_found

val sys__getenv : string -> string
val unix__putenv : string -> string -> unit
val unix__environment : unit -> string array
val unix__getcwd : unit -> string
val unix__chdir : string -> unit
