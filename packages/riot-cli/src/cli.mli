open Std

val initialize_runtime: unit -> unit

val run: args:string list -> (unit, exn) result

val main: args:string list -> (unit, exn) result
