open Std

val command : ArgParser.command
val run : ArgParser.matches -> (unit, exn) result
val main : args:string list -> (unit, exn) result
