open Std

val command : Std.ArgParser.command

val run : Std.ArgParser.matches -> (unit, exn) result
