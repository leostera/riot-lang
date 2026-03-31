open Std

val command : Std.ArgParser.command

val run_install_args : string list -> (unit, exn) result

val run : Std.ArgParser.matches -> (unit, exn) result
