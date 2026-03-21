open Std

val command : ArgParser.command
val list_rules_output : format:Reporter.format -> string
val list_diagnostics_output : format:Reporter.format -> string
val run : ArgParser.matches -> (unit, exn) result
val main : args:string list -> (unit, exn) result
