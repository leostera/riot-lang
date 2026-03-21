open Std

type build_scope = Runtime | Dev

val command : Std.ArgParser.command
val run : Std.ArgParser.matches -> (unit, exn) result
val build_command :
  ?scope:build_scope ->
  string option ->
  string option ->
  (unit, exn) result
