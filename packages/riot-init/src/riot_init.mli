open Std

(** ArgParser command definition for `riot init`. *)
val command: Std.ArgParser.command

(** Execute `riot init` with parsed arguments. *)
val run: Std.ArgParser.matches -> (unit, exn) result
