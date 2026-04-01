open Std
(** Initialize a new Tusk workspace *)
val command: Std.ArgParser.command
(** ArgParser command definition for 'tusk init' *)
val run: Std.ArgParser.matches -> (unit, exn) result
(** Execute the init command with parsed arguments *)
