open Std

(** Initialize a new Riot workspace *)
val command: Std.ArgParser.command

(** ArgParser command definition for 'riot init' *)
val run: Std.ArgParser.matches -> (unit, exn) result

(** Execute the init command with parsed arguments *)
