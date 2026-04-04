open Std

(** Command definition for [riot check]. *)
val command: ArgParser.command

(** Run [riot check] from already-parsed CLI matches. *)
val run: ArgParser.matches -> (unit, exn) result
