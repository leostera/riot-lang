open Std

(** CLI surface for [riot new].

    This command scaffolds a new workspace or package by delegating into
    [riot-init].
*)
val command: Std.ArgParser.command

(** Run [riot new] from already-parsed CLI matches. *)
val run: Std.ArgParser.matches -> (unit, exn) result
