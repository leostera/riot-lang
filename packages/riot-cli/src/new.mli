open Std

(** CLI surface for [riot new].

    This command scaffolds a new workspace or package by delegating into
    [riot-init].
*)
val command: Std.ArgParser.command

(** Run [riot new] from already-parsed CLI matches. *)
val run: Std.ArgParser.matches -> (unit, exn) result

(** User-facing guidance when [riot new] runs outside a workspace. *)
val no_workspace_message: string

(** Report that [riot new] requires an initialized workspace. *)
val run_without_workspace: Std.ArgParser.matches -> (unit, exn) result
