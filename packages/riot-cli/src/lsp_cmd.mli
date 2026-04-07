open Std

(** CLI surface for [riot lsp].

    This command stays intentionally thin and delegates the actual language
    server implementation to [riot-lsp].
*)
val command: ArgParser.command

(** Run [riot lsp] from already-parsed CLI matches. *)
val run: ArgParser.matches -> (unit, exn) result
