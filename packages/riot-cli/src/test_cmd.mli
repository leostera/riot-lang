open Std

(** CLI surface for [riot test].

    This command delegates test execution into [riot-build] and focuses on
    translating parsed arguments into the user-facing test flow.
*)
val command: Std.ArgParser.command

(** Run [riot test] inside a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
