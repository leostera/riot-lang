open Std

(** CLI surface for [riot clean].

    Use this command to run workspace cache cleanup and build-root maintenance
    through [riot-store]'s cleanup policy.
*)
val command: Std.ArgParser.command

(** Run [riot clean] in a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
