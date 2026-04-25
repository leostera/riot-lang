open Std

(**
   CLI surface for [riot install].

   This command resolves whether the target comes from the current workspace,
   an external source, or the registry, then delegates the real install flow
   into [riot-build].
*)
val command: Std.ArgParser.command

(**
   Run [riot install] with optional precomputed workspace information.

   Use this entry point when the caller may already know whether workspace
   discovery succeeded. That lets the CLI keep workspace-bound and detached
   install flows in one place.
*)
val run_with_workspace_info: workspace:Riot_model.Workspace.t option -> workspace_error:string option -> Std.ArgParser.matches -> (unit, exn) result

(** Run [riot install] in a known workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
