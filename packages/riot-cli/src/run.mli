open Std

(** CLI surface for [riot run].

    This command resolves a runnable target, builds it if necessary, and then
    delegates execution to the compiled binary.
*)
val command: Std.ArgParser.command

(** Decide which build scope should be used for a workspace binary. *)
val build_scope_for_binary:
  Riot_model.Workspace.t -> package_name:string -> binary_name:string -> Riot_build.Request.scope

(** Derive the default binary name for a remote source target.

    Use this when the caller omits the binary name and the remote source only
    gives the package identity.
*)
val default_remote_binary_name: string -> string

type implicit_local_target = {
  (** Package chosen for the implicit run target. *)
  package_name: string;
  (** Binary chosen for the implicit run target. *)
  binary_name: string;
}

(** Resolve the single runnable local binary when the user omitted the target.

    This succeeds only when the current workspace context leaves one unambiguous
    runnable binary after applying the optional package filter.
*)
val resolve_implicit_local_target:
  ?package_filter:string -> Riot_model.Workspace.t -> (implicit_local_target, string) result

(** Run [riot run] with optional precomputed workspace information. *)
val run_with_workspace_info:
  workspace:Riot_model.Workspace.t option ->
  workspace_error:string option ->
  Std.ArgParser.matches ->
  (unit, exn) result

(** Run [riot run] in a known workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result
