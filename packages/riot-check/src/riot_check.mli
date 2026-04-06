open Std

(** Command definition for [riot check]. *)
val command: ArgParser.command

open Riot_model

(** Run [riot check] from already-parsed CLI matches.

    When [workspace] is provided, omitted targets are interpreted relative to that
    workspace context (package-aware ignore configuration, package roots, and
    optional [-p]/[--package] package narrowing). When [workspace] is omitted
    and no explicit targets are provided, [riot check] falls back to checking
    from the current directory.

    Optional [stdout] and [stderr] emitters allow tests and embedded callers to
    capture structured and human output without going through process-global
    stdio.
*)
val run:
  ?workspace:Workspace.t ->
  ?stdout:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ArgParser.matches ->
  (unit, exn) result

(** Best-effort local type-cache warmup for a workspace.

    This computes canonical [Typ.ModuleTypings] for the requested workspace
    packages and persists them under the workspace-local type cache rooted in
    the workspace target directory. When [package_names] is empty, all workspace
    member packages are warmed. Failures are intentionally swallowed so callers
    can keep this as a best-effort cache refresh instead of a fatal command
    step. *)
val populate_workspace_typings: workspace:Workspace.t -> package_names:string list -> unit -> unit
