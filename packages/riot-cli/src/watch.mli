open Std

(** Shared watch-mode helpers for long-running CLI commands. *)

(**
   Workspace package roots watched for a command. Empty package filters watch all
   workspace package roots; non-empty filters expand through the selected
   packages' transitive workspace dependency cone.
*)
val watch_roots:
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  Path.t list

val should_ignore_path: workspace:Riot_model.Workspace.t -> Path.t -> bool

val run:
  command:string ->
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  mode:Ui.mode ->
  run_once:(unit -> (unit, exn) result) ->
  unit ->
  (unit, exn) result
