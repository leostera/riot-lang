open Std

(** Shared watch-mode helpers for long-running CLI commands. *)
type session

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

val start:
  command:string ->
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  mode:Ui.mode ->
  (session, exn) result

val changed_paths: session -> Fs.Event.t list -> Path.t list

val wait_change: session -> Path.t list

val drain_changed_paths: session -> Path.t list

val write_change: session -> Path.t list -> unit

val run:
  command:string ->
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  mode:Ui.mode ->
  run_once:(unit -> (unit, exn) result) ->
  unit ->
  (unit, exn) result
