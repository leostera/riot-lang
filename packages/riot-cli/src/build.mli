open Std

(**
   CLI support for [riot build].

   Build owns argument parsing and build orchestration. User-facing rendering
   lives in [Ui].
*)
type build_scope = Riot_build.Request.scope =
  | Runtime
  | Dev
type dev_artifacts = Riot_build.Request.dev_artifacts = {
  tests: bool;
  examples: bool;
  benches: bool;
}
type request = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.request;
  scope: build_scope;
  dev_artifacts: dev_artifacts;
  profile: Riot_model.Profile.t;
  requested_parallelism: int option;
  mode: Ui.mode;
  show_finished_summary: bool;
}

(** Shared [riot build]-compatible argument surface. *)
val build_args: unit Std.ArgParser.arg list

(** Command definition for [riot build]. *)
val command: Std.ArgParser.command

(** Package-provided fix providers that belong to workspace members. *)
val workspace_fix_providers: Riot_model.Workspace.t -> Riot_model.Fix_provider.t list

(** Run [riot build] in a resolved workspace. *)
val run: workspace:Riot_model.Workspace.t -> Std.ArgParser.matches -> (unit, exn) result

(** Parse [riot build]-compatible matches into a build request. *)
val request_of_matches:
  workspace:Riot_model.Workspace.t ->
  Std.ArgParser.matches ->
  (request, exn) result

(** Execute a build command programmatically. *)
val build_command:
  workspace:Riot_model.Workspace.t ->
  ?scope:build_scope ->
  ?dev_artifacts:dev_artifacts ->
  ?profile:string ->
  ?mode:Ui.mode ->
  ?show_finished_summary:bool ->
  ?requested_parallelism:int option ->
  Riot_model.Package_name.t option ->
  string option ->
  (unit, exn) result
