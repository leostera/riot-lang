open Std

type t = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.t list;
  profile: Riot_model.Profile.t;
  parallelism: int option;
}

val make:
  ?packages:Riot_model.Package_name.t list ->
  ?targets:Riot_model.Target.t list ->
  ?profile:Riot_model.Profile.t ->
  ?parallelism:int ->
  workspace:Riot_model.Workspace.t ->
  unit ->
  t
