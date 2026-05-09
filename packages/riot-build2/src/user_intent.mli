open Std

type build = {
  packages: Riot_model.Package_name.t list;
  all_packages: bool;
  profile: Riot_model.Profile.t;
  targets: Riot_model.Target.t list;
}

type test = {
  packages: Riot_model.Package_name.t list;
  filter: string option;
  profile: Riot_model.Profile.t;
  targets: Riot_model.Target.t list;
}

type run = {
  package: Riot_model.Package_name.t option;
  binary: string option;
  args: string list;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}

type t =
  | Build of build
  | Test of test
  | Run of run

val build:
  ?packages:Riot_model.Package_name.t list ->
  ?all_packages:bool ->
  ?profile:Riot_model.Profile.t ->
  ?targets:Riot_model.Target.t list ->
  unit ->
  t

val test:
  ?packages:Riot_model.Package_name.t list ->
  ?filter:string ->
  ?profile:Riot_model.Profile.t ->
  ?targets:Riot_model.Target.t list ->
  unit ->
  t

val run:
  ?package:Riot_model.Package_name.t ->
  ?binary:string ->
  ?args:string list ->
  ?profile:Riot_model.Profile.t ->
  ?target:Riot_model.Target.t ->
  unit ->
  t
