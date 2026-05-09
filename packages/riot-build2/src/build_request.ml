open Std

type t = {
  workspace: Riot_model.Workspace.t;
  packages: Riot_model.Package_name.t list;
  targets: Riot_model.Target.t list;
  profile: Riot_model.Profile.t;
  parallelism: int option;
}

let make = fun
  ?(packages = [])
  ?(targets = [])
  ?(profile = Riot_model.Profile.debug)
  ?parallelism
  ~workspace
  () ->
  {
    workspace;
    packages;
    targets =
      if List.is_empty targets then
        [ Riot_model.Target.current ]
      else
        targets;
    profile;
    parallelism;
  }
