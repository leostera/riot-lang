type t = {
  package: Riot_model.Package_name.t option;
  binary: string option;
  args: string list;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}
