type t = {
  packages: Package_target.t list;
  filter: string option;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
}
