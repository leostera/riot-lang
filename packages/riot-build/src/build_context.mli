open Std

type error =
  | InvalidRequestedParallelism of int

type t = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: Resolved_build.scope;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

val make: ?on_event:(Event.t -> unit) -> Resolved_build.t -> (t, error) result
