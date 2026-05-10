open Std

type error =
  | InvalidRequestedParallelism of int
type t = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

val make: ?on_event:(Event.t -> unit) -> Request.t -> (t, error) result

val emit_phase: t -> Event.runtime_phase -> unit

val emit_building_target: t -> target:Riot_model.Target.t -> host:bool -> unit

val emit_cache_gc: t -> Riot_store.Cache_gc.event -> unit

val flush_events: t -> unit
