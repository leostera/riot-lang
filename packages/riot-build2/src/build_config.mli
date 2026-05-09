open Std

type t = {
  workspace: Riot_model.Workspace.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

val make:
  workspace:Riot_model.Workspace.t ->
  ?parallelism:int ->
  ?on_event:(Event.t -> unit) ->
  unit ->
  t
