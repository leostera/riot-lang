open Std

type t = {
  workspace: Riot_model.Workspace.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

let make = fun
  ~workspace ?(parallelism = Thread.available_parallelism) ?(on_event = fun _ -> ()) () -> {
  workspace;
  parallelism = Int.max 1 parallelism;
  on_event;
}
