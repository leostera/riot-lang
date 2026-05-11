open Std

type mode =
  | ReadOnly
  | ReadWrite
  | Create

type synchronous =
  | Off
  | Normal
  | Full
  | Extra

type t = {
  path: Path.t;
  mode: mode;
  busy_timeout: Time.Duration.t option;
  cache_size: int option;
  synchronous: synchronous option;
}

let default = fun path ->
  {
    path;
    mode = Create;
    busy_timeout = Some (Time.Duration.from_secs 5);
    cache_size = None;
    synchronous = Some Normal;
  }

let in_memory = fun () ->
  {
    path = Path.v ":memory:";
    mode = Create;
    busy_timeout = None;
    cache_size = None;
    synchronous = Some Off;
  }
