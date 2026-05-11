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

val default: Path.t -> t

val in_memory: unit -> t
