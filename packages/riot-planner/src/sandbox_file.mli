open Std

type mode =
  | Reference
  | Link
  | Copy

type t = {
  source: Path.t;
  destination: Path.t;
  mode: mode;
}

val make: mode:mode -> source:Path.t -> destination:Path.t -> t

val reference: source:Path.t -> destination:Path.t -> t

val link: source:Path.t -> destination:Path.t -> t

val copy: source:Path.t -> destination:Path.t -> t
