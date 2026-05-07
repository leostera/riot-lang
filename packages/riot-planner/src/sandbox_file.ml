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

let make = fun ~mode ~source ~destination -> { source; destination; mode }

let reference = fun ~source ~destination -> make ~mode:Reference ~source ~destination

let link = fun ~source ~destination -> make ~mode:Link ~source ~destination

let copy = fun ~source ~destination -> make ~mode:Copy ~source ~destination
