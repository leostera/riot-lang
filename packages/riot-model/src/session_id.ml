open Std

(** Session ID module - provides opaque session identifiers *)
type t = string

(* fixme: use an atomic int and increment it on every new id, so we can keep the id an int until we need to turn it into a string *)

let make = fun () ->
  "build-"
  ^ Int32.to_string (Process.id ())
  ^ "-"
  ^ Int.to_string
    (
      Random.int 1_000_000
      |> Result.expect ~msg:"failed to sample session id suffix"
    )

let to_string = fun t -> t

let from_string = fun s -> s
