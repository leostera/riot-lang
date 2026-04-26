open Std

(** Detected platform *)
type t =
  | MacOS
  | Linux
  | Windows
  | Unknown of string

let to_string = function
  | MacOS -> "macos"
  | Linux -> "linux"
  | Windows -> "windows"
  | Unknown s -> s

let of_string = function
  | "macos" -> MacOS
  | "linux" -> Linux
  | "windows" -> Windows
  | s -> Unknown s

(** Detect the current platform using Std.System.TargetTriple *)
let detect = fun () ->
  let host = System.host_triple in
  match host.os with
  | "darwin" -> MacOS
  | "linux" -> Linux
  | "windows" -> Windows
  | other -> Unknown other

(** Get current platform as string *)
let current_string = fun () ->
  detect ()
  |> to_string
