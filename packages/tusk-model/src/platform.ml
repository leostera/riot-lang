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

(** Detect the current platform using Kernel's System.Host *)
let detect = fun () ->
  let host = Kernel.System.host_triplet in
  match host.os with
  | "darwin" -> MacOS
  | "linux" -> Linux
  | "windows" -> Windows
  | other -> Unknown other

(** Get current platform as string *)
let current_string = fun () -> detect () |> to_string
