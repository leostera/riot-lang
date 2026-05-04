open Std

(** Detected platform *)
type t =
  | MacOS
  | Linux
  | Windows
  | Unknown of string

val to_string: t -> string

val from_string: string -> t

(** Detect the current platform *)
val detect: unit -> t

(** Get current platform as string *)
val current_string: unit -> string
