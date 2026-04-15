type t = Kernel.System.OS.t =
  | Unix
  | Win32
  | Cygwin
val current: t

(** Use `to_string value` for the stable legacy rendering used across Riot. *)
val to_string: t -> string

val is_unix: bool

val is_win32: bool

val is_cygwin: bool
