(** Filesystem utilities *)

type error = SystemError of string

val create_dir : Path.t -> (unit, error) Result.t
(** Create a directory if it doesn't exist *)
