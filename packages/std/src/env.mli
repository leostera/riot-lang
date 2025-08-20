(** Environment utilities *)

val args : string list
(** The command line arguments passed to this program *)

val current_dir : unit -> (Path.t, Path.error) Result.t
val set_current_dir : Path.t -> (unit, Path.error) Result.t

val home_dir : unit -> (Path.t, Path.error) Result.t
(** Get the user's home directory *)

type 't var_type =
  | String : string var_type
  | Int : int var_type
  | Float : float var_type
  | Bool : bool var_type
  | Char : char var_type

val var : 't var_type -> name:string -> 't option
val set_var : name:string -> value:string -> 't option
val vars : unit -> (string * string) list
