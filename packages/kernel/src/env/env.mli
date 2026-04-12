type error =
  | InvalidVarName of { name: string }
  | System of System_error.t
val error_to_string: error -> string

val args: string array

(** Use `executable_name` to inspect the zeroth process argument when one exists. *)
val executable_name: string option

(** Use `get name` to read the current process environment immediately. *)
val get: string -> string option

(** Use `set_var ~name ~value` to update the current process environment immediately. *)
val set_var: name:string -> value:string -> (unit, error) Result.t

(** Use `remove_var ~name` to update the current process environment immediately. *)
val remove_var: name:string -> (unit, error) Result.t

(** Use `vars ()` to snapshot the current process environment immediately. *)
val vars: unit -> (string * string) array

(** Use `current_dir ()` to read the process working directory immediately. *)
val current_dir: unit -> (Path.t, error) Result.t

(** Use `set_current_dir path` to update the process working directory immediately. *)
val set_current_dir: Path.t -> (unit, error) Result.t

val home_dir: unit -> Path.t option
