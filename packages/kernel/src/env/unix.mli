type error =
  | InvalidVarName of { name: string }
  | System of System_error.t

val error_to_string: error -> string

val args: string array

val executable_name: string option

val get: var:string -> string option

val set: var:string -> value:string -> (unit, error) Result.t

val remove: var:string -> (unit, error) Result.t

val vars: unit -> (string * string) array

val current_dir: unit -> (Path.t, error) Result.t

val set_current_dir: Path.t -> (unit, error) Result.t

val home_dir: unit -> Path.t option
