open Std

val command: ArgParser.command

val run_args: ?cwd:Path.t -> string list -> (unit, exn) result

val run_check_paths: ?cwd:Path.t -> Path.t list -> (unit, exn) result

val run: ArgParser.matches -> (unit, exn) result
