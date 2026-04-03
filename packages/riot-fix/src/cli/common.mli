open Std

val current_dir: unit -> Path.t

val set_current_dir: Path.t -> unit

val with_cwd: ?cwd:Path.t -> (unit -> 'a) -> 'a

val resolve_target: ArgParser.matches -> Path.t

val relative_to_cwd: Path.t -> string

val diagnostic_count: Runner.file_result -> int

val clip_result_to_limit: int -> Runner.file_result -> Runner.file_result
