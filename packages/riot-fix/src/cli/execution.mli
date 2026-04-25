open Std

val run_result: mode:Runner.mode -> scope:Fix_config.scope option -> limit:int option -> files:Path.t list -> Types.run_outcome

val run_with_coordinator: ?on_event:(Types.event -> unit) -> output_mode:Types.output_mode -> mode:Runner.mode -> scope:Fix_config.scope option -> limit:int option -> roots:Path.t list -> unit -> (unit, exn) result

val run_generated_runner: cwd:Path.t -> build_package:Types.build_package -> report_output:bool -> mode:Runner.mode -> limit:int option -> target:Path.t -> output_mode:Types.output_mode -> Fix_config.scope -> (unit, exn) result
