open Std
open Riot_model
open Model

type run_context

val create_run_context:
  workspace_root:Path.t ->
  ?target:Target.t ->
  profile:string ->
  filter:string option ->
  partial:bool ->
  argv:string list ->
  unit ->
  run_context

val run_id: run_context -> string

val suite_run_path: run_context -> package_name:Package_name.t -> suite_name:string -> Path.t

val load_recent_suite_runs:
  run_context ->
  package_name:Package_name.t ->
  suite_name:string ->
  limit:int ->
  (stored_suite_run list, string) result

val save_suite_run:
  run_context ->
  package_name:Package_name.t ->
  suite_name:string ->
  suite_run:suite_run ->
  (Path.t option, string) result
