open Std

type artifact =
  | Executable
  | Object
type error =
  | Unsupported_host of { host: Target.t }
  | Unsupported_target of { target: Target.t }
  | Link_failed of { command: string; status: int; stderr: string }
  | Spawn_failed of { command: string; message: string }
type plan
val artifact_to_string: artifact -> string

val error_to_json: error -> Std.Data.Json.t

val plan:
  host:Target.t ->
  target:Target.t ->
  artifact:artifact ->
  input:Path.t ->
  output:Path.t ->
  (plan, error) result

val plan_to_string: plan -> string

val link:
  host:Target.t ->
  target:Target.t ->
  artifact:artifact ->
  input:Path.t ->
  output:Path.t ->
  (unit, error) result
