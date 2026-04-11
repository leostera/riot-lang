open Std

type artifact =
  | Executable
  | Object
type error =
  | UnsupportedHost of { host: Target.t }
  | UnsupportedTarget of { target: Target.t }
  | LinkFailed of { command: string; status: int; stderr: string }
  | SpawnFailed of { command: string; message: string }
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
