open Std

module Compiler_target = Raml_core.Target

type artifact =
  | Executable
  | Object
type error =
  | UnsupportedHostArchitecture of {
      host: Compiler_target.t;
      supported_hosts: Compiler_target.t list
    }
  | UnsupportedTargetArchitecture of {
      host: Compiler_target.t;
      supported_targets: Compiler_target.t list
    }
  | LinkFailed of { command: string; status: int; stderr: string }
  | SpawnFailed of { command: string; message: string }
type plan
val artifact_to_string: artifact -> string

val error_to_json: error -> Std.Data.Json.t

val plan:
  host:Compiler_target.t ->
  target:Compiler_target.t ->
  artifact:artifact ->
  input:Path.t ->
  output:Path.t ->
  (plan, error) result

val plan_to_string: plan -> string

val link:
  host:Compiler_target.t ->
  target:Compiler_target.t ->
  artifact:artifact ->
  input:Path.t ->
  output:Path.t ->
  (unit, error) result
