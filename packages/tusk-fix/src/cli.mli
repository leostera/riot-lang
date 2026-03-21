open Std

type run_outcome = {
  result : Runner.run_result;
  limit_reached : bool;
}

val command : ArgParser.command
val list_rules_output : format:Reporter.format -> string
val list_diagnostics_output : format:Reporter.format -> string
val run_result :
  mode:Runner.mode ->
  scope:Fix_config.scope option ->
  limit:int option ->
  files:Path.t list ->
  run_outcome
val run : ArgParser.matches -> (unit, exn) result
val main : args:string list -> (unit, exn) result
