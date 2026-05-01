open Global

type execution_mode =
  | Concurrent
  | Linear

val main:
  ?execution_mode:execution_mode ->
  name:string ->
  tests:Test_case.t list ->
  args:string list ->
  unit ->
  (unit, Runtime.Actor.exit_reason) result
