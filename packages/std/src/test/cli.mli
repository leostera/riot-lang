open Global

val main:
  name:string ->
  tests:Test_case.t list ->
  args:string list ->
  (unit, Runtime.Actor.exit_reason) result
