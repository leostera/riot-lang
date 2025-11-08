open Global

val main :
  name:string ->
  tests:Test_case.t list ->
  args:string list ->
  (unit, Miniriot.Process.exit_reason) result
