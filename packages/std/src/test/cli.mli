val main :
  name:string ->
  tests:Test_case.t list ->
  ?args:string list ->
  unit ->
  (unit, exn) result
