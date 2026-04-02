open Global

val main:
  name:string -> tests:Test_case.t list -> args:string list -> (unit, Actors.Process.exit_reason) result
