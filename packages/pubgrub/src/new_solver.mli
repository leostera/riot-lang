open Std

type package = string
type version = Version.t
type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t
val solve: string Provider.t -> package -> version -> (solve_result, string) result
