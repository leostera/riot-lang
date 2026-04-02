open Std

type package = string
type version = Version.t
type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t
val solve: ?trace_ctx:Trace.t -> string Provider.t -> package -> version -> (solve_result, string) result
