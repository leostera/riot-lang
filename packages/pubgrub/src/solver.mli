open Std

type package = string
type version = Version.t
type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t
type options = {
  max_iterations: int;
}
type stats = {
  iterations: int;
  decisions: int;
  derivations: int;
  conflicts: int;
  learned_incompatibilities: int;
  backtracks: int;
  provider_choose_version_calls: int;
  provider_count_versions_calls: int;
  provider_get_dependencies_calls: int;
  provider_calls: int;
  max_decision_depth: int;
}
type outcome = {
  result: (solve_result, string) result;
  stats: stats;
}
val default_options: options

val solve_with_stats:
  ?trace_ctx:Trace.t -> ?options:options -> string Provider.t -> package -> version -> outcome

val solve:
  ?trace_ctx:Trace.t ->
  ?options:options ->
  string Provider.t ->
  package ->
  version ->
  (solve_result, string) result
