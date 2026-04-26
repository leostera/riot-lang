open Std

type package = string
type version = Version.t
type decision_level = int
type assignment =
  | Decision of package * version * decision_level * int
  | Derivation of package * version Ranges.t * bool * Incompatibility.t * decision_level * int
type t
type same_decision_levels = {
  cause: Incompatibility.t;
  extra_term: Term.t option;
}
val empty: unit -> t

val add_decision: t -> package -> version -> t

val add_derivation: t -> package -> Incompatibility.t -> t

val get_decision: t -> package -> version option

val get_constraint:
  t ->
  package ->
  [`Decided of version | `Constrained of version Ranges.t | `Undecided]

val extract_solution: t -> (package * version) list

val pick_highest_priority_pkg:
  t ->
  (package -> version Ranges.t -> int) ->
  (package * version Ranges.t) option

val current_decision_level: t -> decision_level

val backtrack: t -> decision_level -> t

val relation:
  t ->
  Incompatibility.t ->
  [`Satisfied | `AlmostSatisfied of package | `Contradicted of package | `Unknown]

val satisfier_search:
  t ->
  Incompatibility.t ->
  package * [
    | `DifferentDecisionLevels of decision_level
    | `SameDecisionLevels of same_decision_levels
  ]
