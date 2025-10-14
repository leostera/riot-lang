open Std

type package = string
type version = Version.t
type decision_level = int

type assignment =
  | Decision of package * version * decision_level
  | Derivation of
      package * version Ranges.t * Incompatibility.t * decision_level

type t

val empty : unit -> t
val add_decision : t -> package -> version -> t
val add_derivation : t -> package -> version Ranges.t -> Incompatibility.t -> t
val get_decision : t -> package -> version option

val get_constraint :
  t ->
  package ->
  [ `Decided of version | `Constrained of version Ranges.t | `Undecided ]

val extract_solution : t -> (package * version) list
val current_decision_level : t -> decision_level
val backtrack : t -> decision_level -> t

val relation :
  t ->
  Incompatibility.t ->
  [ `Satisfied
  | `AlmostSatisfied of package
  | `Contradicted of package
  | `Unknown ]

val satisfier_search :
  t ->
  Incompatibility.t ->
  package
  * [ `DifferentDecisionLevels of decision_level
    | `SameDecisionLevels of Incompatibility.t ]
