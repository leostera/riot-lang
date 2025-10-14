open Std

type package = string
type version = Version.t

type assignment =
  | Decision of package * version
  | Derivation of package * version Ranges.t * Incompatibility.t

type t

val empty : unit -> t
val add_decision : t -> package -> version -> t
val add_derivation : t -> package -> version Ranges.t -> Incompatibility.t -> t
val get_decision : t -> package -> version option
val extract_solution : t -> (package * version) list
