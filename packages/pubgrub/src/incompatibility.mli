open Std

type package = string
type version = Version.t

type cause =
  | Root
  | Dependency of package * version
  | Conflict of t * t
  | NoVersions

and t = { terms : Term.t list; cause : cause }

val create : Term.t list -> cause -> t
val not_root : package -> version -> t
val no_versions : package -> Version.t Ranges.t -> t
val from_dependency : package -> version -> package * Version.t Ranges.t -> t
