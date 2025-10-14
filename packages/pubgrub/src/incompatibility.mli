open Std

type package = string
type version = Version.t

type external_cause =
  | NotRoot of package * version
  | NoVersions of package * version Ranges.t
  | FromDependency of package * version * package * version Ranges.t
  | Custom of package * version Ranges.t * string

type t =
  | External of { terms : Term.t list; cause : external_cause }
  | Derived of {
      terms : Term.t list;
      cause1 : t;
      cause2 : t;
      shared_id : int option;
    }

val create_external : Term.t list -> external_cause -> t
val create_derived : Term.t list -> t -> t -> int option -> t
val not_root : package -> version -> t
val no_versions : package -> version Ranges.t -> t
val from_dependency : package -> version -> package * version Ranges.t -> t
val terms : t -> Term.t list
val get_term : t -> package -> Term.t option
val is_terminal : t -> package -> version -> bool
val merge_dependents : t -> t -> t option
val prior_cause : t -> t -> package -> t
val as_dependency : t -> (package * package) option
