open Std

type package = string
type version = Version.t

type cause =
  | Root
  | Dependency of package * version
  | Conflict of t * t
  | NoVersions

and t = { terms : Term.t list; cause : cause }

let create terms cause = { terms; cause }

let not_root pkg ver =
  let term = Term.negative pkg (Ranges.singleton ver) in
  create [ term ] Root

let no_versions pkg ranges =
  let term = Term.positive pkg ranges in
  create [ term ] NoVersions

let from_dependency pkg ver (dep_pkg, dep_ranges) =
  let parent_term = Term.negative pkg (Ranges.singleton ver) in
  let dep_term = Term.positive dep_pkg dep_ranges in
  create [ parent_term; dep_term ] (Dependency (pkg, ver))
