open Std

type package = string
type version_ranges = Version.t Ranges.t
type t

val package : t -> package
val ranges : t -> version_ranges
val is_positive : t -> bool
val positive : package -> version_ranges -> t
val negative : package -> version_ranges -> t
