open Std

val expand: Package_catalog.t -> Goal.t -> (Package_work.t list, Error.t) result
