open Std

val expand: Package_catalog.t -> User_intent.t -> (Goal.t list, Error.t) result
