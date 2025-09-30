(* Library interface module utilities *)

val template :
  parent:Module_name.t ->
  modules:Module_name.t list ->
  stdlib_modules:Module_name.t list ->
  string list
