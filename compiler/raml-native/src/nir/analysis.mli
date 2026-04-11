module Core = Raml_core.Core_ir

val free_vars:
  name_of_entity:(Core.Entity_id.t -> string option) -> bound:string list -> Core.Expr.t -> string list

val captures_of_lambda:
  name_of_entity:(Core.Entity_id.t -> string option) ->
  bound_values:string list ->
  Core.Expr.lambda ->
  string list

val expr_uses_name_as_value:
  name_of_entity:(Core.Entity_id.t -> string option) ->
  shadowed:string list ->
  string ->
  Core.Expr.t ->
  bool
