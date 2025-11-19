open Std

type unification_error =
  | TypeMismatch of Types.type_expr * Types.type_expr
  | OccursCheck of Types.type_expr * Types.type_expr
  | ArityMismatch of { expected : int; got : int; path : ModulePath.t }

val unify :
  ctx:Types.context ->
  Types.type_expr ->
  Types.type_expr ->
  (Types.context, unification_error) Result.t

val instance :
  ctx:Types.context -> Types.type_expr -> Types.type_expr * Types.context

val generalize : level:int -> Types.type_expr -> unit
val error_to_string : unification_error -> string
