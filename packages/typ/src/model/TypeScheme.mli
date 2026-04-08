open Std

(** Quantified type schemes exported from the prototype inferencer. *)
type t = TypeRepr.t
val of_type: TypeRepr.t -> t

val of_explicit: quantified:int list -> TypeRepr.t -> t

val body: t -> TypeRepr.t

val to_explicit: t -> int list * TypeRepr.t

val instantiate:
  fresh_var:(unit -> TypeRepr.t) ->
  make:(TypeRepr.desc -> TypeRepr.t) ->
  next_mark:(unit -> int) ->
  t ->
  TypeRepr.t

val copy: t -> t

val free_vars: t -> int list
