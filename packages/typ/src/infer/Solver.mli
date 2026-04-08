open Std
open Diagnostics
open Model

type t
type generalization_group
val create: unit -> t

val make_type: t -> TypeRepr.desc -> TypeRepr.t

val fresh_var: t -> TypeRepr.t

val next_mark: t -> int

val group: ?expansive_roots:TypeRepr.t list -> TypeRepr.t list -> generalization_group

val with_local_level_gen:
  t ->
  variance_of_named:(TypeRepr.named_type_head -> TypeRepr.t list -> TypeDecl.variance list) ->
  (unit -> 'a * generalization_group list) ->
  'a * TypeScheme.t list list

val instantiate: t -> TypeScheme.t -> TypeRepr.t

val unify: t -> left:TypeRepr.t -> right:TypeRepr.t -> (unit, Diagnostic.mismatch) result
