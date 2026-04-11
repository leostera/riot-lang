(** JS-owned classification of source-visible direct callees.

    This module centralizes the language-surface names that the JS backend
    treats specially, instead of scattering them across lowering and runtime
    helper definitions. *)

type direct_callee =
  | Runtime_helper of Types.Runtime.helper
  | Primitive of string
  | Boolean_not
  | Boolean_and
  | Boolean_or

val classify_direct_callee: string -> direct_callee option
