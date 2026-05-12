(** JS-owned classification of source-visible direct callees.

    This module centralizes the language-surface names that the JS backend
    treats specially, instead of scattering them across lowering and runtime
    helper definitions.

    Classification is entity-based, not raw-name-based. That lets the backend
    respect local shadowing while still recognizing unresolved/predef/persistent
    Riot surface names.

    The intended contract is:
    - Riot-owned surface names are the primary builtin vocabulary.
    - lowering should target native JS syntax/runtime ownership from those
      classifications, not re-match raw source names elsewhere. *)
module Core = Raml_core.Core_ir

type direct_callee =
  | Console_log
  | Console_error
  | Stdout_write
  | Stderr_write
  | String_constructor
  | Math_sqrt
  | Primitive of Core.Primitive.t
  | Unary_operator of Types.Operator.unary
  | Binary_operator of Types.Operator.binary
  | Boolean_and
  | Boolean_or
val classify_direct_callee: Core.Entity_id.t -> direct_callee option
