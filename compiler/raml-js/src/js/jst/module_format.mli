(** JS module-surface policy derived from the shared compilation context.

    Today the JS backend only emits ESM, but this module is the explicit place
    where target-sensitive output format decisions should live once JS targets
    diverge further, for example `js-unknown-ecma` vs `js-unknown-commonjs`. *)
type t =
  | Esm
val from_context: Raml_core.Compilation_context.t -> t
