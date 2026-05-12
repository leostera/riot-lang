(** JS-owned object literal and property access helpers.

    Algorithm:
    - construct plain JS object literals from named fields
    - lower named property access using the shared JS syntax policy:
      dot-property when valid, bracket access otherwise

    Effect:
    - keeps JS object construction and property access in one subsystem
    - lets records and future namespace/module-object lowering share the same
      backend policy

    Rationale:
    ReScript is the reference for preferring natural JS object syntax. Since
    `raml-js` explicitly means plain JavaScript objects when it says `Object`,
    that ownership should live in one backend module rather than being split
    between lowering and reference code. *)
val field: string -> Types.Expr.t -> Types.Expr.object_field

val literal: Types.Expr.object_field list -> Types.Expr.t

val named_access: Types.Expr.t -> string -> Types.Expr.t
