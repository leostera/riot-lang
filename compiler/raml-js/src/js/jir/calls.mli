module Core = Raml_core.Core_ir

(** JS-owned direct-call lowering policy.

    Algorithm:
    - classify the direct callee through [Jir.Builtins]
    - lower recognized Riot builtins to JS-native syntax or shared primitive
      lowering
    - preserve ordinary direct calls by lowering the callee through
      [Jir.References] and emitting a regular call expression

    Effect:
    - keeps direct-call policy out of the main Core -> JIR traversal
    - centralizes the split between builtin calls, primitive calls, boolean
      short-circuit calls, and ordinary direct calls

    Rationale:
    This is the direct-call analogue of `Jir.Primitives` and `Jir.References`.
    Melange is the reference for having real backend subsystems; ReScript is the
    reference for preferring ordinary JS call shapes where semantics align. *)
val direct: Core.Entity_id.t -> Types.Expr.t list -> Types.Expr.t
