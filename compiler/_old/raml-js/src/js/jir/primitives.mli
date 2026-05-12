module Core = Raml_core.Core_ir

(** JS-owned lowering policy for shared Core IR primitives.

    Algorithm:
    - pattern-match on the typed shared [Core.Primitive.t]
    - lower semantically aligned primitives directly to native JIR forms
      such as arrays, indexing, operators, globals, and method calls
    - fall back to the small runtime helper surface only for primitives whose
      current semantics still require validation or helper-owned behavior

    Effect:
    - keeps primitive lowering out of the main Core -> JIR lowering walk
    - makes the JS-native vs runtime-backed split explicit and testable
    - reduces the amount of helper-heavy JS we emit for ordinary arithmetic,
      tuples, comparisons, tracing, and string conversion

    Rationale:
    ReScript is the better reference for emitted JS shape here: the backend
    should prefer ordinary JS syntax when semantics align. Melange is still the
    reference for owning this as a backend subsystem instead of leaving it as
    scattered ad hoc matches in the lowering pass. *)
val lower: Core.Primitive.t -> Types.Expr.t list -> Types.Expr.t
