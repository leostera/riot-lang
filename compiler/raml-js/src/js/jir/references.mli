module Core = Raml_core.Core_ir

(** Backend-owned lowering policy for entity and property references.

    Algorithm:
    - split resolved or unresolved entity paths into segments
    - classify leading module-like segments as namespace imports when they look
      like sibling compilation units
    - otherwise preserve local identifiers directly
    - lower remaining path segments as JS property/index access using the shared
      syntax policy

    Effect:
    - keeps namespace-import creation and dotted reference lowering out of the
      main Core -> JIR walk
    - centralizes the current heuristic module story in one subsystem
    - makes future module/object namespace work easier to evolve without
      rewriting the rest of lowering

    Rationale:
    Melange is the reference for treating module/path ownership as a real
    backend subsystem. ReScript is the reference for the emitted JS shape once
    those references are lowered. *)
val named_property_access: Types.Expr.t -> string -> Types.Expr.t

val entity: Core.Entity_id.t -> Types.Expr.t
