(** Alpha-renames printable binder names without changing semantic identity.

    Algorithm:
    - walk imports, declarations, and function parameters in source order
    - keep a [Binding_id -> emitted name] map plus a visible-name set
    - sanitize binder names to valid JS binding identifiers before freshness
      checks
    - generate [$]-suffixed fresh names when a printable name would shadow an
      existing visible name
    - rewrite later import references to the renamed local binder

    Effect:
    - binder names in the late JIR are valid and unique within the emitted JS
      surface
    - entity and binding ids stay unchanged, so later passes still reason over
      stable semantic identity

    Rationale:
    - the emitter prints names, not ids
    - later lowering steps still materialize imports and remove aliases using
      ids, but they need a collision-free printable name surface first
*)
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
