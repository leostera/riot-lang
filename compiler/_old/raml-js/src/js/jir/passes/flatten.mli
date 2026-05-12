(** Rewrites statement-shaped zero-argument IIFEs into direct statements.

    Algorithm:
    - scan late JIR for calls of the form [(function () { ... })()]
    - in effect position, inline bodies whose statements can be lowered without
      changing control-flow meaning
    - in declaration initializer position, split the IIFE into a temporary
      target, a block of statements, and a final binding assignment when the
      tail value can be assigned safely
    - keep the original form when the body shape is not supported

    Effect:
    - fewer synthetic IIFEs survive into later JIR
    - more computation appears as ordinary statements, declarations, and
      assignments

    Rationale:
    - early lowering uses IIFEs as the simplest expression-oriented encoding for
      [let], [sequence], and tail control flow
    - later passes reason better about statement form than hidden work inside
      immediately-invoked functions
*)
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
