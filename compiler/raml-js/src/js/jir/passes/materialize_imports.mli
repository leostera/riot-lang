(** Resolves import/runtime references into plain identifier references.

    Algorithm:
    - recursively walk the entire JIR body
    - replace [Imported requirement] with [Identifier requirement.local.entity]
    - replace [Runtime_helper helper] with [Identifier helper.local.entity]
    - leave [program.imports] unchanged

    Effect:
    - the body becomes a resolved JIR tree that refers only to semantic ids
    - import requirements remain attached to [program.imports] as module-level
      metadata instead of expression-level nodes

    Rationale:
    - [JST] should not understand unresolved import/runtime expression forms
    - this pass makes the [JIR -> JST] boundary explicit and enforceable
*)
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
