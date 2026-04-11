(** Normalizes raw JIR structure and recollects required imports.

    Algorithm:
    - recursively normalize expressions and statements
    - normalize nested function bodies and block contents
    - collapse [Block []] and empty conditionals through [Simplify]
    - scan the normalized body for imported/runtime references
    - deduplicate imports while preserving first-seen encounter order

    Effect:
    - later passes see a smaller, more regular tree
    - [program.imports] becomes the canonical list of import requirements for
      the current body

    Rationale:
    - lowering intentionally creates direct but noisy JIR
    - the backend wants a cheap canonicalization pass before more targeted
      transformations like flattening, alias removal, and DCE
*)
val program: Types.Program.t -> Types.Program.t
