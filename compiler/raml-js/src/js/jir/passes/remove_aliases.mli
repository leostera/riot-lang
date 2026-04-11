(** Removes trivial const aliases and rewrites references to their target.

    Algorithm:
    - collect assigned entities in the whole program
    - walk the program while maintaining a [Binding_id -> Entity_id] alias map
    - treat [const a = b] as an alias only when:
      - [a] and [b] are different entities
      - [a] is not exported
      - [b] is never assigned anywhere in the program
    - rewrite identifier references through the alias map
    - drop alias declarations and simplify any empty blocks/conditionals or
      pure expression leftovers exposed by that rewrite

    Effect:
    - late JIR references point more directly at their semantic target
    - redundant alias declarations disappear

    Rationale:
    - earlier lowering and import materialization produce harmless but noisy
      names that obscure real data flow
    - removing those aliases exposes more opportunities for DCE and import
      pruning without changing runtime behavior
*)
val program: context:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
