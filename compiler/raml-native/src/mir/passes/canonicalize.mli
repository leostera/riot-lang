(** Canonicalize the structured [MIR] tree before the heavier MIR passes run.
    It recursively rewrites conditionals, drops no-op moves, folds constant or
    empty branches, and leaves the tree in a smaller, more regular shape. This
    pays down structural noise early so later analyses spend their effort on
    real work instead of administrative artifacts. *)
val program: Types.Program.t -> Types.Program.t
