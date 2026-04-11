(** Trim obviously redundant structure from [NIR] before instruction
    selection. The pass folds literal-boolean conditionals, removes dead pure
    [let] bindings, collapses [let x = expr in x], and drops pure entry-side
    [Eval] items so later stages only carry meaningful work forward. *)
val program: Types.Program.t -> Types.Program.t
