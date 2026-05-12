(** Apply cheap local rewrites to linear [LIR]. It removes no-op moves, folds
    constant conditional branches, drops branches that only jump to the next
    label, and turns [move; return] into direct returns. The pass exists to
    take easy linear wins before scheduling and emission. *)
val program: Types.Program.t -> Types.Program.t
