(** Perform a conservative form of common-subexpression elimination for [MIR].
    Today it only reuses repeated literal and symbol-address materializations,
    rewriting later copies to point at the first destination register. That
    keeps the optimization honest on the current IR while still reducing
    repeated pure setup work. *)
val program: Types.Program.t -> Types.Program.t
