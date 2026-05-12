(** Remove dead local work from [MIR]. It runs a backwards liveness walk, keeps
    instructions whose results are still needed or whose effects matter, and
    drops pure moves or empty conditionals that no longer contribute anything.
    This shrinks the program before linearization. *)
val program: Types.Program.t -> Types.Program.t
