(** Rewrite virtual register references in [LIR] to explicit homes. The pass
    uses the frame metadata computed earlier to replace register operands and
    destinations with concrete homes, so the emitter no longer has to guess
    where values live. This is the seam between late IR cleanup and
    target-specific emission. *)
val program: Types.Program.t -> Types.Program.t
