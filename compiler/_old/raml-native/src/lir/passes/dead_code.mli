(** This pass uses [LIR] liveness to drop local writes whose values are never
    observed.

    It walks the linear instruction stream with the per-instruction live-after
    sets from [Liveness], removes [Move] instructions whose destination is dead
    after the instruction, and rewrites [Call] instructions to discard their
    result when the destination is dead but the call itself must still happen.

    The effect is that later passes and the emitter stop carrying obviously
    dead result traffic, especially pointless stores of call results and local
    copies that never feed another instruction.

    The rationale is the same as [asmcomp]'s early dead-code cleanup: do the
    cheap liveness-driven trimming before frame analysis and home assignment, so
    later passes do less work and frame layout does not count dead values. *)
val program: Types.Program.t -> Types.Program.t
