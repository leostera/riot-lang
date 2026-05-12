(** This pass applies the target calling convention after homes and
    target-owned reloads are already explicit.

    For the current AArch64 Darwin slice it inserts entry moves from incoming
    argument registers into the assigned parameter homes, rewrites call
    arguments to explicit pre-call moves into the target profile's argument
    registers, and materializes call results as explicit post-call moves from
    the target profile's return register.

    The effect is that the emitter stops owning argument placement, parameter
    prologue moves, and call-result shuffling. Those ABI choices become normal
    compiler instructions that show up in snapshots.

    The rationale is the same one that drives the rest of this cleanup: the
    emitter should render target code, not decide calling-convention mechanics
    on the fly. The shared compilation context is the input because the pass is
    target-owned by design. *)
val program: ctx:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
