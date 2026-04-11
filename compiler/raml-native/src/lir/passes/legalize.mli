(** This pass rewrites home-assigned [LIR] into forms the AArch64 Darwin
    emitter can treat as direct target operations instead of ad hoc reload
    cases.

    It introduces explicit scratch-register moves for cases like stack-to-stack
    copies, stack-backed indirect callees, zero-branches on non-register
    operands, and returns from non-return-register locations, using the active
    native target profile from the shared compilation context.

    The effect is that the emitter stops inventing these reloads on demand and
    instead consumes an instruction stream that already makes temporary value
    movement visible in snapshots.

    The rationale is the same as [asmcomp]'s reload layer: after allocation,
    there is still target-specific legalization work to do before emission, and
    that work belongs in the compiler pipeline, not hidden inside the renderer.
    The full compilation context is the input for exactly that reason. *)
val program: ctx:Raml_core.Compilation_context.t -> Types.Program.t -> Types.Program.t
