(** This pass assigns concrete homes to [LIR] virtual values after frame
    analysis has already run.

    The algorithm computes live intervals, reads the allocatable register pools
    from the active native target profile, uses a caller-saved pool for
    short-lived values, uses a callee-saved pool for values that stay live
    across calls, and reuses stack slots for spilled intervals whose live
    ranges do not overlap.

    The effect is that [LIR] leaves this pass with explicit register or stack
    homes, plus a frame whose slot count matches the values that actually
    interfered instead of reserving one stack slot per spilled name forever.

    The rationale is to make home allocation a real compiler pass instead of an
    emitter convention, while keeping the first allocator simple and honest:
    caller-saved registers for cheap temporaries, callee-saved registers for
    call-live values, and reused stack slots for the rest. *)
type analysis = Layout_frames.analysis
val program:
  ctx:Raml_core.Compilation_context.t -> analysis:analysis -> Types.Program.t -> Types.Program.t
