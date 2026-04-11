(** This pass assigns concrete homes to [LIR] virtual values after frame
    analysis has already run.

    The algorithm computes live intervals, uses a small caller-saved register
    pool for values that are not live across calls, and spills the rest to
    stack slots with stable offsets.

    The effect is that [LIR] leaves this pass with explicit register or stack
    homes, plus a frame whose slot count matches the values that actually
    spilled.

    The rationale is to make home allocation a real compiler pass instead of an
    emitter convention, while keeping the first allocator simple and honest:
    registers for cheap temporaries, stack for call-live or overflowed values. *)
type analysis = Layout_frames.analysis
val program: analysis:analysis -> Types.Program.t -> Types.Program.t
