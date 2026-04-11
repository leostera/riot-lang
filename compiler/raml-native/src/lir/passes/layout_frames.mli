(** Compute explicit frame metadata for [LIR] procedures. The pass uses
    [Frame_analysis] to decide whether a frame is needed, assigns stable
    stack-slot offsets, records homes for the virtual values that live in that
    frame, and finishes by computing an aligned frame size. That gives later
    passes and emitters concrete layout information instead of forcing them to
    rebuild it themselves. *)
val program: Types.Program.t -> Types.Program.t
