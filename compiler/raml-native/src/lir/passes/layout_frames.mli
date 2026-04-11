(** Compute explicit frame metadata for [LIR] procedures. The pass uses
    [Frame_analysis] to decide whether a frame is needed, assigns stable
    stack-slot offsets, and records an aligned frame size so emitters can
    consume frame layout directly instead of rebuilding it themselves. *)
val program: Types.Program.t -> Types.Program.t
