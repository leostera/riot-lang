(** This pass assigns homes to virtual values after frame layout is already
    known.

    The algorithm reuses the per-procedure analysis gathered during
    [layout_frames], zips the ordered virtual names with the ordered stack
    slots, and records that mapping as explicit frame homes.

    The effect is that the compiler has a real seam between “what does the
    frame look like?” and “where does each virtual value live?”.

    The rationale is to keep location assignment separate from frame layout so
    later work can replace this simple stack-only mapping with a richer
    allocator without rewriting frame construction again. *)
type analysis = Layout_frames.analysis
val program: analysis:analysis -> Types.Program.t -> Types.Program.t
