(** This pass computes frame layout, not value locations.

    The algorithm asks [Frame_analysis] which virtual values need storage,
    decides whether the procedure needs a frame at all, allocates a stable
    stack slot for each required value, and computes the final aligned frame
    size.

    The effect is that later passes can talk about concrete frame shape
    without yet committing to how virtual values are mapped onto those slots.

    The rationale is to keep frame layout separate from home assignment, which
    is the same ownership split `asmcomp` keeps between stack-frame analysis
    and later location work. *)
type analysis
val program_with_analysis: Types.Program.t -> Types.Program.t * analysis

val program: Types.Program.t -> Types.Program.t
