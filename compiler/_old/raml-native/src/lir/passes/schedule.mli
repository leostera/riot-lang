(** Clean up the linear control-flow graph in [LIR]. The pass collapses label
    chains, rewrites branch targets, removes unreachable code, drops
    fallthrough jumps, and deletes unused labels so the emitter sees a compact
    linear program instead of leftover lowering artifacts. *)
val program: Types.Program.t -> Types.Program.t
