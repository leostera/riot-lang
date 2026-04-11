(** This module computes liveness over linear [LIR] procedures and derives the
    coarse live intervals that home allocation needs.

    It builds a control-flow graph from labels, jumps, and zero-branches, runs
    the usual backward dataflow equations to a fixpoint, then records for each
    virtual name the first and last instruction positions where it is live or
    mentioned.

    The result gives later passes a stable interval per virtual name plus a
    flag telling them whether the value is live across a call site. That is
    enough for a first linear-scan style allocator to keep cheap temporaries in
    caller-saved registers and spill the rest. *)
type live_set = string Std.Collections.HashSet.t
type point = {
  instruction: Types.Instruction.t;
  live_before: live_set;
  live_after: live_set;
}
type interval = {
  name: string;
  start: int;
  finish: int;
  live_across_call: bool;
}
val points_of_procedure: Types.Procedure.t -> point list

val intervals_of_procedure: Types.Procedure.t -> interval list
