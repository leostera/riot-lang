(** Stub implementation of Riot runtime for compiler bootstrapping *)
(** This module is replaced by the real Riot implementation when available *)

module Runtime : sig
  val increment_reduction_count : unit -> unit
  (** No-op stub for reduction counting. Real implementation tracks reductions
      and yields to scheduler when threshold is reached. *)
end