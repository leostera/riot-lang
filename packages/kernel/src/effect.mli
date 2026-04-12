(** Re-export the compiler-owned effect surface needed by Riot's runtime. *)
include module type of Stdlib.Effect
