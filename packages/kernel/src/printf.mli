(** Re-export Stdlib.Printf for packages that need it *)

include module type of Stdlib.Printf
