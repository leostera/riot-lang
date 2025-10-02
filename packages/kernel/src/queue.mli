(** Re-export Stdlib.Queue for packages that need it *)

include module type of Stdlib.Queue
