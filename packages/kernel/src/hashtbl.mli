(** Re-export Stdlib.Hashtbl for packages that need it *)

include module type of Stdlib.Hashtbl
