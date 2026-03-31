(** Re-export Stdlib.Int for packages that need it *)
include Stdlib.Int

let of_string = Stdlib.int_of_string
