external identity : int -> int = "zort_identity"

let run x =
  identity x

let () = Callback.register "zort_e2e_external_identity" run
