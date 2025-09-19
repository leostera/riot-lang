(* b.ml - depends on a (circular!) *)
let value_b = 20
let get_from_a () = A.value_a + 1