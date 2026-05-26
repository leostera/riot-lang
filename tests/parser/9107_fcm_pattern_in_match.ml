(* TEST_BELOW *)

match x with
| (module M) -> M.value
| _ -> 0
