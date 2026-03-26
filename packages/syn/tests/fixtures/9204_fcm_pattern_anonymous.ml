(* TEST_BELOW *)

let ignore_typed packed =
  let (module _ : S) = packed in
  ()

let ignore_plain packed =
  let (module _) = packed in
  ()
