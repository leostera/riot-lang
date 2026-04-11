(* Top-level function with a tail conditional and a later direct call. *)
let choose flag when_true when_false =
  if flag then when_true else when_false

let selected = choose true "left" "right"
