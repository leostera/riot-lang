(* Local function binding captures an outer parameter and is called later. *)
let choose flag =
  let decide when_true when_false =
    if flag then when_true else when_false
  in
  decide "left" "right"

let selected = choose true
