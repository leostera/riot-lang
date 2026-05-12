(* Sequencing before a local conditional result. *)
let choose flag =
  let consume value = value in
  consume ();
  let selected = if flag then "left" else "right" in
  selected

let left = choose true

let right = choose false
