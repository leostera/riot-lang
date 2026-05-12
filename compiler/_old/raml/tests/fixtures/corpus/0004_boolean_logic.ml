(* Boolean operators and comparisons. *)
let p = true
let q = false
let r = (not q && p) || (3 < 2)

let () = Printf.printf "%b\n" r
