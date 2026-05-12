(* Integer-only tail loop intended to be allocation-light. *)
let dot n =
  let rec loop acc i =
    if i = n then acc
    else
      let x = Sys.opaque_identity i in
      loop (acc + (x * 3) - 1) (i + 1)
  in
  loop 0 0

let () = Printf.printf "%d\n" (dot 20000)
