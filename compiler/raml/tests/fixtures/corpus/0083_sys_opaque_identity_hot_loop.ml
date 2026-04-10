(* Opaque identity in an integer hot loop. *)
let sum_upto n =
  let rec loop acc i =
    if i > n then acc
    else
      let i = Sys.opaque_identity i in
      loop (acc + i) (i + 1)
  in
  loop 0 0

let () = Printf.printf "%d\n" (sum_upto 40000)
