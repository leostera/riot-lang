(* Integer bit operations. *)
let popcount n =
  let rec loop acc x =
    if x = 0 then acc
    else loop (acc + (x land 1)) (x lsr 1)
  in
  loop 0 n

let () =
  let x = 0b10110100 in
  Printf.printf "%d %d\n" (popcount x) (x lxor 0xFF)
