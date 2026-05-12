(* Simple conditional control flow. *)
let choose cond =
  if cond then 1 else 0

let () = Printf.printf "%d\n" (choose true)
