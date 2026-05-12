(* Option-based control flow. *)
let safe_div a b =
  if b = 0 then None else Some (a / b)

let result =
  match safe_div 84 2 with
  | None -> None
  | Some x -> safe_div x 3

let () =
  match result with
  | Some n -> Printf.printf "%d\n" n
  | None -> print_endline "none"
