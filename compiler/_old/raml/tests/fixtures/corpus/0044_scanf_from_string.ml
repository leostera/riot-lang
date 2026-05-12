(* Text scanning with Scanf.sscanf. *)
let a, b, c =
  Scanf.sscanf "12,34,56" "%d,%d,%d" (fun a b c -> (a, b, c))

let () = Printf.printf "%d\n" (a + b + c)
