(* Binding operators. *)
let ( let* ) o f =
  match o with
  | Some x -> f x
  | None -> None

let ( let+ ) o f =
  match o with
  | Some x -> Some (f x)
  | None -> None

let parse s =
  try Some (int_of_string s) with
  | Failure _ -> None

let result =
  let* x = parse "21" in
  let+ y = parse "2" in
  x * y

let () =
  match result with
  | Some n -> Printf.printf "%d\n" n
  | None -> print_endline "none"
