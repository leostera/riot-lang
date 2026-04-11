let double_if_some value =
  match value with
  | None -> None
  | Some n -> Some (n + n)

let () =
  match double_if_some (Some 21) with
  | Some n -> Printf.printf "%d\n" n
  | None -> print_endline "none"
