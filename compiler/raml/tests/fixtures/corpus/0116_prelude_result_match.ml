let bump_if_ok value =
  match value with
  | Ok n -> Ok (n + 1)
  | Error msg -> Error msg

let () =
  match bump_if_ok (Ok 41) with
  | Ok n -> Printf.printf "%d\n" n
  | Error msg -> print_endline msg
