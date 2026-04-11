let result =
  let unused = 10 in
  let dead x = x + unused in
  42

let () = Printf.printf "%d\n" result
