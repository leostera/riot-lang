(* Top-level mutual recursion should stay one grouped recursive init slice. *)
let rec even n =
  if n <= 0 then true else odd (n - 1)
and odd n =
  if n <= 0 then false else even (n - 1)

let result = even 10

let () = Printf.printf "%b\n" result
