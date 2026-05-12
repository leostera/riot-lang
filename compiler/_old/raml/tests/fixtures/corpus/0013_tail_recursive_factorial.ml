(* Tail-recursive integer loop. *)
let fact n =
  let rec loop acc n =
    if n <= 1 then acc else loop (acc * n) (n - 1)
  in
  loop 1 n

let () = Printf.printf "%d\n" (fact 10)
