(* Trampolining to avoid stack growth. *)
type 'a bounce =
  | Done of 'a
  | More of (unit -> 'a bounce)

let rec fact n acc =
  if n <= 1 then Done acc
  else More (fun () -> fact (n - 1) (n * acc))

let rec run = function
  | Done x -> x
  | More f -> run (f ())

let () = Printf.printf "%d\n" (run (fact 10 1))
