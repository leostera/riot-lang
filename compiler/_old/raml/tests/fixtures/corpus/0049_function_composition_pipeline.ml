(* Higher-order function composition. *)
let compose f g x = f (g x)

let pipeline =
  compose
    (fun x -> x + 1)
    (compose
       (fun x -> x * 2)
       (fun x -> x - 3))

let () = Printf.printf "%d\n" (pipeline 25)
