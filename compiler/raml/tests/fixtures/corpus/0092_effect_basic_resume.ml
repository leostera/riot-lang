(* Basic algebraic effect handling. *)
open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int t

let comp () =
  perform (Xchg 10) + perform (Xchg 20)

let () =
  let result =
    try comp () with
    | effect (Xchg n), k -> continue k (n + 1)
  in
  Printf.printf "%d\n" result
