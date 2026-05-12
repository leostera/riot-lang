(* User-defined state effect. *)
open Effect
open Effect.Deep

type _ Effect.t +=
  | Get : int t
  | Put : int -> unit t

let get () = perform Get
let put n = perform (Put n)

let program () =
  let x = get () in
  put (x + 10);
  let y = get () in
  x + y

let rec run state f =
  match f () with
  | v -> (state, v)
  | effect Get, k -> run state (fun () -> continue k state)
  | effect (Put n), k -> run n (fun () -> continue k ())

let () =
  let state, value = run 5 program in
  Printf.printf "%d %d\n" state value
