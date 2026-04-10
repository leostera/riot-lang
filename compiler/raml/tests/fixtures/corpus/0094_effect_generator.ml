(* Generator-style effect collection. *)
open Effect
open Effect.Deep

type _ Effect.t += Yield : int -> unit t

let yield n = perform (Yield n)

let rec traverse = function
  | [] -> ()
  | x :: xs ->
      yield x;
      traverse xs

let collect f =
  let acc = ref [] in
  let rec loop thunk =
    match thunk () with
    | () -> List.rev !acc
    | effect (Yield n), k ->
        acc := n :: !acc;
        loop (fun () -> continue k ())
  in
  loop f

let () =
  collect (fun () -> traverse [ 1; 2; 3; 4 ])
  |> List.iter (fun x -> Printf.printf "%d " x);
  print_newline ()
