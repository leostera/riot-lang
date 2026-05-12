(* Memoization with Hashtbl. *)
let fib =
  let memo = Hashtbl.create 64 in
  let rec fib n =
    match Hashtbl.find_opt memo n with
    | Some v -> v
    | None ->
        let v =
          if n <= 1 then n else fib (n - 1) + fib (n - 2)
        in
        Hashtbl.add memo n v;
        v
  in
  fib

let () = Printf.printf "%d\n" (fib 30)
