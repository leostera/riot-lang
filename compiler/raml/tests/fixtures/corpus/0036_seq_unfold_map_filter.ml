(* Lazy sequences with map and filter. *)
let rec range a b () =
  if a > b then Seq.Nil
  else Seq.Cons (a, range (a + 1) b)

let () =
  range 1 10
  |> Seq.filter (fun x -> x mod 2 = 0)
  |> Seq.map (fun x -> x * x)
  |> List.from_seq
  |> List.iter (fun x -> Printf.printf "%d " x);
  print_newline ()
