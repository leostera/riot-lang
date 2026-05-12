(* Weak pointers. *)
let () =
  let w = Weak.create 3 in
  let a = "keep" in
  let b = "also" in
  Weak.set w 0 (Some a);
  Weak.set w 1 (Some b);
  let show i =
    match Weak.get w i with
    | None -> "none"
    | Some s -> s
  in
  Printf.printf "%s %s %s\n" (show 0) (show 1) (show 2)
