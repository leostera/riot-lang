(* Local exception used for non-local exit. *)
let first_even xs =
  let exception Found of int in
  try
    List.iter
      (fun x -> if x mod 2 = 0 then raise (Found x))
      xs;
    None
  with
  | Found x -> Some x

let () =
  match first_even [ 1; 3; 5; 8; 9 ] with
  | None -> print_endline "none"
  | Some x -> Printf.printf "%d\n" x
