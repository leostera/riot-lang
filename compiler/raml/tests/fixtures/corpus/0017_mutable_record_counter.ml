(* Mutable record fields. *)
type counter = { mutable value : int }

let bump c = c.value <- c.value + 1

let () =
  let c = { value = 0 } in
  bump c;
  bump c;
  bump c;
  Printf.printf "%d\n" c.value
