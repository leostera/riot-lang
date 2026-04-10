(* Ephemerons. *)
let () =
  let key = "key" in
  let eph = Ephemeron.K1.make key 99 in
  let value =
    match Ephemeron.K1.query eph key with
    | None -> -1
    | Some n -> n
  in
  Printf.printf "%d\n" value
