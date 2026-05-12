(* Source-level direct `<` calls must lower through the JS runtime boundary. *)
let smaller = 3 < 5

let () = Printf.printf "%b\n" smaller
