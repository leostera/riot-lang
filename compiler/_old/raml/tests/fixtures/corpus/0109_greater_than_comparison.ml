(* Source-level direct `>` calls must lower through the JS runtime boundary. *)
let larger = 5 > 3

let () = Printf.printf "%b\n" larger
