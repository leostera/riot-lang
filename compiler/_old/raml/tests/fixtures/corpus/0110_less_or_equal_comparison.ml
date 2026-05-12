(* Source-level direct `<=` calls must lower through the JS runtime boundary. *)
let no_larger = 3 <= 5

let () = Printf.printf "%b\n" no_larger
