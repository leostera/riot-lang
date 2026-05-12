(* Source-level direct `>=` calls must lower through the JS runtime boundary. *)
let no_smaller = 5 >= 3

let () = Printf.printf "%b\n" no_smaller
