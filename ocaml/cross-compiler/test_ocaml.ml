(* Simple OCaml test program *)
let () =
  print_endline "Hello from OCaml cross-compiled binary!";
  Printf.printf "OCaml version: %s\n" Sys.ocaml_version;
  Printf.printf "OS type: %s\n" Sys.os_type;
  Printf.printf "Word size: %d bits\n" Sys.word_size;
  exit 42