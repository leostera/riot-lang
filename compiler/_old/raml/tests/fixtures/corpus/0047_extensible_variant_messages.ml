(* Extensible variants without objects. *)
type msg = ..

type msg +=
  | Ping
  | Data of int
  | Quit

let handle = function
  | Ping -> "ping"
  | Data n -> "data:" ^ string_of_int n
  | Quit -> "quit"
  | _ -> "unknown"

let () =
  let xs = [ Ping; Data 42; Quit ] in
  List.iter (fun m -> Printf.printf "%s " (handle m)) xs;
  print_newline ()
