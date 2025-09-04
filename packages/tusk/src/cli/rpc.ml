(** RPC command implementation *)

let run args =
  Printf.printf "RPC command with args: %s\n" (String.concat " " args)
