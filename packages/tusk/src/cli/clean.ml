open Std

(** Execute the clean command *)
let run _args =
  Printf.printf "🧹 Cleaning build artifacts...\n%!";
  let result = Command.system "rm -rf ./target" in
  match Command.of_unix_status result with
  | Command.Exited 0 ->
      Printf.printf "Build artifacts cleaned!\n%!";
      Ok ()
  | _ -> Error (Failure "Failed to clean build artifacts")
