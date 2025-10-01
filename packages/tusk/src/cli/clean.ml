open Std

(** Execute the clean command *)
let run _args =
  Printf.printf "🧹 Cleaning build artifacts...\n%!";
  let target_dir = Path.v "./target" in
  match Fs.remove_dir_all target_dir with
  | Ok () ->
      Printf.printf "Build artifacts cleaned!\n%!";
      Ok ()
  | Error _ -> Error (Failure "Failed to clean build artifacts")
