open Std

(** Execute the clean command *)
let run _args =
  println "🧹 Cleaning build artifacts...";
  let target_dir = Path.v "./target" in
  match Fs.remove_dir_all target_dir with
  | Ok () ->
      println "Build artifacts cleaned!";
      Ok ()
  | Error _ -> Error (Failure "Failed to clean build artifacts")
