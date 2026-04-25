open Std

exception Format_failed of string

let source = "let  add  x   y= x+y"

let main ~args:_ =
  let parsed = Syn.parse ~filename:(Path.v "example.ml") source in
  match Krasny.format parsed with
  | Ok formatted ->
      println formatted;
      Ok ()
  | Error err -> Error (Format_failed (Krasny.format_error_to_string err))

let () = Runtime.run ~main ~args:Env.args ()
