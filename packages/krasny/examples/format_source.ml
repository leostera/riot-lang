open Std

exception Format_failed of string

let source = "let  add  x   y= x+y"

let main ~args:_ =
  let parsed = Krasny.parse_source ~filename:(Path.v "example.ml") source in
  match Krasny.stream_format_to_string parsed ~width:100 with
  | Ok formatted ->
      println formatted;
      Ok ()
  | Error err -> Error (Format_failed (Krasny.format_error_to_string err))

let () = Runtime.run ~main ~args:Env.args ()
