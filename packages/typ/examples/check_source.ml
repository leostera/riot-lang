open Std

let source = {|
let id x = x
let answer = id 42
|}

let main = fun ~args:_ ->
  let result = Typ.Batch.check_source ~filename:(Path.v "example.ml") source in
  println (Typ.Report.render_report result);
  Ok ()

let () = Actors.run ~main ~args:Env.args ()
