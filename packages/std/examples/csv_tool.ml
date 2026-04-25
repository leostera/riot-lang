open Std

let main ~args:_ =
  println "csv_tool example";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
