open Std

let main ~args:_ =
  println "Hello, World!";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
