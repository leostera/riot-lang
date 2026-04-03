open Std

let main ~args:_ =
  println "Hello, world!";
  Ok ()

let () = Actors.run ~main ~args:Env.args ()
