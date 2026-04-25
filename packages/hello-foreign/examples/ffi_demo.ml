open Std
open Hello_foreign

let main ~args:_ =
  let num = 21 in
  let doubled = Bindings.double num in
  let plus_ten = Bindings.add_ten num in
  println ("double(21) = " ^ Int.to_string doubled);
  println ("add_ten(21) = " ^ Int.to_string plus_ten);
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
