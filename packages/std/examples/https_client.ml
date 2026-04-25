(** Simple HTTPS client example using TlsStream *)
open Std

let main ~args:_ =
  println "https_client example";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
