open Std
open Std.Time
open Miniriot

type Message.t += Go

let main ~args =
  println "hello world: %s" Datetime.(now () |> to_iso8601);

  let _ref = Timer.send_after (self ()) ~after:0.001 Go in

  let selector msg = if msg = Go then `select () else `skip in

  let () = receive ~selector () in

  println "ohnoe world: %s" Datetime.(now () |> to_iso8601);

  Ok ()

let () = Miniriot.run ~main ~args:Env.args ()
