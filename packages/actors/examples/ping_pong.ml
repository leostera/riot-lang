open Std
open Actors

type Message.t +=
  | Ping of Pid.t
  | Pong

let worker = fun () ->
  let sender =
    receive
      ~selector:(
        function
        | Ping sender -> `select sender
        | _ -> `skip
      )
      ()
  in
  send sender Pong;
  Ok ()

let main = fun ~args:_ ->
  let worker_pid = spawn worker in
  send worker_pid (Ping (self ()));
  receive
    ~selector:(
      function
      | Pong ->
          Kernel.println "received pong";
          `select ()
      | _ -> `skip
    )
    ();
  Ok ()

let () = run ~main ~args:Env.args ()
