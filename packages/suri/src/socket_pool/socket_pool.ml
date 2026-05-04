open Std
open Std.Collections

module Connection = Connection
module Handler = Handler
module Transport = Transport

(** SocketPool supervisor state *)
type ('s, 'e) pool_state = {
  listener: Net.TcpListener.t;
  buffer_size: int;
  handler: ('s, 'e) Handler.handler;
  initial_ctx: 's;
  transport: Transport.t;
  acceptor_supervisor: Supervisor.Dynamic.t;
}

type error =
  | InvalidAddress of Net.Addr.error
  | BindFailed of Net.TcpListener.error
  | InvalidAcceptors of int
  | InvalidBufferSize of int

let validate_start_options = fun ~acceptors ~buffer_size ->
  if acceptors <= 0 then
    Error (InvalidAcceptors acceptors)
  else if buffer_size <= 0 then
    Error (InvalidBufferSize buffer_size)
  else
    Ok ()

(** Start a supervised pool of acceptors *)
let start_link = fun
  ~host
  ~port
  ?(acceptors = 100)
  ?(buffer_size = 4_096)
  ?(transport = Transport.tcp ())
  (type s e)
  (handler: (s, e) Handler.handler)
  (initial_ctx: s) ->
  match validate_start_options ~acceptors ~buffer_size with
  | Error error -> Error error
  | Ok () ->
      match Net.Addr.from_host_and_port ~host ~port with
      | Error error -> Error (InvalidAddress error)
      | Ok addr -> (
          match Net.TcpListener.bind ~reuse_addr:true ~reuse_port:false addr with
          | Error error -> Error (BindFailed error)
          | Ok listener ->
              Log.info ("Listening on " ^ host ^ ":" ^ (Int.to_string port));
              (* Start a dynamic supervisor for acceptors *)
              let acceptor_supervisor =
                Supervisor.Dynamic.start_link
                  ~intensity:{ max_restarts = 10; window = Time.Duration.from_secs 60 }
                  ~max_children:(acceptors * 2)
                  ()
              in
              (* Spawn acceptors under the supervisor *)
              for i = 1 to acceptors do
                let start () =
                  let state =
                    Acceptor.{
                      listener;
                      buffer_size;
                      handler;
                      initial_ctx;
                      transport;
                    }
                  in
                  Acceptor.spawn state
                in
                match Supervisor.Dynamic.start_child
                  acceptor_supervisor
                  ~start
                  ~restart:Permanent
                  ~shutdown:(Timeout (Time.Duration.from_secs 5))
                  () with
                | Ok _pid -> ()
                | Error err -> Log.error ("Failed to start acceptor: " ^ err)
              done;
              Ok acceptor_supervisor
        )
