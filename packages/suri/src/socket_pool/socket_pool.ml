open Std
open Std.Collections

module Connection = Connection
module Handler = Handler
module Transport = Transport

let start_link ~host ~port ?(acceptors = 100) ?(buffer_size = 4096)
    ?(transport = Transport.tcp ()) (type s e)
    (handler : (module Handler.Intf with type state = s and type error = e))
    (initial_ctx : s) =
  match Net.Addr.of_host_and_port ~host ~port with
  | Error _ -> Error `Bind_error
  | Ok addr -> (
      match Net.TcpListener.bind ~reuse_addr:true ~reuse_port:false addr with
      | Error _ -> Error `Bind_error
      | Ok listener ->
          Log.info "Listening on 0.0.0.0:%d" port;
          let _acceptor_pids =
            List.make ~len:acceptors ~fn:(fun _ ->
                let state =
                  Acceptor.
                    { listener; buffer_size; handler; initial_ctx; transport }
                in
                Acceptor.start_link state)
          in
          Ok ())
