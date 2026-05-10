open Std
open IO

type Message.t +=
  | Udp_server_received of string

let string_of_udp_error = fun (Net.UdpSocket.System_error err) -> IO.error_message err

let string_of_udp_server_error = fun (Net.UdpServer.System_error err) -> IO.error_message err

let local_udp_addr = fun port -> Net.Addr.udp Net.Addr.loopback port

let bind_socket = fun addr ->
  match Net.UdpSocket.bind addr with
  | Ok socket -> Ok socket
  | Error err -> Error (string_of_udp_error err)

let test_udp_server_serves_one_datagram = fun _ctx ->
  let parent = Runtime.self () in
  let handler ~socket ~from payload ~len =
    let received =
      Bytes.sub payload ~offset:0 ~len
      |> Result.expect ~msg:"server handler payload slice should be valid"
      |> Bytes.to_string
    in
    Runtime.send parent (Udp_server_received received);
    let _ =
      match Net.UdpSocket.send_to socket from (Bytes.from_string "pong") () with
      | Ok _ -> Ok ()
      | Error _err -> Ok ()
    in
    Net.UdpSocket.close socket
  in
  match Net.UdpServer.bind (local_udp_addr 0) ~handler with
  | Error err -> Error ("UdpServer.bind failed: " ^ string_of_udp_server_error err)
  | Ok server ->
      let _server_pid =
        Runtime.spawn
          (fun () ->
            let _ =
              match Net.UdpServer.serve server with
              | Ok () -> Ok ()
              | Error _err -> Ok ()
            in
            Ok ())
      in
      match bind_socket (local_udp_addr 0) with
      | Error err ->
          Net.UdpServer.close server;
          Error err
      | Ok client ->
          let buffer = Bytes.create ~size:32 in
          match Net.UdpSocket.send_to
            client
            (Net.UdpServer.local_addr server)
            (Bytes.from_string "ping")
            () with
          | Error err ->
              Net.UdpSocket.close client;
              Net.UdpServer.close server;
              Error ("client send_to server failed: " ^ string_of_udp_error err)
          | Ok _ ->
              match Runtime.receive
                ~selector:(fun __tmp1 ->
                  match __tmp1 with
                  | Udp_server_received payload -> Select payload
                  | _ -> Skip)
                ~timeout:1.0
                () with
              | payload when not (String.equal payload "ping") ->
                  Net.UdpSocket.close client;
                  Error "UdpServer handler received the wrong payload"
              | _ ->
                  match Net.UdpSocket.recv_from
                    client
                    buffer
                    ~timeout:(Time.Duration.from_millis 500)
                    () with
                  | Error err ->
                      Net.UdpSocket.close client;
                      Error ("client recv_from server reply failed: " ^ string_of_udp_error err)
                  | Ok { bytes_read; _ } ->
                      Net.UdpSocket.close client;
                      if String.equal
                        (
                          Bytes.sub buffer ~offset:0 ~len:bytes_read
                          |> Result.expect ~msg:"server reply slice should be valid"
                          |> Bytes.to_string
                        )
                        "pong" then
                        Ok ()
                      else
                        Error "UdpServer reply payload was incorrect"

let tests =
  Test.[ case "UdpServer bind and serve handle a datagram" test_udp_server_serves_one_datagram ]

let main ~args = Test.Cli.main ~name:"std_net_udp_server_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
