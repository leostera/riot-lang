open Std
open IO

type Message.t +=
  | Udp_server_received of string

let string_of_udp_error = function
  | Net.UdpSocket.System_error err -> IO.error_message err

let string_of_udp_server_error = function
  | Net.UdpServer.System_error err -> IO.error_message err

let local_udp_addr = fun port ->
  Net.Addr.udp Net.Addr.loopback port

let bind_socket = fun addr ->
  match Net.UdpSocket.bind addr with
  | Ok socket -> Ok socket
  | Error err -> Error (string_of_udp_error err)

let test_udp_socket_send_to_and_recv_from = fun _ctx ->
  match (bind_socket (local_udp_addr 0), bind_socket (local_udp_addr 0)) with
  | (Error err, _)
  | (_, Error err) -> Error err
  | Ok server, Ok client ->
      let server_addr = Net.UdpSocket.local_addr server in
      let client_addr = Net.UdpSocket.local_addr client in
      let server_buffer = Bytes.create ~size:32 in
      let client_buffer = Bytes.create ~size:32 in
      let ping = Bytes.from_string "ping" in
      let pong = Bytes.from_string "pong" in
      match Net.UdpSocket.send_to client server_addr ping () with
      | Error err ->
          Net.UdpSocket.close client;
          Net.UdpSocket.close server;
          Error ("client send_to failed: " ^ string_of_udp_error err)
      | Ok _ -> (
          match Net.UdpSocket.recv_from
            server
            server_buffer
            ~timeout:(Time.Duration.from_millis 500)
            () with
          | Error err ->
              Net.UdpSocket.close client;
              Net.UdpSocket.close server;
              Error ("server recv_from failed: " ^ string_of_udp_error err)
          | Ok { bytes_read; from } ->
              if not
                  (
                    String.equal
                      (Bytes.sub server_buffer ~offset:0 ~len:bytes_read
                      |> Result.expect ~msg:"server datagram slice should be valid"
                      |> Bytes.to_string)
                      "ping"
                  ) then
                (
                  Net.UdpSocket.close client;
                  Net.UdpSocket.close server;
                  Error "server received the wrong datagram payload"
                )
              else if not (Int.equal (Net.Addr.port from) (Net.Addr.port client_addr)) then
                (
                  Net.UdpSocket.close client;
                  Net.UdpSocket.close server;
                  Error "server recv_from should report the sender port"
                )
              else
                match Net.UdpSocket.send_to server from pong () with
                | Error err ->
                    Net.UdpSocket.close client;
                    Net.UdpSocket.close server;
                    Error ("server send_to failed: " ^ string_of_udp_error err)
                | Ok _ -> (
                    match Net.UdpSocket.recv_from
                      client
                      client_buffer
                      ~timeout:(Time.Duration.from_millis 500)
                      () with
                    | Error err ->
                        Net.UdpSocket.close client;
                        Net.UdpSocket.close server;
                        Error ("client recv_from failed: " ^ string_of_udp_error err)
                    | Ok { bytes_read; from } ->
                        Net.UdpSocket.close client;
                        Net.UdpSocket.close server;
                        if not
                            (
                              String.equal
                                (Bytes.sub client_buffer ~offset:0 ~len:bytes_read
                                |> Result.expect ~msg:"client datagram slice should be valid"
                                |> Bytes.to_string)
                                "pong"
                            ) then
                          Error "client received the wrong datagram payload"
                        else if not (Int.equal (Net.Addr.port from) (Net.Addr.port server_addr)) then
                          Error "client recv_from should report the server port"
                        else
                          Ok ()
                  )
        )

let test_udp_socket_connect_supports_send_and_recv = fun _ctx ->
  match (bind_socket (local_udp_addr 0), bind_socket (local_udp_addr 0)) with
  | (Error err, _)
  | (_, Error err) -> Error err
  | Ok server, Ok client ->
      let server_addr = Net.UdpSocket.local_addr server in
      let client_addr = Net.UdpSocket.local_addr client in
      let server_buffer = Bytes.create ~size:32 in
      let client_buffer = Bytes.create ~size:32 in
      match (Net.UdpSocket.connect server client_addr, Net.UdpSocket.connect client server_addr) with
      | (Error err, _)
      | (_, Error err) ->
          Net.UdpSocket.close client;
          Net.UdpSocket.close server;
          Error ("udp connect failed: " ^ string_of_udp_error err)
      | Ok (), Ok () -> (
          match Net.UdpSocket.send client (Bytes.from_string "hello") () with
          | Error err ->
              Net.UdpSocket.close client;
              Net.UdpSocket.close server;
              Error ("connected send failed: " ^ string_of_udp_error err)
          | Ok _ -> (
              match Net.UdpSocket.recv
                server
                server_buffer
                ~timeout:(Time.Duration.from_millis 500)
                () with
              | Error err ->
                  Net.UdpSocket.close client;
                  Net.UdpSocket.close server;
                  Error ("connected recv failed: " ^ string_of_udp_error err)
              | Ok bytes_read ->
                  if not
                      (
                        String.equal
                          (Bytes.sub server_buffer ~offset:0 ~len:bytes_read
                          |> Result.expect ~msg:"connected server datagram slice should be valid"
                          |> Bytes.to_string)
                          "hello"
                      ) then
                    (
                      Net.UdpSocket.close client;
                      Net.UdpSocket.close server;
                      Error "connected recv returned the wrong payload"
                    )
                  else
                    match Net.UdpSocket.send server (Bytes.from_string "world") () with
                    | Error err ->
                        Net.UdpSocket.close client;
                        Net.UdpSocket.close server;
                        Error ("connected reply send failed: " ^ string_of_udp_error err)
                    | Ok _ -> (
                        match Net.UdpSocket.recv
                          client
                          client_buffer
                          ~timeout:(Time.Duration.from_millis 500)
                          () with
                        | Error err ->
                            Net.UdpSocket.close client;
                            Net.UdpSocket.close server;
                            Error ("connected reply recv failed: " ^ string_of_udp_error err)
                        | Ok reply_bytes ->
                            Net.UdpSocket.close client;
                            Net.UdpSocket.close server;
                            if String.equal
                                (Bytes.sub client_buffer ~offset:0 ~len:reply_bytes
                                |> Result.expect ~msg:"connected client datagram slice should be valid"
                                |> Bytes.to_string)
                                "world" then
                              Ok ()
                            else
                              Error "connected reply recv returned the wrong payload"
                      )
            )
        )

let test_udp_server_serves_one_datagram = fun _ctx ->
  let parent = Runtime.self () in
  let handler ~socket ~from payload ~len =
    let received = Bytes.sub payload ~offset:0 ~len
    |> Result.expect ~msg:"server handler payload slice should be valid"
    |> Bytes.to_string in
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
  | Ok server -> (
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
          | Ok _ -> (
              match
                Runtime.receive
                  ~selector:(
                    function
                    | Udp_server_received payload -> `select payload
                    | _ -> `skip
                  )
                  ~timeout:1.0
                  ()
              with
              | payload when not (String.equal payload "ping") ->
                  Net.UdpSocket.close client;
                  Error "UdpServer handler received the wrong payload"
              | _ -> (
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
                          (Bytes.sub buffer ~offset:0 ~len:bytes_read
                          |> Result.expect ~msg:"server reply slice should be valid"
                          |> Bytes.to_string)
                          "pong" then
                        Ok ()
                      else
                        Error "UdpServer reply payload was incorrect"
                )
            )
    )

let tests =
  Test.[
    case "UdpSocket send_to and recv_from preserve datagram boundaries" test_udp_socket_send_to_and_recv_from;
    case "UdpSocket connect enables send and recv without explicit peers" test_udp_socket_connect_supports_send_and_recv;
    case "UdpServer bind and serve handle a datagram" test_udp_server_serves_one_datagram;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"std_net_udp" ~tests ~args) ~args:Env.args ()
