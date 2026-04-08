open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let string_of_error = function
  | Kernel.Error.Unknown code ->
      "unknown kernel error " ^ Int.to_string code
  | error ->
      Kernel.Error.to_string error

let lift = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_error error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with error ->
    finally ();
    raise error

let is_would_block = function
  | Kernel.Error.Would_block -> true
  | _ -> false

let close_stream = fun stream ->
  let _ = Kernel.Net.TcpStream.close stream in
  ()

let close_listener = fun listener ->
  let _ = Kernel.Net.TcpListener.close listener in
  ()

let close_udp = fun socket ->
  let _ = Kernel.Net.UdpSocket.close socket in
  ()

let with_poll = fun fn ->
  let* poll =
    lift (Kernel.Async.Poll.make ())
  in
  fn poll

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () =
    lift (Kernel.Async.Poll.register poll token interest source)
  in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events =
        lift (Kernel.Async.Poll.poll ~timeout:100_000_000L poll)
      in
      let found =
        List.exists
          (fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event)
            && pred event)
          events
      in
      if found then
        Ok ()
      else
        Error "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.readable
    ~source
    ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for
    poll
    ~token
    ~interest:Kernel.Async.Interest.writable
    ~source
    ~pred:Kernel.Async.Event.is_writable

let rec write_all_stream = fun poll ~token stream buffer ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    match Kernel.Net.TcpStream.write stream ~pos ~len buffer with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Error "expected tcp write to make progress"
        else
          write_all_stream
            poll
            ~token
            stream
            buffer
            ~pos:(pos + written)
            ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream)
          in
          write_all_stream poll ~token stream buffer ~pos ~len
        else
          Error (string_of_error error)

let rec write_all_vectored = fun poll ~token stream iov ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
    match Kernel.Net.TcpStream.write_vectored stream slice with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Error "expected tcp vectored write to make progress"
        else
          write_all_vectored
            poll
            ~token
            stream
            iov
            ~pos:(pos + written)
            ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream)
          in
          write_all_vectored poll ~token stream iov ~pos ~len
        else
          Error (string_of_error error)

let rec read_exact_stream = fun poll ~token stream buffer ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    match Kernel.Net.TcpStream.read stream ~pos ~len buffer with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Error "expected tcp read to make progress"
        else
          read_exact_stream
            poll
            ~token
            stream
            buffer
            ~pos:(pos + read)
            ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream)
          in
          read_exact_stream poll ~token stream buffer ~pos ~len
        else
          Error (string_of_error error)

let rec read_exact_vectored = fun poll ~token stream iov ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
    match Kernel.Net.TcpStream.read_vectored stream slice with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Error "expected tcp vectored read to make progress"
        else
          read_exact_vectored
            poll
            ~token
            stream
            iov
            ~pos:(pos + read)
            ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () =
            wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream)
          in
          read_exact_vectored poll ~token stream iov ~pos ~len
        else
          Error (string_of_error error)

let rec accept_stream = fun poll listener ->
  match Kernel.Net.TcpListener.accept listener with
  | Kernel.Result.Ok accepted -> Ok accepted
  | Kernel.Result.Error error ->
      if is_would_block error then
        let* () =
          wait_readable
            poll
            ~token:(Kernel.Async.Token.make 301)
            (Kernel.Net.TcpListener.to_source listener)
      in
        accept_stream poll listener
      else
        Error (string_of_error error)

let connect_stream = fun poll addr ->
  let* connect_result =
    lift (Kernel.Net.TcpStream.connect addr)
  in
  match connect_result with
  | Kernel.Net.TcpStream.Connected stream -> Ok stream
  | Kernel.Net.TcpStream.In_progress stream ->
      let* () =
        wait_writable
          poll
          ~token:(Kernel.Async.Token.make 302)
          (Kernel.Net.TcpStream.to_source stream)
      in
      Ok stream

let with_tcp_pair = fun fn ->
  with_poll
    (fun poll ->
      let* listener =
        lift (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let* listener_addr =
            lift (Kernel.Net.TcpListener.local_addr listener)
          in
          let* client =
            connect_stream poll listener_addr
          in
          protect
            ~finally:(fun () -> close_stream client)
            (fun () ->
              let* (server, peer) =
                accept_stream poll listener
              in
              protect
                ~finally:(fun () -> close_stream server)
                (fun () ->
                  fn
                    ~poll
                    ~listener
                    ~listener_addr
                    ~client
                    ~server
                    ~peer))))

let wait_udp_readable = fun poll ~token socket ->
  wait_readable poll ~token (Kernel.Net.UdpSocket.to_source socket)

let recv_from_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv_from socket buffer with
    | Kernel.Result.Ok value -> Ok value
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_udp_readable poll ~token socket in
          loop ()
        else
          Error (string_of_error error)
  in
  loop ()

let recv_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv socket buffer with
    | Kernel.Result.Ok value -> Ok value
    | Kernel.Result.Error error ->
        if is_would_block error then
          let* () = wait_udp_readable poll ~token socket in
          loop ()
        else
          Error (string_of_error error)
  in
  loop ()

let test_ip_addr_validates_ipv4_and_ipv6 = fun _ctx ->
  match
    ( Kernel.Net.IpAddr.of_string "127.0.0.1",
      Kernel.Net.IpAddr.of_string "::1",
      Kernel.Net.IpAddr.of_string "nope" )
  with
  | ( Kernel.Result.Ok ipv4,
      Kernel.Result.Ok ipv6,
      Kernel.Result.Error Kernel.Error.Invalid_argument ) ->
      if Kernel.String.equal (Kernel.Net.IpAddr.to_string ipv4) "127.0.0.1"
         && Kernel.String.equal (Kernel.Net.IpAddr.to_string ipv6) "::1"
      then
        Ok ()
      else
        Error "expected ip parsing to preserve canonical loopback forms"
  | _ ->
      Error "expected ip parser to accept ipv4/ipv6 loopback and reject invalid input"

let test_tcp_listener_and_stream_roundtrip = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr ~client ~server ~peer ->
      let* client_local =
        lift (Kernel.Net.TcpStream.local_addr client)
      in
      let* client_peer =
        lift (Kernel.Net.TcpStream.peer_addr client)
      in
      let listener_port = Kernel.Net.SocketAddr.port listener_addr in
      let client_local_port = Kernel.Net.SocketAddr.port client_local in
      let client_peer_port = Kernel.Net.SocketAddr.port client_peer in
      let accepted_peer_port = Kernel.Net.SocketAddr.port peer in
      if client_peer_port != listener_port || accepted_peer_port != client_local_port then
        Error "expected tcp peer addresses to line up across connect and accept"
      else
        let ping = Kernel.Bytes.of_string "ping" in
        let pong = Kernel.IO.Iovec.of_string_array [|"po"; "ng"|] in
        let* () =
          write_all_stream
            poll
            ~token:(Kernel.Async.Token.make 303)
            client
            ping
            ~pos:0
            ~len:(Kernel.Bytes.length ping)
        in
        let server_buf = Kernel.Bytes.create 4 in
        let* () =
          read_exact_stream
            poll
            ~token:(Kernel.Async.Token.make 304)
            server
            server_buf
            ~pos:0
            ~len:4
        in
        let* () =
          write_all_vectored
            poll
            ~token:(Kernel.Async.Token.make 305)
            server
            pong
            ~pos:0
            ~len:(Kernel.IO.Iovec.length pong)
        in
        let client_buf = Kernel.IO.Iovec.create ~count:2 ~size:4 () in
        let* () =
          read_exact_vectored
            poll
            ~token:(Kernel.Async.Token.make 306)
            client
            client_buf
            ~pos:0
            ~len:4
        in
        if Kernel.String.equal (Kernel.Bytes.sub_string server_buf 0 4) "ping"
           && Kernel.String.equal (Kernel.IO.Iovec.into_string client_buf) "pong"
        then
          Ok ()
        else
          Error "expected tcp loopback roundtrip to preserve scalar and vectored payloads")

let test_udp_socket_send_to_and_recv_from = fun _ctx ->
  with_poll
    (fun poll ->
      let* server =
        lift (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* server_addr =
                lift (Kernel.Net.UdpSocket.local_addr server)
              in
              let* client_addr =
                lift (Kernel.Net.UdpSocket.local_addr client)
              in
              let* sent =
                lift
                  (Kernel.Net.UdpSocket.send_to
                     client
                     server_addr
                     (Kernel.Bytes.of_string "ping"))
              in
              if sent != 4 then
                Error "expected udp send_to to send one datagram"
              else
                let server_buf = Kernel.Bytes.create 32 in
                let* (read, from) =
                  recv_from_udp poll ~token:(Kernel.Async.Token.make 307) server server_buf
                in
                if read != 4
                   || not (Kernel.String.equal (Kernel.Bytes.sub_string server_buf 0 read) "ping")
                   || Kernel.Net.SocketAddr.port from != Kernel.Net.SocketAddr.port client_addr
                then
                  Error "expected udp recv_from to preserve sender and payload"
                else
                  let* () =
                    lift (Kernel.Net.UdpSocket.connect server client_addr)
                  in
                  let* () =
                    lift (Kernel.Net.UdpSocket.connect client server_addr)
                  in
                  let* _ =
                    lift (Kernel.Net.UdpSocket.send server (Kernel.Bytes.of_string "pong"))
                  in
                  let client_buf = Kernel.Bytes.create 32 in
                  let* reply =
                    recv_udp poll ~token:(Kernel.Async.Token.make 308) client client_buf
                  in
                  if reply = 4
                     && Kernel.String.equal (Kernel.Bytes.sub_string client_buf 0 reply) "pong"
                  then
                    Ok ()
                  else
                    Error "expected connected udp send and recv to preserve payload")))

let tests = [
  Test.case "Net.IpAddr validates ipv4 and ipv6" test_ip_addr_validates_ipv4_and_ipv6;
  Test.case "Net.TcpListener and TcpStream roundtrip over loopback" test_tcp_listener_and_stream_roundtrip;
  Test.case "Net.UdpSocket send_to and recv_from roundtrip over loopback" test_udp_socket_send_to_and_recv_from;
]

let main = fun ~args ->
  Test.Cli.main ~name:"kernel_new_net_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
