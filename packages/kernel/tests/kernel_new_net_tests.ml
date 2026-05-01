open Std

module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let string_of_async_error = fun error -> Kernel.Error.to_string (Kernel.Error.from_async error)

let string_of_tcp_listener_error = fun error ->
  Kernel.Error.to_string
    (Kernel.Error.from_net_tcp_listener error)

let string_of_tcp_stream_error = fun error ->
  Kernel.Error.to_string
    (Kernel.Error.from_net_tcp_stream error)

let string_of_udp_error = fun error ->
  Kernel.Error.to_string
    (Kernel.Error.from_net_udp_socket error)

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_async_error error)

let lift_tcp_listener result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error)

let lift_tcp_stream result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)

let lift_udp result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_udp_error error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let is_tcp_listener_would_block error =
  match error with
  | Kernel.Net.TcpListener.WouldBlock -> true
  | Kernel.Net.TcpListener.System system_error -> Kernel.SystemError.would_block system_error
  | _ -> false

let is_tcp_stream_would_block error =
  match error with
  | Kernel.Net.TcpStream.WouldBlock -> true
  | Kernel.Net.TcpStream.System system_error -> Kernel.SystemError.would_block system_error
  | _ -> false

let is_udp_would_block error =
  match error with
  | Kernel.Net.UdpSocket.WouldBlock -> true
  | Kernel.Net.UdpSocket.System system_error -> Kernel.SystemError.would_block system_error
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

let rec close_streams streams =
  match streams with
  | [] -> ()
  | stream :: rest ->
      close_stream stream;
      close_streams rest

let with_poll = fun fn ->
  let* poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let* () = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let* events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.any
          events
          ~fn:(fun event ->
            Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
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

let has_readable_token = fun token events ->
  List.any
    events
    ~fn:(fun event ->
      Kernel.Async.Event.is_readable event
      && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))

let rec write_all_stream = fun poll ~token stream buffer ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    match Kernel.Net.TcpStream.write stream ~pos ~len buffer with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Error "expected tcp write to make progress"
        else
          write_all_stream poll ~token stream buffer ~pos:(pos + written) ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          let* () = wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) in
          write_all_stream poll ~token stream buffer ~pos ~len
        else
          Error (string_of_tcp_stream_error error)

let rec write_all_vectored = fun poll ~token stream iov ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    let slice =
      Kernel.IO.IoVec.sub ~pos ~len iov
      |> Result.unwrap
    in
    match Kernel.Net.TcpStream.write_vectored stream slice with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Error "expected tcp vectored write to make progress"
        else
          write_all_vectored poll ~token stream iov ~pos:(pos + written) ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          let* () = wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) in
          write_all_vectored poll ~token stream iov ~pos ~len
        else
          Error (string_of_tcp_stream_error error)

let rec read_exact_stream = fun poll ~token stream buffer ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    match Kernel.Net.TcpStream.read stream ~pos ~len buffer with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Error "expected tcp read to make progress"
        else
          read_exact_stream poll ~token stream buffer ~pos:(pos + read) ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          let* () = wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream) in
          read_exact_stream poll ~token stream buffer ~pos ~len
        else
          Error (string_of_tcp_stream_error error)

let rec read_exact_vectored = fun poll ~token stream iov ~pos ~len ->
  if len = 0 then
    Ok ()
  else
    let slice =
      Kernel.IO.IoVec.sub ~pos ~len iov
      |> Result.unwrap
    in
    match Kernel.Net.TcpStream.read_vectored stream slice with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Error "expected tcp vectored read to make progress"
        else
          read_exact_vectored poll ~token stream iov ~pos:(pos + read) ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          let* () = wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream) in
          read_exact_vectored poll ~token stream iov ~pos ~len
        else
          Error (string_of_tcp_stream_error error)

let rec accept_stream = fun poll listener ->
  match Kernel.Net.TcpListener.accept listener with
  | Kernel.Result.Ok accepted -> Ok accepted
  | Kernel.Result.Error error ->
      if is_tcp_listener_would_block error then
        let* () =
          wait_readable
            poll
            ~token:(Kernel.Async.Token.make 301)
            (Kernel.Net.TcpListener.to_source listener)
        in
        accept_stream poll listener
      else
        Error (string_of_tcp_listener_error error)

let connect_stream = fun poll addr ->
  let* connect_result = lift_tcp_stream (Kernel.Net.TcpStream.connect addr) in
  match connect_result with
  | Kernel.Net.TcpStream.Connected stream -> Ok stream
  | Kernel.Net.TcpStream.InProgress stream ->
      let token = Kernel.Async.Token.make 302 in
      let rec finish attempts =
        if attempts = 0 then
          Error "expected nonblocking tcp connect to eventually complete"
        else
          let* () = wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) in
          match Kernel.Net.TcpStream.finish_connect stream with
          | Kernel.Result.Ok () -> Ok stream
          | Kernel.Result.Error error ->
              if is_tcp_stream_would_block error then
                finish (attempts - 1)
              else
                Error (string_of_tcp_stream_error error)
      in
      finish 8

let with_tcp_pair_at = fun listener_addr fn ->
  with_poll
    (fun poll ->
      let* listener = lift_tcp_listener (Kernel.Net.TcpListener.bind listener_addr) in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let* bound_addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let* client = connect_stream poll bound_addr in
          protect
            ~finally:(fun () -> close_stream client)
            (fun () ->
              let* (server, peer) = accept_stream poll listener in
              protect
                ~finally:(fun () -> close_stream server)
                (fun () ->
                  fn ~poll ~listener ~listener_addr:bound_addr ~client ~server ~peer))))

let with_tcp_pair = fun fn -> with_tcp_pair_at (Kernel.Net.SocketAddr.loopback_v4 ~port:0) fn

let wait_udp_readable = fun poll ~token socket ->
  wait_readable
    poll
    ~token
    (Kernel.Net.UdpSocket.to_source socket)

let recv_from_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv_from socket buffer with
    | Kernel.Result.Ok value -> Ok value
    | Kernel.Result.Error error ->
        if is_udp_would_block error then
          let* () = wait_udp_readable poll ~token socket in
          loop ()
        else
          Error (string_of_udp_error error)
  in
  loop ()

let recv_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv socket buffer with
    | Kernel.Result.Ok value -> Ok value
    | Kernel.Result.Error error ->
        if is_udp_would_block error then
          let* () = wait_udp_readable poll ~token socket in
          loop ()
        else
          Error (string_of_udp_error error)
  in
  loop ()

let with_udp_pair_at = fun bind_addr fn ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind bind_addr) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client = lift_udp (Kernel.Net.UdpSocket.bind bind_addr) in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let* client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
              fn ~poll ~server ~server_addr ~client ~client_addr)))

let with_udp_pair = fun fn -> with_udp_pair_at (Kernel.Net.SocketAddr.loopback_v4 ~port:0) fn

let with_connected_udp_pair = fun fn ->
  with_udp_pair
    (fun ~poll ~server ~server_addr ~client ~client_addr ->
      let* () = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
      let* () = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
      fn ~poll ~server ~server_addr ~client ~client_addr)

let test_ip_addr_validates_ipv4_and_ipv6 = fun _ctx ->
  match (
    Kernel.Net.IpAddr.from_string "127.0.0.1",
    Kernel.Net.IpAddr.from_string "::1",
    Kernel.Net.IpAddr.from_string "nope"
  ) with
  | (
      Kernel.Result.Ok ipv4,
      Kernel.Result.Ok ipv6,
      Kernel.Result.Error (Kernel.Net.IpAddr.InvalidText _)
    ) ->
      if
        Kernel.String.equal (Kernel.Net.IpAddr.to_string ipv4) "127.0.0.1"
        && Kernel.String.equal (Kernel.Net.IpAddr.to_string ipv6) "::1"
      then
        Ok ()
      else
        Error "expected ip parsing to preserve canonical loopback forms"
  | _ -> Error "expected ip parser to accept ipv4/ipv6 loopback and reject invalid input"

let test_tcp_listener_and_stream_roundtrip = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr ~client ~server ~peer ->
      let* client_local = lift_tcp_stream (Kernel.Net.TcpStream.local_addr client) in
      let* client_peer = lift_tcp_stream (Kernel.Net.TcpStream.peer_addr client) in
      let* server_local = lift_tcp_stream (Kernel.Net.TcpStream.local_addr server) in
      let* server_peer = lift_tcp_stream (Kernel.Net.TcpStream.peer_addr server) in
      let listener_port = Kernel.Net.SocketAddr.port listener_addr in
      let client_local_port = Kernel.Net.SocketAddr.port client_local in
      let client_peer_port = Kernel.Net.SocketAddr.port client_peer in
      let server_local_port = Kernel.Net.SocketAddr.port server_local in
      let server_peer_port = Kernel.Net.SocketAddr.port server_peer in
      let accepted_peer_port = Kernel.Net.SocketAddr.port peer in
      let listener_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip listener_addr) in
      let client_local_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip client_local) in
      let client_peer_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip client_peer) in
      let server_local_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip server_local) in
      let server_peer_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip server_peer) in
      if
        client_peer_port != listener_port
        || server_local_port != listener_port
        || accepted_peer_port != client_local_port
        || server_peer_port != client_local_port
        || client_peer_ip != listener_ip
        || server_local_ip != listener_ip
        || server_peer_ip != client_local_ip
      then
        Error "expected tcp peer addresses to line up across connect and accept"
      else
        let ping = Kernel.Bytes.from_string "ping" in
        let pong =
          Kernel.IO.IoVec.from_string_array [|"po"; "ng"|]
          |> Result.unwrap
        in
        let* () =
          write_all_stream
            poll
            ~token:(Kernel.Async.Token.make 303)
            client
            ping
            ~pos:0
            ~len:(Kernel.Bytes.length ping)
        in
        let server_buf = Kernel.Bytes.create ~size:4 in
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
            ~len:(Kernel.IO.IoVec.length pong)
        in
        let client_buf =
          Kernel.IO.IoVec.create ~count:2 ~size:4 ()
          |> Result.unwrap
        in
        let* () =
          read_exact_vectored
            poll
            ~token:(Kernel.Async.Token.make 306)
            client
            client_buf
            ~pos:0
            ~len:4
        in
        if
          Kernel.String.equal (Kernel.Bytes.sub_string server_buf ~offset:0 ~len:4) "ping"
          && Kernel.String.equal (Kernel.IO.IoVec.to_string client_buf) "pong"
        then
          Ok ()
        else
          Error "expected tcp loopback roundtrip to preserve scalar and vectored payloads")

let test_tcp_vectored_burst_roundtrip_preserves_order = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let parts = [|"a"; "bb"; "ccc"; "dddd"; "eeeee"|] in
      let outbound =
        Kernel.IO.IoVec.from_string_array parts
        |> Result.unwrap
      in
      let payload = Kernel.IO.IoVec.to_string outbound in
      let total = Kernel.IO.IoVec.length outbound in
      let inbound = Kernel.Bytes.create ~size:total in
      let rec write_many remaining =
        if remaining = 0 then
          Ok ()
        else
          let* () =
            write_all_vectored
              poll
              ~token:(Kernel.Async.Token.make ("tcp-vburst-write", remaining))
              client
              outbound
              ~pos:0
              ~len:total
          in
          write_many (remaining - 1)
      in
      let rec read_many remaining acc =
        if remaining = 0 then
          Ok (Kernel.String.concat "" (List.reverse acc))
        else
          let* () =
            read_exact_stream
              poll
              ~token:(Kernel.Async.Token.make ("tcp-vburst-read", remaining))
              server
              inbound
              ~pos:0
              ~len:total
          in
          read_many (remaining - 1) (Kernel.Bytes.sub_string inbound ~offset:0 ~len:total :: acc)
      in
      let* () = write_many 8 in
      let* actual = read_many 8 [] in
      if actual = Kernel.String.concat "" (List.init ~count:8 ~fn:(fun _ -> payload)) then
        Ok ()
      else
        Error "expected repeated small vectored tcp writes to preserve segment order")

let test_tcp_stream_reports_eof_after_peer_close = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close server) in
      let* () =
        wait_readable
          poll
          ~token:(Kernel.Async.Token.make 309)
          (Kernel.Net.TcpStream.to_source client)
      in
      let buffer = Kernel.Bytes.create ~size:8 in
      let* read = lift_tcp_stream (Kernel.Net.TcpStream.read client buffer) in
      if read = 0 then
        Ok ()
      else
        Error "expected tcp stream read to report eof after peer close")

let test_tcp_stream_shutdown_write_reports_eof_to_peer = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.Write) in
      let* () =
        wait_readable
          poll
          ~token:(Kernel.Async.Token.make 312)
          (Kernel.Net.TcpStream.to_source server)
      in
      let buffer = Kernel.Bytes.create ~size:8 in
      let* read = lift_tcp_stream (Kernel.Net.TcpStream.read server buffer) in
      if read = 0 then
        Ok ()
      else
        Error "expected tcp write shutdown to surface eof to the peer")

let test_tcp_stream_shutdown_write_rejects_further_writes = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.Write) in
      match Kernel.Net.TcpStream.write client (Kernel.Bytes.from_string "x") with
      | Kernel.Result.Error Kernel.Net.TcpStream.BrokenPipe -> Ok ()
      | Kernel.Result.Error Kernel.Net.TcpStream.NotConnected -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
      | Kernel.Result.Ok _ -> Error "expected write-shutdown tcp stream to reject further writes")

let test_tcp_stream_shutdown_read_preserves_write_half = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.Read) in
      let payload = Kernel.Bytes.from_string "ping" in
      let* () =
        write_all_stream
          poll
          ~token:(Kernel.Async.Token.make 314)
          client
          payload
          ~pos:0
          ~len:(Kernel.Bytes.length payload)
      in
      let buffer = Kernel.Bytes.create ~size:4 in
      let* () =
        read_exact_stream poll ~token:(Kernel.Async.Token.make 315) server buffer ~pos:0 ~len:4
      in
      if Kernel.String.equal (Kernel.Bytes.sub_string buffer ~offset:0 ~len:4) "ping" then
        Ok ()
      else
        Error "expected read-shutdown tcp stream to preserve the write half")

let test_tcp_stream_shutdown_read_write_disables_both_halves = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () =
        lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.ReadWrite)
      in
      let* () =
        wait_readable
          poll
          ~token:(Kernel.Async.Token.make 318)
          (Kernel.Net.TcpStream.to_source server)
      in
      let buffer = Kernel.Bytes.create ~size:8 in
      let* read = lift_tcp_stream (Kernel.Net.TcpStream.read server buffer) in
      if read != 0 then
        Error "expected read-write shutdown to surface eof to the peer"
      else
        match Kernel.Net.TcpStream.write client (Kernel.Bytes.from_string "x") with
        | Kernel.Result.Error Kernel.Net.TcpStream.BrokenPipe -> Ok ()
        | Kernel.Result.Error Kernel.Net.TcpStream.NotConnected -> Ok ()
        | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
        | Kernel.Result.Ok _ -> Error "expected read-write shutdown to reject further writes")

let test_tcp_stream_peer_write_shutdown_preserves_local_write_half = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown server Kernel.Net.TcpStream.Write) in
      let* () =
        wait_readable
          poll
          ~token:(Kernel.Async.Token.make 319)
          (Kernel.Net.TcpStream.to_source client)
      in
      let eof_buffer = Kernel.Bytes.create ~size:8 in
      let* eof_read = lift_tcp_stream (Kernel.Net.TcpStream.read client eof_buffer) in
      if eof_read != 0 then
        Error "expected peer write shutdown to surface eof to the local reader"
      else
        let payload = Kernel.Bytes.from_string "pong" in
        let* () =
          write_all_stream
            poll
            ~token:(Kernel.Async.Token.make 320)
            client
            payload
            ~pos:0
            ~len:(Kernel.Bytes.length payload)
        in
        let buffer = Kernel.Bytes.create ~size:4 in
        let* () =
          read_exact_stream poll ~token:(Kernel.Async.Token.make 321) server buffer ~pos:0 ~len:4
        in
        if Kernel.String.equal (Kernel.Bytes.sub_string buffer ~offset:0 ~len:4) "pong" then
          Ok ()
        else
          Error "expected peer write shutdown to preserve the local write half")

let test_tcp_stream_finish_connect_is_idempotent_after_success = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.finish_connect client) in
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.finish_connect client) in
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.finish_connect server) in
      Ok ())

let test_tcp_listener_ipv6_local_addr_roundtrips = fun _ctx ->
  let* listener =
    lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0))
  in
  protect
    ~finally:(fun () -> close_listener listener)
    (fun () ->
      let* addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
      if
        Kernel.Net.SocketAddr.port addr > 0
        && Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip addr) = "::1"
      then
        Ok ()
      else
        Error "expected ipv6 tcp listener to preserve the loopback address")

let test_tcp_listener_and_stream_ipv6_roundtrip = fun _ctx ->
  with_tcp_pair_at
    (Kernel.Net.SocketAddr.loopback_v6 ~port:0)
    (fun ~poll ~listener:_ ~listener_addr ~client ~server ~peer ->
      let listener_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip listener_addr) in
      let accepted_peer_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip peer) in
      let* client_peer = lift_tcp_stream (Kernel.Net.TcpStream.peer_addr client) in
      let client_peer_ip = Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip client_peer) in
      if
        not (Kernel.String.equal listener_ip "::1")
        || not (Kernel.String.equal accepted_peer_ip "::1")
        || not (Kernel.String.equal client_peer_ip "::1")
      then
        Error "expected ipv6 tcp loopback peers to preserve the loopback address"
      else
        let payload = Kernel.Bytes.from_string "ipv6" in
        let* () =
          write_all_stream
            poll
            ~token:(Kernel.Async.Token.make 316)
            client
            payload
            ~pos:0
            ~len:(Kernel.Bytes.length payload)
        in
        let buffer = Kernel.Bytes.create ~size:4 in
        let* () =
          read_exact_stream poll ~token:(Kernel.Async.Token.make 317) server buffer ~pos:0 ~len:4
        in
        if Kernel.String.equal (Kernel.Bytes.sub_string buffer ~offset:0 ~len:4) "ipv6" then
          Ok ()
        else
          Error "expected ipv6 tcp loopback to preserve payload")

let test_tcp_listener_bind_rejects_in_use_address = fun _ctx ->
  let* listener =
    lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
  in
  protect
    ~finally:(fun () -> close_listener listener)
    (fun () ->
      let* addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
      match Kernel.Net.TcpListener.bind addr with
      | Kernel.Result.Ok extra ->
          let _ = Kernel.Net.TcpListener.close extra in
          Error "expected second tcp listener bind to fail on the same address"
      | Kernel.Result.Error Kernel.Net.TcpListener.AddressInUse -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error))

let test_tcp_listener_accept_reports_would_block = fun _ctx ->
  let* listener =
    lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
  in
  protect
    ~finally:(fun () -> close_listener listener)
    (fun () ->
      match Kernel.Net.TcpListener.accept listener with
      | Kernel.Result.Error Kernel.Net.TcpListener.WouldBlock -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error)
      | Kernel.Result.Ok (stream, _) ->
          close_stream stream;
          Error "expected nonblocking tcp listener accept to report would_block without a pending peer")

let test_tcp_listener_accept_after_close_reports_bad_file_descriptor = fun _ctx ->
  let* listener =
    lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
  in
  let* () = lift_tcp_listener (Kernel.Net.TcpListener.close listener) in
  match Kernel.Net.TcpListener.accept listener with
  | Kernel.Result.Error (
    Kernel.Net.TcpListener.System Kernel.SystemError.BadFileDescriptor
  ) ->
      Ok ()
  | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error)
  | Kernel.Result.Ok (stream, _) ->
      close_stream stream;
      Error "expected closed tcp listener accept to fail with bad file descriptor"

let test_tcp_listener_source_deregister_after_close_is_harmless = fun _ctx ->
  with_poll
    (fun poll ->
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let source = Kernel.Net.TcpListener.to_source listener in
          let* () =
            lift_async
              (Kernel.Async.Poll.register
                poll
                (Kernel.Async.Token.make "listener-close-then-deregister")
                Kernel.Async.Interest.readable
                source)
          in
          let* () = lift_tcp_listener (Kernel.Net.TcpListener.close listener) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          Ok ()))

let test_tcp_listener_accepts_many_clients_in_one_burst = fun _ctx ->
  with_poll
    (fun poll ->
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let* addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let rec connect_many remaining acc =
            if remaining = 0 then
              Ok (List.reverse acc)
            else
              let* client = connect_stream poll addr in
              connect_many (remaining - 1) (client :: acc)
          in
          let* clients = connect_many 16 [] in
          protect
            ~finally:(fun () -> close_streams clients)
            (fun () ->
              let rec accept_many remaining acc =
                if remaining = 0 then
                  Ok acc
                else
                  let* (server, _) = accept_stream poll listener in
                  accept_many (remaining - 1) (server :: acc)
              in
              let* servers = accept_many 16 [] in
              protect
                ~finally:(fun () -> close_streams servers)
                (fun () ->
                  if List.length servers = 16 then
                    Ok ()
                  else
                    Error "expected tcp listener to accept every queued client in the burst"))))

let test_tcp_stream_finish_connect_reports_connection_refused = fun _ctx ->
  with_poll
    (fun poll ->
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      let* addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
      let* () = lift_tcp_listener (Kernel.Net.TcpListener.close listener) in
      let* connect_result = lift_tcp_stream (Kernel.Net.TcpStream.connect addr) in
      match connect_result with
      | Kernel.Net.TcpStream.Connected stream ->
          close_stream stream;
          Error "expected tcp connect to a closed port to fail"
      | Kernel.Net.TcpStream.InProgress stream ->
          protect
            ~finally:(fun () -> close_stream stream)
            (fun () ->
              let token = Kernel.Async.Token.make 313 in
              let rec loop attempts =
                if attempts = 0 then
                  Error "expected nonblocking tcp connect to report a refused connection"
                else
                  let* () = wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) in
                  match Kernel.Net.TcpStream.finish_connect stream with
                  | Kernel.Result.Ok () -> Error "expected refused connect to fail after readiness"
                  | Kernel.Result.Error Kernel.Net.TcpStream.ConnectionRefused -> Ok ()
                  | Kernel.Result.Error error ->
                      if is_tcp_stream_would_block error then
                        loop (attempts - 1)
                      else
                        Error (string_of_tcp_stream_error error)
              in
              loop 8))

let test_tcp_stream_source_deregister_after_close_is_harmless = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client:_ ~server ~peer:_ ->
      let source = Kernel.Net.TcpStream.to_source server in
      let* () =
        lift_async
          (Kernel.Async.Poll.register
            poll
            (Kernel.Async.Token.make "tcp-close-then-deregister")
            Kernel.Async.Interest.readable
            source)
      in
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close server) in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
      Ok ())

let test_tcp_stream_source_deregister_before_close_is_harmless = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client:_ ~server ~peer:_ ->
      let source = Kernel.Net.TcpStream.to_source server in
      let* () =
        lift_async
          (Kernel.Async.Poll.register
            poll
            (Kernel.Async.Token.make "tcp-deregister-then-close")
            Kernel.Async.Interest.readable
            source)
      in
      let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close server) in
      let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
      Ok ())

let test_async_poll_handles_many_tcp_streams = fun _ctx ->
  with_poll
    (fun poll ->
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let* listener_addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let rec connect_many remaining acc =
            if remaining = 0 then
              Ok acc
            else
              let* stream = connect_stream poll listener_addr in
              connect_many (remaining - 1) (stream :: acc)
          in
          let rec accept_many remaining acc =
            if remaining = 0 then
              Ok acc
            else
              let* (stream, _) = accept_stream poll listener in
              accept_many (remaining - 1) (stream :: acc)
          in
          let rec close_streams = fun __tmp1 ->
            match __tmp1 with
            | [] -> ()
            | stream :: rest ->
                close_stream stream;
                close_streams rest
          in
          let* clients = connect_many 12 [] in
          protect
            ~finally:(fun () -> close_streams clients)
            (fun () ->
              let* servers = accept_many 12 [] in
              protect
                ~finally:(fun () -> close_streams servers)
                (fun () ->
                  let clients = List.reverse clients in
                  let servers = List.reverse servers in
                  let rec register index = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> Ok ()
                    | stream :: rest ->
                        let* () =
                          lift_async
                            (Kernel.Async.Poll.register
                              poll
                              (Kernel.Async.Token.make index)
                              Kernel.Async.Interest.readable
                              (Kernel.Net.TcpStream.to_source stream))
                        in
                        register (index + 1) rest
                  in
                  let rec write_all_clients = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> Ok ()
                    | stream :: rest ->
                        let* written =
                          lift_tcp_stream
                            (Kernel.Net.TcpStream.write stream (Kernel.Bytes.from_string "x"))
                        in
                        if written != 1 then
                          Error "expected tcp write to make progress during many-stream readiness"
                        else
                          write_all_clients rest
                  in
                  let seen = Kernel.Array.make ~count:12 ~value:false in
                  let rec mark = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> ()
                    | event :: rest ->
                        if Kernel.Async.Event.is_readable event then
                          let token =
                            Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event)
                          in
                          if token >= 0 && token < 12 then
                            Kernel.Array.set seen ~at:token ~value:true;
                        mark rest
                  in
                  let rec all_seen index =
                    if index = 12 then
                      true
                    else if Kernel.Array.get_unchecked seen ~at:index then
                      all_seen (index + 1)
                    else
                      false
                  in
                  let* () = register 0 servers in
                  let* () = write_all_clients clients in
                  let rec poll_until attempts =
                    if attempts = 0 then
                      Error "expected many tcp streams to become readable after client writes"
                    else
                      let* events =
                        lift_async
                          (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:64 poll)
                      in
                      mark events;
                    if all_seen 0 then
                      Ok ()
                    else
                      poll_until (attempts - 1)
                  in
                  poll_until 12))))

let test_udp_socket_send_to_and_recv_from = fun _ctx ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let* client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
              let* sent =
                lift_udp
                  (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "ping"))
              in
              if sent != 4 then
                Error "expected udp send_to to send one datagram"
              else
                let server_buf = Kernel.Bytes.create ~size:32 in
                let* (read, from) =
                  recv_from_udp poll ~token:(Kernel.Async.Token.make 307) server server_buf
                in
                if
                  read != 4
                  || not
                    (Kernel.String.equal
                      (Kernel.Bytes.sub_string server_buf ~offset:0 ~len:read)
                      "ping")
                  || Kernel.Net.SocketAddr.port from != Kernel.Net.SocketAddr.port client_addr
                then
                  Error "expected udp recv_from to preserve sender and payload"
                else
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
                  let* _ =
                    lift_udp (Kernel.Net.UdpSocket.send server (Kernel.Bytes.from_string "pong"))
                  in
                  let client_buf = Kernel.Bytes.create ~size:32 in
                  let* reply = recv_udp poll ~token:(Kernel.Async.Token.make 308) client client_buf in
                  if
                    reply = 4
                    && Kernel.String.equal
                      (Kernel.Bytes.sub_string client_buf ~offset:0 ~len:reply)
                      "pong"
                  then
                    Ok ()
                  else
                    Error "expected connected udp send and recv to preserve payload")))

let test_udp_connected_socket_ignores_other_peers = fun _ctx ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* other =
                lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
              in
              protect
                ~finally:(fun () -> close_udp other)
                (fun () ->
                  let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                  let* client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
                  let token = Kernel.Async.Token.make 310 in
                  let source = Kernel.Net.UdpSocket.to_source server in
                  let* () =
                    lift_async
                      (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
                  in
                  protect
                    ~finally:(fun () ->
                      let _ = Kernel.Async.Poll.deregister poll source in
                      ())
                    (fun () ->
                      let* sent =
                        lift_udp
                          (Kernel.Net.UdpSocket.send_to
                            other
                            server_addr
                            (Kernel.Bytes.from_string "rogue"))
                      in
                      if sent != 5 then
                        Error "expected udp send_to to send the rogue datagram"
                      else
                        let* events = lift_async (Kernel.Async.Poll.poll ~timeout:1_000_000L poll) in
                        let server_ready =
                          List.any
                            events
                            ~fn:(fun event ->
                              Kernel.Async.Event.is_readable event
                              && Kernel.Async.Token.equal token (Kernel.Async.Event.token event))
                        in
                        let buffer = Kernel.Bytes.create ~size:32 in
                        match Kernel.Net.UdpSocket.recv server buffer with
                        | Kernel.Result.Error Kernel.Net.UdpSocket.WouldBlock when not server_ready ->
                            Ok ()
                        | Kernel.Result.Error error -> Error (string_of_udp_error error)
                        | Kernel.Result.Ok _ ->
                            Error "expected connected udp socket to ignore datagrams from other peers")))))

let test_udp_connected_socket_delivers_connected_peer_after_filtering_foreign_datagrams = fun
  _ctx ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* other =
                lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
              in
              protect
                ~finally:(fun () -> close_udp other)
                (fun () ->
                  let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                  let* client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
                  let source = Kernel.Net.UdpSocket.to_source server in
                  let token = Kernel.Async.Token.make "udp-connected-filtered-peer" in
                  let* () =
                    lift_async
                      (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
                  in
                  protect
                    ~finally:(fun () ->
                      let _ = Kernel.Async.Poll.deregister poll source in
                      ())
                    (fun () ->
                      let* sent_a =
                        lift_udp
                          (Kernel.Net.UdpSocket.send_to
                            other
                            server_addr
                            (Kernel.Bytes.from_string "rogue-a"))
                      in
                      let* sent_b =
                        lift_udp
                          (Kernel.Net.UdpSocket.send_to
                            other
                            server_addr
                            (Kernel.Bytes.from_string "rogue-b"))
                      in
                      if sent_a != 7 || sent_b != 7 then
                        Error "expected udp send_to to preserve both foreign datagrams"
                      else
                        let* quiet =
                          lift_async
                            (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll)
                        in
                        if has_readable_token token quiet then
                          Error "expected connected udp socket to stay unreadable for foreign datagrams"
                        else
                          let buffer = Kernel.Bytes.create ~size:32 in
                          match Kernel.Net.UdpSocket.recv server buffer with
                          | Kernel.Result.Error Kernel.Net.UdpSocket.WouldBlock ->
                              let* sent =
                                lift_udp
                                  (Kernel.Net.UdpSocket.send
                                    client
                                    (Kernel.Bytes.from_string "peer"))
                              in
                              if sent != 4 then
                                Error "expected connected udp send to write one datagram"
                              else
                                let* events =
                                  lift_async
                                    (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll)
                                in
                                if not (has_readable_token token events) then
                                  Error "expected connected udp peer traffic to become readable"
                                else
                                  let* (read, from) =
                                    lift_udp (Kernel.Net.UdpSocket.recv_from server buffer)
                                  in
                                  if
                                    read = 4
                                    && Kernel.String.equal
                                      (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read)
                                      "peer"
                                    && Kernel.Net.SocketAddr.port from
                                    = Kernel.Net.SocketAddr.port client_addr
                                  then
                                    Ok ()
                                  else
                                    Error "expected connected udp recv_from to preserve the accepted peer"
                          | Kernel.Result.Error error -> Error (string_of_udp_error error)
                          | Kernel.Result.Ok _ ->
                              Error "expected connected udp socket to ignore foreign datagrams before peer traffic")))))

let test_async_poll_handles_many_udp_sockets = fun _ctx ->
  with_poll
    (fun poll ->
      let rec bind_many remaining acc =
        if remaining = 0 then
          Ok acc
        else
          let* server =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          bind_many (remaining - 1) ((server, client) :: acc)
      in
      let rec close_many = fun __tmp1 ->
        match __tmp1 with
        | [] -> ()
        | (server, client) :: rest ->
            let _ = Kernel.Net.UdpSocket.close server in
            let _ = Kernel.Net.UdpSocket.close client in
            close_many rest
      in
      let* pairs = bind_many 16 [] in
      protect
        ~finally:(fun () -> close_many pairs)
        (fun () ->
          let pairs = List.reverse pairs in
          let rec register index = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok ()
            | (server, _) :: rest ->
                let* () =
                  lift_async
                    (Kernel.Async.Poll.register
                      poll
                      (Kernel.Async.Token.make index)
                      Kernel.Async.Interest.readable
                      (Kernel.Net.UdpSocket.to_source server))
                in
                register (index + 1) rest
          in
          let rec send_all = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok ()
            | (server, client) :: rest ->
                let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                let* sent =
                  lift_udp
                    (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "x"))
                in
                if sent != 1 then
                  Error "expected udp send_to to write one byte"
                else
                  send_all rest
          in
          let* () = register 0 pairs in
          let* () = send_all pairs in
          let seen = Kernel.Array.make ~count:16 ~value:false in
          let rec mark = fun __tmp1 ->
            match __tmp1 with
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 16 then
                    Kernel.Array.set seen ~at:token ~value:true;
                mark rest
          in
          let rec all_seen index =
            if index = 16 then
              true
            else if Kernel.Array.get_unchecked seen ~at:index then
              all_seen (index + 1)
            else
              false
          in
          let rec poll_until attempts =
            if attempts = 0 then
              Error "expected many udp sockets to become readable after repeated polls"
            else
              let* events =
                lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:64 poll)
              in
              mark events;
            if all_seen 0 then
              Ok ()
            else
              poll_until (attempts - 1)
          in
          poll_until 16))

let test_async_poll_tolerates_closed_registered_udp_sockets = fun _ctx ->
  with_poll
    (fun poll ->
      let rec bind_many remaining acc =
        if remaining = 0 then
          Ok acc
        else
          let* server =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
          in
          bind_many (remaining - 1) ((server, client) :: acc)
      in
      let rec close_many = fun __tmp1 ->
        match __tmp1 with
        | [] -> ()
        | (server, client) :: rest ->
            let _ = Kernel.Net.UdpSocket.close server in
            let _ = Kernel.Net.UdpSocket.close client in
            close_many rest
      in
      let* pairs = bind_many 8 [] in
      protect
        ~finally:(fun () -> close_many pairs)
        (fun () ->
          let pairs = List.reverse pairs in
          let rec register index = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok ()
            | (server, _) :: rest ->
                let* () =
                  lift_async
                    (Kernel.Async.Poll.register
                      poll
                      (Kernel.Async.Token.make index)
                      Kernel.Async.Interest.readable
                      (Kernel.Net.UdpSocket.to_source server))
                in
                register (index + 1) rest
          in
          let rec close_even index = fun __tmp1 ->
            match __tmp1 with
            | [] -> ()
            | (server, _) :: rest ->
                if index land 1 = 0 then
                  close_udp server;
                close_even (index + 1) rest
          in
          let rec send_live index = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok ()
            | (server, client) :: rest ->
                if index land 1 = 0 then
                  send_live (index + 1) rest
                else
                  let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                  let* sent =
                    lift_udp
                      (Kernel.Net.UdpSocket.send_to
                        client
                        server_addr
                        (Kernel.Bytes.from_string "x"))
                  in
                  if sent != 1 then
                    Error "expected udp send_to to write one byte"
                  else
                    send_live (index + 1) rest
          in
          let seen = Kernel.Array.make ~count:8 ~value:false in
          let rec mark = fun __tmp1 ->
            match __tmp1 with
            | [] -> ()
            | event :: rest ->
                if Kernel.Async.Event.is_readable event then
                  let token = Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event) in
                  if token >= 0 && token < 8 then
                    Kernel.Array.set seen ~at:token ~value:true;
                mark rest
          in
          let rec live_seen index =
            if index = 8 then
              true
            else if index land 1 = 0 then
              live_seen (index + 1)
            else if Kernel.Array.get_unchecked seen ~at:index then
              live_seen (index + 1)
            else
              false
          in
          let* () = register 0 pairs in
          close_even 0 pairs;
          let* () = send_live 0 pairs in
          let rec poll_until attempts =
            if attempts = 0 then
              Error "expected closed registered udp sockets to not poison remaining readiness"
            else
              let* events =
                lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll)
              in
              mark events;
            if live_seen 0 then
              Ok ()
            else
              poll_until (attempts - 1)
          in
          poll_until 8))

let test_udp_socket_source_deregister_after_close_is_harmless = fun _ctx ->
  with_poll
    (fun poll ->
      let* socket = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp socket)
        (fun () ->
          let source = Kernel.Net.UdpSocket.to_source socket in
          let* () =
            lift_async
              (Kernel.Async.Poll.register
                poll
                (Kernel.Async.Token.make "udp-close-then-deregister")
                Kernel.Async.Interest.readable
                source)
          in
          let* () = lift_udp (Kernel.Net.UdpSocket.close socket) in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          Ok ()))

let test_udp_socket_source_deregister_before_close_is_harmless = fun _ctx ->
  with_poll
    (fun poll ->
      let* socket = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp socket)
        (fun () ->
          let source = Kernel.Net.UdpSocket.to_source socket in
          let* () =
            lift_async
              (Kernel.Async.Poll.register
                poll
                (Kernel.Async.Token.make "udp-deregister-then-close")
                Kernel.Async.Interest.readable
                source)
          in
          let* () = lift_async (Kernel.Async.Poll.deregister poll source) in
          let* () = lift_udp (Kernel.Net.UdpSocket.close socket) in
          let* _ = lift_async (Kernel.Async.Poll.poll ~timeout:0L poll) in
          Ok ()))

let test_async_poll_tolerates_closed_registered_tcp_streams = fun _ctx ->
  with_poll
    (fun poll ->
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      protect
        ~finally:(fun () -> close_listener listener)
        (fun () ->
          let* listener_addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let rec connect_many remaining acc =
            if remaining = 0 then
              Ok acc
            else
              let* stream = connect_stream poll listener_addr in
              connect_many (remaining - 1) (stream :: acc)
          in
          let rec accept_many remaining acc =
            if remaining = 0 then
              Ok acc
            else
              let* (stream, _) = accept_stream poll listener in
              accept_many (remaining - 1) (stream :: acc)
          in
          let* clients = connect_many 8 [] in
          protect
            ~finally:(fun () -> close_streams clients)
            (fun () ->
              let* servers = accept_many 8 [] in
              protect
                ~finally:(fun () -> close_streams servers)
                (fun () ->
                  let clients = List.reverse clients in
                  let servers = List.reverse servers in
                  let rec register index = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> Ok ()
                    | stream :: rest ->
                        let* () =
                          lift_async
                            (Kernel.Async.Poll.register
                              poll
                              (Kernel.Async.Token.make index)
                              Kernel.Async.Interest.readable
                              (Kernel.Net.TcpStream.to_source stream))
                        in
                        register (index + 1) rest
                  in
                  let rec close_even index = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> ()
                    | stream :: rest ->
                        if index land 1 = 0 then
                          close_stream stream;
                        close_even (index + 1) rest
                  in
                  let rec write_live index clients servers =
                    match (clients, servers) with
                    | ([], []) -> Ok ()
                    | (client :: client_rest, _server :: server_rest) ->
                        if index land 1 = 0 then
                          write_live (index + 1) client_rest server_rest
                        else
                          let* written =
                            lift_tcp_stream
                              (Kernel.Net.TcpStream.write client (Kernel.Bytes.from_string "x"))
                          in
                          if written != 1 then
                            Error "expected tcp write to make progress for live registered streams"
                          else
                            write_live (index + 1) client_rest server_rest
                    | _ -> Error "expected tcp client/server lists to stay aligned"
                  in
                  let seen = Kernel.Array.make ~count:8 ~value:false in
                  let rec mark = fun __tmp1 ->
                    match __tmp1 with
                    | [] -> ()
                    | event :: rest ->
                        if Kernel.Async.Event.is_readable event then
                          let token =
                            Kernel.Async.Token.unsafe_value (Kernel.Async.Event.token event)
                          in
                          if token >= 0 && token < 8 then
                            Kernel.Array.set seen ~at:token ~value:true;
                        mark rest
                  in
                  let rec live_seen index =
                    if index = 8 then
                      true
                    else if index land 1 = 0 then
                      live_seen (index + 1)
                    else if Kernel.Array.get_unchecked seen ~at:index then
                      live_seen (index + 1)
                    else
                      false
                  in
                  let* () = register 0 servers in
                  close_even 0 servers;
                  let* () = write_live 0 clients servers in
                  let rec poll_until attempts =
                    if attempts = 0 then
                      Error "expected closed registered tcp streams to not poison remaining readiness"
                    else
                      let* events =
                        lift_async
                          (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll)
                      in
                      mark events;
                    if live_seen 0 then
                      Ok ()
                    else
                      poll_until (attempts - 1)
                  in
                  poll_until 8))))

let test_udp_socket_ipv6_send_to_and_recv_from = fun _ctx ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let* sent =
                lift_udp
                  (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "ipv6"))
              in
              if sent != 4 then
                Error "expected ipv6 udp send_to to send one datagram"
              else
                let buffer = Kernel.Bytes.create ~size:32 in
                let* (read, from) =
                  recv_from_udp poll ~token:(Kernel.Async.Token.make 314) server buffer
                in
                if
                  read = 4
                  && Kernel.String.equal (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read) "ipv6"
                  && Kernel.String.equal
                    (Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip from))
                    "::1"
                then
                  Ok ()
                else
                  Error "expected ipv6 udp loopback to preserve payload and peer address")))

let test_udp_connected_ipv6_socket_ignores_other_peers = fun _ctx ->
  with_poll
    (fun poll ->
      let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp server)
        (fun () ->
          let* client =
            lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0))
          in
          protect
            ~finally:(fun () -> close_udp client)
            (fun () ->
              let* other =
                lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v6 ~port:0))
              in
              protect
                ~finally:(fun () -> close_udp other)
                (fun () ->
                  let* server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                  let* client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
                  let* () = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
                  let source = Kernel.Net.UdpSocket.to_source server in
                  let token = Kernel.Async.Token.make "udp-connected-ipv6-filter" in
                  let* () =
                    lift_async
                      (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
                  in
                  protect
                    ~finally:(fun () ->
                      let _ = Kernel.Async.Poll.deregister poll source in
                      ())
                    (fun () ->
                      let* sent =
                        lift_udp
                          (Kernel.Net.UdpSocket.send_to
                            other
                            server_addr
                            (Kernel.Bytes.from_string "rogue"))
                      in
                      if sent != 5 then
                        Error "expected ipv6 udp send_to to preserve the rogue datagram"
                      else
                        let* quiet =
                          lift_async
                            (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll)
                        in
                        if has_readable_token token quiet then
                          Error "expected connected ipv6 udp socket to ignore foreign datagrams"
                        else
                          let buffer = Kernel.Bytes.create ~size:32 in
                          match Kernel.Net.UdpSocket.recv_from server buffer with
                          | Kernel.Result.Error Kernel.Net.UdpSocket.WouldBlock ->
                              let* sent =
                                lift_udp
                                  (Kernel.Net.UdpSocket.send
                                    client
                                    (Kernel.Bytes.from_string "v6ok"))
                              in
                              if sent != 4 then
                                Error "expected connected ipv6 udp send to write one datagram"
                              else
                                let* events =
                                  lift_async
                                    (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:8 poll)
                                in
                                if not (has_readable_token token events) then
                                  Error "expected connected ipv6 udp peer traffic to become readable"
                                else
                                  let* (read, from) =
                                    lift_udp (Kernel.Net.UdpSocket.recv_from server buffer)
                                  in
                                  if
                                    read = 4
                                    && Kernel.String.equal
                                      (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read)
                                      "v6ok"
                                    && Kernel.String.equal
                                      (Kernel.Net.IpAddr.to_string (Kernel.Net.SocketAddr.ip from))
                                      "::1"
                                  then
                                    Ok ()
                                  else
                                    Error "expected connected ipv6 udp recv_from to preserve the loopback peer"
                          | Kernel.Result.Error error -> Error (string_of_udp_error error)
                          | Kernel.Result.Ok _ ->
                              Error "expected connected ipv6 udp socket to ignore foreign datagrams before peer traffic")))))

let test_tcp_listener_repeated_bind_and_close_stays_healthy = fun _ctx ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      let* listener =
        lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
      in
      let* addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
      let* () = lift_tcp_listener (Kernel.Net.TcpListener.close listener) in
      if Kernel.Net.SocketAddr.port addr > 0 then
        loop (remaining - 1)
      else
        Error "expected repeated tcp listener binds to yield an ephemeral port"
  in
  loop 64

let test_udp_socket_repeated_bind_and_close_stays_healthy = fun _ctx ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      let* socket = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      let* addr = lift_udp (Kernel.Net.UdpSocket.local_addr socket) in
      let* () = lift_udp (Kernel.Net.UdpSocket.close socket) in
      if Kernel.Net.SocketAddr.port addr > 0 then
        loop (remaining - 1)
      else
        Error "expected repeated udp binds to yield an ephemeral port"
  in
  loop 128

let test_udp_send_requires_connected_peer = fun _ctx ->
  let* socket = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
  protect
    ~finally:(fun () -> close_udp socket)
    (fun () ->
      match Kernel.Net.UdpSocket.send socket (Kernel.Bytes.from_string "ping") with
      | Kernel.Result.Error Kernel.Net.UdpSocket.DestinationAddressRequired -> Ok ()
      | Kernel.Result.Error Kernel.Net.UdpSocket.NotConnected -> Ok ()
      | Kernel.Result.Error error ->
          Error (Kernel.String.append
            "expected destination-address error, got "
            (string_of_udp_error error))
      | Kernel.Result.Ok _ -> Error "expected udp send to fail for an unconnected socket")

let test_udp_send_and_recv_after_close_report_bad_file_descriptor = fun _ctx ->
  let* server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
  protect
    ~finally:(fun () -> close_udp server)
    (fun () ->
      let* peer = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect
        ~finally:(fun () -> close_udp peer)
        (fun () ->
          let* peer_addr = lift_udp (Kernel.Net.UdpSocket.local_addr peer) in
          let* () = lift_udp (Kernel.Net.UdpSocket.connect server peer_addr) in
          let* () = lift_udp (Kernel.Net.UdpSocket.close server) in
          let buffer = Kernel.Bytes.create ~size:8 in
          match (
            Kernel.Net.UdpSocket.send server (Kernel.Bytes.from_string "x"),
            Kernel.Net.UdpSocket.recv server buffer
          ) with
          | (
              Kernel.Result.Error (
                Kernel.Net.UdpSocket.System Kernel.SystemError.BadFileDescriptor
              ),
              Kernel.Result.Error (
                Kernel.Net.UdpSocket.System Kernel.SystemError.BadFileDescriptor
              )
            ) -> Ok ()
          | (Kernel.Result.Error send_error, Kernel.Result.Error recv_error) ->
              Error (Kernel.String.concat
                " | "
                [
                  Kernel.String.concat
                    ""
                    [
                      "expected closed udp send to fail with bad file descriptor but got ";
                      string_of_udp_error send_error;
                    ];
                  Kernel.String.concat
                    ""
                    [
                      "expected closed udp recv to fail with bad file descriptor but got ";
                      string_of_udp_error recv_error;
                    ];
                ])
          | _ -> Error "expected closed udp socket to reject both send and recv"))

let test_udp_bind_rejects_in_use_address = fun _ctx ->
  let addr = Kernel.Net.SocketAddr.loopback_v4 ~port:0 in
  let* first = lift_udp (Kernel.Net.UdpSocket.bind addr) in
  protect
    ~finally:(fun () -> close_udp first)
    (fun () ->
      let* bound_addr = lift_udp (Kernel.Net.UdpSocket.local_addr first) in
      match Kernel.Net.UdpSocket.bind ~reuse_addr:false ~reuse_port:false bound_addr with
      | Kernel.Result.Error Kernel.Net.UdpSocket.AddressInUse -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_udp_error error)
      | Kernel.Result.Ok socket ->
          close_udp socket;
          Error "expected binding the same udp address twice to fail")

let test_tcp_listener_bind_rejects_invalid_backlog = fun _ctx ->
  match Kernel.Net.TcpListener.bind ~backlog:0 (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
  | Kernel.Result.Error (Kernel.Net.TcpListener.InvalidBacklog { backlog = 0 }) -> Ok ()
  | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error)
  | Kernel.Result.Ok listener ->
      close_listener listener;
      Error "expected invalid backlog to be rejected before binding"

let test_socket_addr_rejects_negative_ports = fun _ctx ->
  match Kernel.Net.IpAddr.from_string "127.0.0.1" with
  | Kernel.Result.Ok ip -> (
      match Kernel.Net.SocketAddr.make ~ip ~port:(-1) with
      | Kernel.Result.Error (Kernel.Net.SocketAddr.InvalidPort { port = (-1) }) -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Net.SocketAddr.error_to_string error)
      | Kernel.Result.Ok _ -> Error "expected SocketAddr.make to reject negative ports"
    )
  | Kernel.Result.Error error -> Error (Kernel.Net.IpAddr.error_to_string error)

let test_socket_addr_rejects_ports_past_65535 = fun _ctx ->
  match Kernel.Net.IpAddr.from_string "127.0.0.1" with
  | Kernel.Result.Ok ip -> (
      match Kernel.Net.SocketAddr.make ~ip ~port:65_536 with
      | Kernel.Result.Error (Kernel.Net.SocketAddr.InvalidPort { port = 65_536 }) -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Net.SocketAddr.error_to_string error)
      | Kernel.Result.Ok _ -> Error "expected SocketAddr.make to reject ports above 65535"
    )
  | Kernel.Result.Error error -> Error (Kernel.Net.IpAddr.error_to_string error)

let test_socket_addr_ipv6_to_string_is_bracketed = fun _ctx ->
  let rendered = Kernel.Net.SocketAddr.to_string (Kernel.Net.SocketAddr.loopback_v6 ~port:80) in
  if rendered = "[::1]:80" then
    Ok ()
  else
    Error "expected SocketAddr.to_string to render IPv6 addresses with brackets"

let test_tcp_listener_close_twice_reports_bad_file_descriptor = fun _ctx ->
  let* listener =
    lift_tcp_listener (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0))
  in
  let* () = lift_tcp_listener (Kernel.Net.TcpListener.close listener) in
  match Kernel.Net.TcpListener.close listener with
  | Kernel.Result.Error (
    Kernel.Net.TcpListener.System Kernel.SystemError.BadFileDescriptor
  ) ->
      Ok ()
  | Kernel.Result.Error error -> Error (string_of_tcp_listener_error error)
  | Kernel.Result.Ok () ->
      Error "expected closing the same tcp listener twice to report bad_file_descriptor"

let test_tcp_stream_read_len_zero_is_a_no_op = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let buffer = Kernel.Bytes.from_string "unchanged" in
      match Kernel.Net.TcpStream.read client ~pos:2 ~len:0 buffer with
      | Kernel.Result.Ok 0 when Kernel.Bytes.to_string buffer = "unchanged" -> Ok ()
      | Kernel.Result.Ok _ -> Error "expected TcpStream.read ~len:0 to leave the buffer unchanged"
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error))

let test_tcp_stream_write_len_zero_sends_nothing = fun _ctx ->
  with_tcp_pair
    (fun ~poll ~listener:_ ~listener_addr:_ ~client ~server ~peer:_ ->
      let source = Kernel.Net.TcpStream.to_source server in
      let token = Kernel.Async.Token.make "tcp-write-zero" in
      let* () =
        lift_async (Kernel.Async.Poll.register poll token Kernel.Async.Interest.readable source)
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          match Kernel.Net.TcpStream.write client ~pos:1 ~len:0 (Kernel.Bytes.from_string "riot") with
          | Kernel.Result.Ok 0 ->
              let* events =
                lift_async (Kernel.Async.Poll.poll ~timeout:20_000_000L ~max_events:8 poll)
              in
              if has_readable_token token events then
                Error "expected TcpStream.write ~len:0 to leave the peer unreadable"
              else
                Ok ()
          | Kernel.Result.Ok _ ->
              Error "expected TcpStream.write ~len:0 to report zero bytes written"
          | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)))

let test_tcp_stream_read_rejects_invalid_slices = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      match Kernel.Net.TcpStream.read client ~pos:(-1) (Kernel.Bytes.create ~size:4) with
      | Kernel.Result.Error (Kernel.Net.TcpStream.InvalidSlice { pos = (-1); _ }) -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
      | Kernel.Result.Ok _ -> Error "expected TcpStream.read to reject invalid slices")

let test_tcp_stream_write_rejects_invalid_slices = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      match Kernel.Net.TcpStream.write client ~pos:2 ~len:3 (Kernel.Bytes.create ~size:4) with
      | Kernel.Result.Error (
        Kernel.Net.TcpStream.InvalidSlice { pos = 2; len = 3; buffer_len = 4 }
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
      | Kernel.Result.Ok _ -> Error "expected TcpStream.write to reject invalid slices")

let test_tcp_stream_close_twice_reports_bad_file_descriptor = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close client) in
      match Kernel.Net.TcpStream.close client with
      | Kernel.Result.Error (
        Kernel.Net.TcpStream.System Kernel.SystemError.BadFileDescriptor
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
      | Kernel.Result.Ok () ->
          Error "expected closing the same tcp stream twice to report bad_file_descriptor")

let test_finish_connect_after_close_reports_bad_file_descriptor = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close client) in
      match Kernel.Net.TcpStream.finish_connect client with
      | Kernel.Result.Error (
        Kernel.Net.TcpStream.System Kernel.SystemError.BadFileDescriptor
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (string_of_tcp_stream_error error)
      | Kernel.Result.Ok () ->
          Error "expected finish_connect after close to report bad_file_descriptor")

let test_local_and_peer_addr_after_close_report_bad_file_descriptor = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.close client) in
      match (Kernel.Net.TcpStream.local_addr client, Kernel.Net.TcpStream.peer_addr client) with
      | (
          Kernel.Result.Error (
            Kernel.Net.TcpStream.System Kernel.SystemError.BadFileDescriptor
          ),
          Kernel.Result.Error (
            Kernel.Net.TcpStream.System Kernel.SystemError.BadFileDescriptor
          )
        ) -> Ok ()
      | (Kernel.Result.Error local_error, Kernel.Result.Error peer_error) ->
          Error (Kernel.String.concat
            " | "
            [
              Kernel.String.append
                "unexpected local_addr error: "
                (string_of_tcp_stream_error local_error);
              Kernel.String.append
                "unexpected peer_addr error: "
                (string_of_tcp_stream_error peer_error);
            ])
      | _ -> Error "expected local_addr and peer_addr after close to report bad_file_descriptor")

let test_shutdown_write_is_idempotent = fun _ctx ->
  with_tcp_pair
    (fun ~poll:_ ~listener:_ ~listener_addr:_ ~client ~server:_ ~peer:_ ->
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.Write) in
      let* () = lift_tcp_stream (Kernel.Net.TcpStream.shutdown client Kernel.Net.TcpStream.Write) in
      Ok ())

let test_udp_recv_on_unconnected_socket_accepts_any_peer = fun _ctx ->
  with_udp_pair
    (fun ~poll ~server ~server_addr ~client ~client_addr:_ ->
      let* sent =
        lift_udp (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "ping"))
      in
      if sent != 4 then
        Error "expected udp send_to to write a single datagram"
      else
        let buffer = Kernel.Bytes.create ~size:16 in
        let* read = recv_udp poll ~token:(Kernel.Async.Token.make 925) server buffer in
        if read = 4 && Kernel.Bytes.sub_string buffer ~offset:0 ~len:read = "ping" then
          Ok ()
        else
          Error "expected recv on an unconnected udp socket to accept peer traffic")

let test_udp_send_and_recv_reject_invalid_slices = fun _ctx ->
  with_connected_udp_pair
    (fun ~poll:_ ~server ~server_addr:_ ~client ~client_addr:_ ->
      match (
        Kernel.Net.UdpSocket.send client ~pos:2 ~len:3 (Kernel.Bytes.create ~size:4),
        Kernel.Net.UdpSocket.recv server ~pos:(-1) (Kernel.Bytes.create ~size:4)
      ) with
      | (
          Kernel.Result.Error (
            Kernel.Net.UdpSocket.InvalidSlice { pos = 2; len = 3; buffer_len = 4 }
          ),
          Kernel.Result.Error (
            Kernel.Net.UdpSocket.InvalidSlice { pos = (-1); _ }
          )
        ) -> Ok ()
      | (Kernel.Result.Error send_error, Kernel.Result.Error recv_error) ->
          Error (Kernel.String.concat
            " | "
            [
              Kernel.String.append
                "unexpected udp send invalid-slice error: "
                (string_of_udp_error send_error);
              Kernel.String.append
                "unexpected udp recv invalid-slice error: "
                (string_of_udp_error recv_error);
            ])
      | _ -> Error "expected udp send and recv to reject invalid slices before doing I/O")

let test_udp_connect_after_close_reports_bad_file_descriptor = fun _ctx ->
  with_udp_pair
    (fun ~poll:_ ~server ~server_addr:_ ~client:_ ~client_addr ->
      let* () = lift_udp (Kernel.Net.UdpSocket.close server) in
      match Kernel.Net.UdpSocket.connect server client_addr with
      | Kernel.Result.Error (
        Kernel.Net.UdpSocket.System Kernel.SystemError.BadFileDescriptor
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (string_of_udp_error error)
      | Kernel.Result.Ok () ->
          Error "expected UdpSocket.connect after close to report bad_file_descriptor")

let test_udp_zero_length_datagrams_have_a_stable_contract = fun _ctx ->
  with_connected_udp_pair
    (fun ~poll ~server ~server_addr:_ ~client ~client_addr:_ ->
      let* sent = lift_udp (Kernel.Net.UdpSocket.send client (Kernel.Bytes.from_string "")) in
      if sent != 0 then
        Error "expected sending a zero-length udp datagram to report zero bytes"
      else
        let buffer = Kernel.Bytes.create ~size:1 in
        let* read = recv_udp poll ~token:(Kernel.Async.Token.make 926) server buffer in
        if read = 0 then
          Ok ()
        else
          Error "expected the peer to observe an empty udp datagram")

let test_oversized_udp_datagrams_are_rejected_cleanly = fun _ctx ->
  with_udp_pair
    (fun ~poll:_ ~server:_ ~server_addr ~client ~client_addr:_ ->
      let payload = Kernel.Bytes.create ~size:70_000 in
      Kernel.Bytes.fill payload ~offset:0 ~len:(Kernel.Bytes.length payload) ~char:'x';
      match Kernel.Net.UdpSocket.send_to client server_addr payload with
      | Kernel.Result.Error Kernel.Net.UdpSocket.MessageTooLong -> Ok ()
      | Kernel.Result.Error error -> Error (string_of_udp_error error)
      | Kernel.Result.Ok _ ->
          Error "expected oversized udp datagrams to be rejected with message_too_long")

let tests = [
  Test.case "Net.SocketAddr rejects negative ports" test_socket_addr_rejects_negative_ports;
  Test.case "Net.SocketAddr rejects ports above 65535" test_socket_addr_rejects_ports_past_65535;
  Test.case "Net.SocketAddr renders IPv6 with brackets" test_socket_addr_ipv6_to_string_is_bracketed;
  Test.case "Net.IpAddr validates ipv4 and ipv6" test_ip_addr_validates_ipv4_and_ipv6;
  Test.case
    "Net.TcpListener and TcpStream roundtrip over loopback"
    test_tcp_listener_and_stream_roundtrip;
  Test.case
    "Net.TcpListener close on the same listener twice reports bad_file_descriptor"
    test_tcp_listener_close_twice_reports_bad_file_descriptor;
  Test.case
    "Net.TcpStream vectored burst roundtrip preserves order"
    test_tcp_vectored_burst_roundtrip_preserves_order;
  Test.case "Net.TcpStream read len=0 is a no-op" test_tcp_stream_read_len_zero_is_a_no_op;
  Test.case "Net.TcpStream write len=0 sends nothing" test_tcp_stream_write_len_zero_sends_nothing;
  Test.case "Net.TcpStream read rejects invalid slices" test_tcp_stream_read_rejects_invalid_slices;
  Test.case
    "Net.TcpStream write rejects invalid slices"
    test_tcp_stream_write_rejects_invalid_slices;
  Test.case
    "Net.TcpStream close on the same stream twice reports bad_file_descriptor"
    test_tcp_stream_close_twice_reports_bad_file_descriptor;
  Test.case
    "Net.TcpStream reports eof after peer close"
    test_tcp_stream_reports_eof_after_peer_close;
  Test.case
    "Net.TcpStream write shutdown reports eof to the peer"
    test_tcp_stream_shutdown_write_reports_eof_to_peer;
  Test.case
    "Net.TcpStream write shutdown rejects further writes"
    test_tcp_stream_shutdown_write_rejects_further_writes;
  Test.case "Net.TcpStream shutdown Write is idempotent" test_shutdown_write_is_idempotent;
  Test.case
    "Net.TcpStream read shutdown preserves the write half"
    test_tcp_stream_shutdown_read_preserves_write_half;
  Test.case
    "Net.TcpStream read-write shutdown disables both halves"
    test_tcp_stream_shutdown_read_write_disables_both_halves;
  Test.case
    "Net.TcpStream peer write shutdown preserves the local write half"
    test_tcp_stream_peer_write_shutdown_preserves_local_write_half;
  Test.case
    "Net.TcpListener ipv6 local_addr preserves loopback"
    test_tcp_listener_ipv6_local_addr_roundtrips;
  Test.case
    "Net.TcpListener and TcpStream roundtrip over ipv6 loopback"
    test_tcp_listener_and_stream_ipv6_roundtrip;
  Test.case
    "Net.TcpListener bind rejects invalid backlog"
    test_tcp_listener_bind_rejects_invalid_backlog;
  Test.case
    "Net.TcpListener bind rejects address already in use"
    test_tcp_listener_bind_rejects_in_use_address;
  Test.case
    "Net.TcpListener accept reports would_block without pending peers"
    test_tcp_listener_accept_reports_would_block;
  Test.case
    "Net.TcpListener accept after close reports bad file descriptor"
    test_tcp_listener_accept_after_close_reports_bad_file_descriptor;
  Test.case
    "Net.TcpListener source deregister after close is harmless"
    test_tcp_listener_source_deregister_after_close_is_harmless;
  Test.case
    "Net.TcpListener accepts many clients in one burst"
    test_tcp_listener_accepts_many_clients_in_one_burst;
  Test.case
    "Net.TcpStream finish_connect is idempotent after success"
    test_tcp_stream_finish_connect_is_idempotent_after_success;
  Test.case
    "Net.TcpStream finish_connect after close reports bad_file_descriptor"
    test_finish_connect_after_close_reports_bad_file_descriptor;
  Test.case
    "Net.TcpStream finish_connect reports connection refused"
    test_tcp_stream_finish_connect_reports_connection_refused;
  Test.case
    "Net.TcpStream local_addr and peer_addr after close report bad_file_descriptor"
    test_local_and_peer_addr_after_close_report_bad_file_descriptor;
  Test.case
    "Net.UdpSocket send_to and recv_from roundtrip over loopback"
    test_udp_socket_send_to_and_recv_from;
  Test.case
    "Net.UdpSocket recv on an unconnected socket accepts any peer"
    test_udp_recv_on_unconnected_socket_accepts_any_peer;
  Test.case
    "Net.UdpSocket ipv6 send_to and recv_from roundtrip over loopback"
    test_udp_socket_ipv6_send_to_and_recv_from;
  Test.case
    "Net.UdpSocket connected socket ignores other peers"
    test_udp_connected_socket_ignores_other_peers;
  Test.case
    "Net.UdpSocket connected socket delivers its peer after filtering foreign datagrams"
    test_udp_connected_socket_delivers_connected_peer_after_filtering_foreign_datagrams;
  Test.case
    "Net.UdpSocket connected ipv6 socket ignores other peers"
    test_udp_connected_ipv6_socket_ignores_other_peers;
  Test.case "Async poll handles many udp sockets" test_async_poll_handles_many_udp_sockets;
  Test.case "Async poll handles many tcp streams" test_async_poll_handles_many_tcp_streams;
  Test.case
    "Async poll tolerates closed registered udp sockets"
    test_async_poll_tolerates_closed_registered_udp_sockets;
  Test.case
    "Net.UdpSocket source deregister after close is harmless"
    test_udp_socket_source_deregister_after_close_is_harmless;
  Test.case
    "Net.UdpSocket source deregister before close is harmless"
    test_udp_socket_source_deregister_before_close_is_harmless;
  Test.case
    "Async poll tolerates closed registered tcp streams"
    test_async_poll_tolerates_closed_registered_tcp_streams;
  Test.case
    "Net.TcpStream source deregister after close is harmless"
    test_tcp_stream_source_deregister_after_close_is_harmless;
  Test.case
    "Net.TcpStream source deregister before close is harmless"
    test_tcp_stream_source_deregister_before_close_is_harmless;
  Test.case
    "Net.TcpListener repeated bind and close stays healthy"
    test_tcp_listener_repeated_bind_and_close_stays_healthy;
  Test.case
    "Net.UdpSocket repeated bind and close stays healthy"
    test_udp_socket_repeated_bind_and_close_stays_healthy;
  Test.case "Net.UdpSocket bind rejects address already in use" test_udp_bind_rejects_in_use_address;
  Test.case "Net.UdpSocket send requires a connected peer" test_udp_send_requires_connected_peer;
  Test.case
    "Net.UdpSocket send and recv reject invalid slices"
    test_udp_send_and_recv_reject_invalid_slices;
  Test.case
    "Net.UdpSocket connect after close reports bad_file_descriptor"
    test_udp_connect_after_close_reports_bad_file_descriptor;
  Test.case
    "Net.UdpSocket zero-length datagrams have a stable contract"
    test_udp_zero_length_datagrams_have_a_stable_contract;
  Test.case
    "Net.UdpSocket oversized datagrams are rejected cleanly"
    test_oversized_udp_datagrams_are_rejected_cleanly;
  Test.case
    "Net.UdpSocket send and recv after close report bad file descriptor"
    test_udp_send_and_recv_after_close_report_bad_file_descriptor;
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"kernel_new_net_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
