open Std
module Kernel = Kernel

let panic_async = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_async error))

let panic_tcp_listener = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_net_tcp_listener error))

let panic_tcp_stream = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_net_tcp_stream error))

let panic_udp = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.from_net_udp_socket error))

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let lift_async result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_async error

let lift_tcp_listener result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_tcp_listener error

let lift_tcp_stream result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_tcp_stream error

let lift_udp result =
  match result with
  | Kernel.Result.Ok value -> value
  | Kernel.Result.Error error -> panic_udp error

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

let wait_for = fun poll ~token ~interest ~source ~pred ->
  let _ = lift_async (Kernel.Async.Poll.register poll token interest source) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.deregister poll source in
      ())
    (fun () ->
      let events = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L poll) in
      let found =
        List.any
          events
          ~fn:(fun event -> Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
      in
      if not found then
        Kernel.SystemError.panic "expected readiness event")

let wait_readable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.readable ~source ~pred:Kernel.Async.Event.is_readable

let wait_writable = fun poll ~token source ->
  wait_for poll ~token ~interest:Kernel.Async.Interest.writable ~source ~pred:Kernel.Async.Event.is_writable

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

let rec close_udp_pairs pairs =
  match pairs with
  | [] -> ()
  | (server, client) :: rest ->
      close_udp server;
      close_udp client;
      close_udp_pairs rest

let with_poll = fun fn ->
  let poll = lift_async (Kernel.Async.Poll.make ()) in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Async.Poll.close poll in
      ())
    (fun () -> fn poll)

let connect_stream = fun poll addr ->
  match lift_tcp_stream (Kernel.Net.TcpStream.connect addr) with
  | Kernel.Net.TcpStream.Connected stream -> stream
  | Kernel.Net.TcpStream.InProgress stream ->
      let token = Kernel.Async.Token.make 401 in
      let rec finish attempts =
        if attempts = 0 then
          Kernel.SystemError.panic "expected nonblocking tcp connect to eventually complete"
        else (
          wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream);
          match Kernel.Net.TcpStream.finish_connect stream with
          | Kernel.Result.Ok () -> stream
          | Kernel.Result.Error error ->
              if is_tcp_stream_would_block error then
                finish (attempts - 1)
              else
                panic_tcp_stream error
        )
      in
      finish 8

let rec accept_stream = fun poll listener ->
  match Kernel.Net.TcpListener.accept listener with
  | Kernel.Result.Ok (stream, _peer) -> stream
  | Kernel.Result.Error error ->
      if is_tcp_listener_would_block error then
        (
          wait_readable
            poll
            ~token:(Kernel.Async.Token.make 402)
            (Kernel.Net.TcpListener.to_source listener);
          accept_stream poll listener
        )
      else
        panic_tcp_listener error

let rec write_all_stream = fun poll ~token stream buffer ~pos ~len ->
  if len != 0 then
    match Kernel.Net.TcpStream.write stream ~pos ~len buffer with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Kernel.SystemError.panic "expected tcp write to make progress";
        write_all_stream poll ~token stream buffer ~pos:(pos + written) ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          (
            wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream);
            write_all_stream poll ~token stream buffer ~pos ~len
          )
        else
          panic_tcp_stream error

let rec read_exact_stream = fun poll ~token stream buffer ~pos ~len ->
  if len != 0 then
    match Kernel.Net.TcpStream.read stream ~pos ~len buffer with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Kernel.SystemError.panic "expected tcp read to make progress";
        read_exact_stream poll ~token stream buffer ~pos:(pos + read) ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          (
            wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream);
            read_exact_stream poll ~token stream buffer ~pos ~len
          )
        else
          panic_tcp_stream error

let rec write_all_vectored = fun poll ~token stream iov ~pos ~len ->
  if len != 0 then
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov |> Result.unwrap in
    match Kernel.Net.TcpStream.write_vectored stream slice with
    | Kernel.Result.Ok written ->
        if written <= 0 then
          Kernel.SystemError.panic "expected tcp vectored write to make progress";
        write_all_vectored poll ~token stream iov ~pos:(pos + written) ~len:(len - written)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          (
            wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream);
            write_all_vectored poll ~token stream iov ~pos ~len
          )
        else
          panic_tcp_stream error

let rec read_exact_vectored = fun poll ~token stream iov ~pos ~len ->
  if len != 0 then
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov |> Result.unwrap in
    match Kernel.Net.TcpStream.read_vectored stream slice with
    | Kernel.Result.Ok read ->
        if read <= 0 then
          Kernel.SystemError.panic "expected tcp vectored read to make progress";
        read_exact_vectored poll ~token stream iov ~pos:(pos + read) ~len:(len - read)
    | Kernel.Result.Error error ->
        if is_tcp_stream_would_block error then
          (
            wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream);
            read_exact_vectored poll ~token stream iov ~pos ~len
          )
        else
          panic_tcp_stream error

let bench_tcp_loopback_roundtrip = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let client = connect_stream poll addr in
          protect ~finally:(fun () -> close_stream client)
            (fun () ->
              let server = accept_stream poll listener in
              protect ~finally:(fun () -> close_stream server)
                (fun () ->
                  let payload = Kernel.Bytes.from_string "ping" in
                  let reply = Kernel.Bytes.from_string "pong" in
                  let server_buf = Kernel.Bytes.create ~size:4 in
                  let client_buf = Kernel.Bytes.create ~size:4 in
                  write_all_stream
                    poll
                    ~token:(Kernel.Async.Token.make 403)
                    client
                    payload
                    ~pos:0
                    ~len:4;
                  read_exact_stream
                    poll
                    ~token:(Kernel.Async.Token.make 404)
                    server
                    server_buf
                    ~pos:0
                    ~len:4;
                  write_all_stream
                    poll
                    ~token:(Kernel.Async.Token.make 405)
                    server
                    reply
                    ~pos:0
                    ~len:4;
                  read_exact_stream
                    poll
                    ~token:(Kernel.Async.Token.make 406)
                    client
                    client_buf
                    ~pos:0
                    ~len:4))))

let bench_tcp_vectored_roundtrip = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let client = connect_stream poll addr in
          protect ~finally:(fun () -> close_stream client)
            (fun () ->
              let server = accept_stream poll listener in
              protect ~finally:(fun () -> close_stream server)
                (fun () ->
                  let payload = Kernel.IO.Iovec.from_string_array [|"pi"; "ng"|] |> Result.unwrap in
                  let reply = Kernel.IO.Iovec.from_string_array [|"po"; "ng"|] |> Result.unwrap in
                  let server_buf = Kernel.IO.Iovec.create ~count:2 ~size:4 () |> Result.unwrap in
                  let client_buf = Kernel.IO.Iovec.create ~count:2 ~size:4 () |> Result.unwrap in
                  write_all_vectored
                    poll
                    ~token:(Kernel.Async.Token.make 410)
                    client
                    payload
                    ~pos:0
                    ~len:(Kernel.IO.Iovec.length payload);
                  read_exact_vectored
                    poll
                    ~token:(Kernel.Async.Token.make 411)
                    server
                    server_buf
                    ~pos:0
                    ~len:(Kernel.IO.Iovec.length payload);
                  write_all_vectored
                    poll
                    ~token:(Kernel.Async.Token.make 412)
                    server
                    reply
                    ~pos:0
                    ~len:(Kernel.IO.Iovec.length reply);
                  read_exact_vectored
                    poll
                    ~token:(Kernel.Async.Token.make 413)
                    client
                    client_buf
                    ~pos:0
                    ~len:(Kernel.IO.Iovec.length reply)))))

let recv_from_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv_from socket buffer with
    | Kernel.Result.Ok value -> value
    | Kernel.Result.Error error ->
        if is_udp_would_block error then
          (
            wait_readable poll ~token (Kernel.Net.UdpSocket.to_source socket);
            loop ()
          )
        else
          panic_udp error
  in
  loop ()

let recv_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv socket buffer with
    | Kernel.Result.Ok value -> value
    | Kernel.Result.Error error ->
        if is_udp_would_block error then
          (
            wait_readable poll ~token (Kernel.Net.UdpSocket.to_source socket);
            loop ()
          )
        else
          panic_udp error
  in
  loop ()

let bench_tcp_connect_accept_loopback = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let client = connect_stream poll addr in
          protect ~finally:(fun () -> close_stream client)
            (fun () ->
              let server = accept_stream poll listener in
              protect ~finally:(fun () -> close_stream server) (fun () -> ()))))

let bench_tcp_accept_burst = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let rec connect_many remaining acc =
            if remaining = 0 then
              List.reverse acc
            else
              let client = connect_stream poll addr in
              connect_many (remaining - 1) (client :: acc)
          in
          let clients = connect_many 16 [] in
          protect ~finally:(fun () -> close_streams clients)
            (fun () ->
              let rec accept_many remaining acc =
                if remaining = 0 then
                  List.reverse acc
                else
                  let server = accept_stream poll listener in
                  accept_many (remaining - 1) (server :: acc)
              in
              let servers = accept_many 16 [] in
              protect ~finally:(fun () -> close_streams servers) (fun () -> ()))))

let bench_udp_loopback_datagram = fun () ->
  with_poll
    (fun poll ->
      let server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_udp server)
        (fun () ->
          let client = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          protect ~finally:(fun () -> close_udp client)
            (fun () ->
              let server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let _ = lift_udp
                (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "ping")) in
              let buffer = Kernel.Bytes.create ~size:32 in
              let _ = recv_from_udp poll ~token:(Kernel.Async.Token.make 407) server buffer in
              ())))

let bench_udp_connected_roundtrip = fun () ->
  with_poll
    (fun poll ->
      let server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_udp server)
        (fun () ->
          let client = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          protect ~finally:(fun () -> close_udp client)
            (fun () ->
              let server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
              let _ = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
              let _ = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
              let _ = lift_udp (Kernel.Net.UdpSocket.send client (Kernel.Bytes.from_string "ping")) in
              let server_buf = Kernel.Bytes.create ~size:32 in
              let _ = recv_udp poll ~token:(Kernel.Async.Token.make 408) server server_buf in
              let _ = lift_udp (Kernel.Net.UdpSocket.send server (Kernel.Bytes.from_string "pong")) in
              let client_buf = Kernel.Bytes.create ~size:32 in
              let _ = recv_udp poll ~token:(Kernel.Async.Token.make 409) client client_buf in
              ())))

let bench_udp_burst_datagrams = fun () ->
  with_poll
    (fun poll ->
      let server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_udp server)
        (fun () ->
          let client = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          protect ~finally:(fun () -> close_udp client)
            (fun () ->
              let server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
              let payload = Kernel.Bytes.from_string (String.make ~len:64 ~char:'x') in
              let rec send_many remaining =
                if remaining = 0 then
                  ()
                else
                  let _ = lift_udp (Kernel.Net.UdpSocket.send_to client server_addr payload) in
                  send_many (remaining - 1)
              in
              let buffer = Kernel.Bytes.create ~size:128 in
              let rec recv_many remaining =
                if remaining = 0 then
                  ()
                else
                  let _ = recv_from_udp
                    poll
                    ~token:(Kernel.Async.Token.make ("udp-burst", remaining))
                    server
                    buffer in
                  recv_many (remaining - 1)
              in
              send_many 32;
              recv_many 32)))

let bulk_payload = Kernel.Bytes.from_string (String.make ~len:65_536 ~char:'x')

let bulk_reply = Kernel.Bytes.from_string (String.make ~len:65_536 ~char:'y')

let bulk_len = Kernel.Bytes.length bulk_payload

let bench_tcp_bulk_roundtrip = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let client = connect_stream poll addr in
          protect ~finally:(fun () -> close_stream client)
            (fun () ->
              let server = accept_stream poll listener in
              protect ~finally:(fun () -> close_stream server)
                (fun () ->
                  let server_buf = Kernel.Bytes.create ~size:bulk_len in
                  let client_buf = Kernel.Bytes.create ~size:bulk_len in
                  write_all_stream
                    poll
                    ~token:(Kernel.Async.Token.make 420)
                    client
                    bulk_payload
                    ~pos:0
                    ~len:bulk_len;
                  read_exact_stream
                    poll
                    ~token:(Kernel.Async.Token.make 421)
                    server
                    server_buf
                    ~pos:0
                    ~len:bulk_len;
                  write_all_stream
                    poll
                    ~token:(Kernel.Async.Token.make 422)
                    server
                    bulk_reply
                    ~pos:0
                    ~len:bulk_len;
                  read_exact_stream
                    poll
                    ~token:(Kernel.Async.Token.make 423)
                    client
                    client_buf
                    ~pos:0
                    ~len:bulk_len))))

let bench_udp_connected_peer_filtered_roundtrip = fun () ->
  with_poll
    (fun poll ->
      let server = lift_udp (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_udp server)
        (fun () ->
          let client = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          protect ~finally:(fun () -> close_udp client)
            (fun () ->
              let other = lift_udp
                (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
              protect ~finally:(fun () -> close_udp other)
                (fun () ->
                  let server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                  let client_addr = lift_udp (Kernel.Net.UdpSocket.local_addr client) in
                  let _ = lift_udp (Kernel.Net.UdpSocket.connect server client_addr) in
                  let _ = lift_udp (Kernel.Net.UdpSocket.connect client server_addr) in
                  let _ = lift_udp
                    (Kernel.Net.UdpSocket.send_to
                      other
                      server_addr
                      (Kernel.Bytes.from_string "rogue")) in
                  let ignored = Kernel.Bytes.create ~size:32 in
                  (
                    match Kernel.Net.UdpSocket.recv server ignored with
                    | Kernel.Result.Error error ->
                        if not (is_udp_would_block error) then
                          panic_udp error
                    | Kernel.Result.Ok _ -> Kernel.SystemError.panic "expected connected udp bench to ignore foreign datagrams"
                  );
                  let _ = lift_udp
                    (Kernel.Net.UdpSocket.send client (Kernel.Bytes.from_string "ping")) in
                  let server_buf = Kernel.Bytes.create ~size:32 in
                  let _ = recv_udp poll ~token:(Kernel.Async.Token.make 424) server server_buf in
                  let _ = lift_udp
                    (Kernel.Net.UdpSocket.send server (Kernel.Bytes.from_string "pong")) in
                  let client_buf = Kernel.Bytes.create ~size:32 in
                  let _ = recv_udp poll ~token:(Kernel.Async.Token.make 425) client client_buf in
                  ()))))

let bench_udp_many_source_readiness = fun () ->
  with_poll
    (fun poll ->
      let rec bind_many remaining acc =
        if remaining = 0 then
          acc
        else
          let server = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          let client = lift_udp
            (Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
          bind_many (remaining - 1) ((server, client) :: acc)
      in
      let pairs = List.reverse (bind_many 16 []) in
      protect ~finally:(fun () -> close_udp_pairs pairs)
        (fun () ->
          let rec register index = function
            | [] -> ()
            | (server, _) :: rest ->
                let _ = lift_async
                  (Kernel.Async.Poll.register
                    poll
                    (Kernel.Async.Token.make index)
                    Kernel.Async.Interest.readable
                    (Kernel.Net.UdpSocket.to_source server)) in
                register (index + 1) rest
          in
          let rec send_all = function
            | [] -> ()
            | (server, client) :: rest ->
                let server_addr = lift_udp (Kernel.Net.UdpSocket.local_addr server) in
                let _ = lift_udp
                  (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.from_string "x")) in
                send_all rest
          in
          register 0 pairs;
          send_all pairs;
          let _ = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
          ()))

let bench_tcp_many_stream_readiness = fun () ->
  with_poll
    (fun poll ->
      let listener = lift_tcp_listener
        (Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0)) in
      protect ~finally:(fun () -> close_listener listener)
        (fun () ->
          let addr = lift_tcp_listener (Kernel.Net.TcpListener.local_addr listener) in
          let rec connect_many remaining clients servers =
            if remaining = 0 then
              (List.reverse clients, List.reverse servers)
            else
              let client = connect_stream poll addr in
              let server = accept_stream poll listener in
              connect_many (remaining - 1) (client :: clients) (server :: servers)
          in
          let clients, servers = connect_many 16 [] [] in
          protect
            ~finally:(fun () ->
              close_streams clients;
              close_streams servers)
            (fun () ->
              let rec register index = function
                | [] -> ()
                | server :: rest ->
                    let _ = lift_async
                      (Kernel.Async.Poll.register
                        poll
                        (Kernel.Async.Token.make index)
                        Kernel.Async.Interest.readable
                        (Kernel.Net.TcpStream.to_source server)) in
                    register (index + 1) rest
              in
              let rec send_all index = function
                | [] -> ()
                | client :: rest ->
                    let payload = Kernel.Bytes.from_string "x" in
                    write_all_stream
                      poll
                      ~token:(Kernel.Async.Token.make (800 + index))
                      client
                      payload
                      ~pos:0
                      ~len:1;
                    send_all (index + 1) rest
              in
              register 0 servers;
              send_all 0 clients;
              let _ = lift_async (Kernel.Async.Poll.poll ~timeout:100_000_000L ~max_events:32 poll) in
              ())))

let benchmarks =
  Bench.[
    with_config ~config:{ iterations = 25; warmup = 5 } "net tcp connect+accept loopback" bench_tcp_connect_accept_loopback;
    with_config ~config:{ iterations = 20; warmup = 5 } "net tcp accept burst: 16 clients" bench_tcp_accept_burst;
    with_config ~config:{ iterations = 25; warmup = 5 } "net tcp loopback roundtrip" bench_tcp_loopback_roundtrip;
    with_config ~config:{ iterations = 25; warmup = 5 } "net tcp vectored roundtrip" bench_tcp_vectored_roundtrip;
    with_config ~config:{ iterations = 15; warmup = 3 } "net tcp bulk roundtrip: 64KiB" bench_tcp_bulk_roundtrip;
    with_config ~config:{ iterations = 25; warmup = 5 } "net udp loopback datagram" bench_udp_loopback_datagram;
    with_config ~config:{ iterations = 20; warmup = 5 } "net udp burst datagrams: 32 x 64B" bench_udp_burst_datagrams;
    with_config ~config:{ iterations = 25; warmup = 5 } "net udp connected roundtrip" bench_udp_connected_roundtrip;
    with_config ~config:{ iterations = 25; warmup = 5 } "net udp connected peer-filtered roundtrip" bench_udp_connected_peer_filtered_roundtrip;
    with_config ~config:{ iterations = 20; warmup = 5 } "net udp many-source readiness" bench_udp_many_source_readiness;
    with_config ~config:{ iterations = 20; warmup = 5 } "net tcp many-stream readiness" bench_tcp_many_stream_readiness;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_net_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
