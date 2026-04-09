open Std
module Kernel = Kernel_new

let panic_async = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_async error))

let panic_tcp_listener = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))

let panic_tcp_stream = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))

let panic_udp = fun error ->
  Kernel.SystemError.panic (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))

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
  | Kernel.Net.TcpListener.System system_error -> Kernel.SystemError.is_would_block system_error
  | _ -> false

let is_tcp_stream_would_block error =
  match error with
  | Kernel.Net.TcpStream.WouldBlock -> true
  | Kernel.Net.TcpStream.System system_error -> Kernel.SystemError.is_would_block system_error
  | _ -> false

let is_udp_would_block error =
  match error with
  | Kernel.Net.UdpSocket.WouldBlock -> true
  | Kernel.Net.UdpSocket.System system_error -> Kernel.SystemError.is_would_block system_error
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
        List.exists
          (fun event -> Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
          events
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
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
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
    let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
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
                  let payload = Kernel.Bytes.of_string "ping" in
                  let reply = Kernel.Bytes.of_string "pong" in
                  let server_buf = Kernel.Bytes.create 4 in
                  let client_buf = Kernel.Bytes.create 4 in
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
                  let payload = Kernel.IO.Iovec.of_string_array [|"pi"; "ng"|] in
                  let reply = Kernel.IO.Iovec.of_string_array [|"po"; "ng"|] in
                  let server_buf = Kernel.IO.Iovec.create ~count:2 ~size:4 () in
                  let client_buf = Kernel.IO.Iovec.create ~count:2 ~size:4 () in
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
                (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.of_string "ping")) in
              let buffer = Kernel.Bytes.create 32 in
              ignore (recv_from_udp poll ~token:(Kernel.Async.Token.make 407) server buffer))))

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
              let _ = lift_udp (Kernel.Net.UdpSocket.send client (Kernel.Bytes.of_string "ping")) in
              let server_buf = Kernel.Bytes.create 32 in
              ignore (recv_udp poll ~token:(Kernel.Async.Token.make 408) server server_buf);
              let _ = lift_udp (Kernel.Net.UdpSocket.send server (Kernel.Bytes.of_string "pong")) in
              let client_buf = Kernel.Bytes.create 32 in
              ignore (recv_udp poll ~token:(Kernel.Async.Token.make 409) client client_buf))))

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
      let pairs = List.rev (bind_many 16 []) in
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
                  (Kernel.Net.UdpSocket.send_to client server_addr (Kernel.Bytes.of_string "x")) in
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
              (List.rev clients, List.rev servers)
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
                    let payload = Kernel.Bytes.of_string "x" in
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
    with_config ~config:{ iterations = 25; warmup = 5 } "net tcp loopback roundtrip" bench_tcp_loopback_roundtrip;
    with_config ~config:{ iterations = 25; warmup = 5 } "net tcp vectored roundtrip" bench_tcp_vectored_roundtrip;
    with_config ~config:{ iterations = 25; warmup = 5 } "net udp loopback datagram" bench_udp_loopback_datagram;
    with_config ~config:{ iterations = 25; warmup = 5 } "net udp connected roundtrip" bench_udp_connected_roundtrip;
    with_config ~config:{ iterations = 20; warmup = 5 } "net udp many-source readiness" bench_udp_many_source_readiness;
    with_config ~config:{ iterations = 20; warmup = 5 } "net tcp many-stream readiness" bench_tcp_many_stream_readiness;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"kernel_new_net_bench" ~benchmarks ~args)
    ~args:Env.args
    ()
