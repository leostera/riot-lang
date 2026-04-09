open Std
open Propane
module Test = Std.Test
module Kernel = Kernel_new

let contains_slash = fun value ->
  let rec loop index =
    if index >= String.length value then
      false
    else if String.get value index = '/' then
      true
    else
      loop (index + 1)
  in
  loop 0

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let array_to_list = fun values ->
  let rec loop index acc =
    if index < 0 then
      acc
    else
      loop (index - 1) (Kernel.Array.get values index :: acc)
  in
  loop (Kernel.Array.length values - 1) []

let with_temp_path = fun prefix filename fn ->
  match
    Fs.with_tempdir ~prefix
      (fun tempdir ->
        let path = Kernel.Path.(Path.to_string tempdir / filename) in
        fn path)
  with
  | Ok value -> value
  | Error err -> fail (IO.error_message err)

let with_poll = fun fn ->
  match Kernel.Async.Poll.make () with
  | Kernel.Result.Ok poll ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.close poll in
          ())
        (fun () -> fn poll)
  | Kernel.Result.Error error -> fail (Kernel.Error.to_string (Kernel.Error.of_async error))

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
  match Kernel.Async.Poll.register poll token interest source with
  | Kernel.Result.Error error -> fail (Kernel.Error.to_string (Kernel.Error.of_async error))
  | Kernel.Result.Ok () ->
      protect
        ~finally:(fun () ->
          let _ = Kernel.Async.Poll.deregister poll source in
          ())
        (fun () ->
          match Kernel.Async.Poll.poll ~timeout:100_000_000L poll with
          | Kernel.Result.Error error -> fail (Kernel.Error.to_string (Kernel.Error.of_async error))
          | Kernel.Result.Ok events -> List.exists
            (fun event -> Kernel.Async.Token.equal token (Kernel.Async.Event.token event) && pred event)
            events)

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

let connect_stream = fun poll addr ->
  match Kernel.Net.TcpStream.connect addr with
  | Kernel.Result.Error error ->
      fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
  | Kernel.Result.Ok Kernel.Net.TcpStream.Connected stream ->
      stream
  | Kernel.Result.Ok (Kernel.Net.TcpStream.InProgress stream) ->
      let token = Kernel.Async.Token.make 801 in
      let rec finish attempts =
        if attempts = 0 then
          fail "expected property tcp connect to complete"
        else if wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) then
          match Kernel.Net.TcpStream.finish_connect stream with
          | Kernel.Result.Ok () -> stream
          | Kernel.Result.Error error ->
              if is_tcp_stream_would_block error then
                finish (attempts - 1)
              else
                fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
        else
          fail "expected property tcp connect to become writable"
      in
      finish 8

let accept_stream = fun poll listener ->
  let rec loop () =
    match Kernel.Net.TcpListener.accept listener with
    | Kernel.Result.Ok (stream, _) -> stream
    | Kernel.Result.Error error ->
        if is_tcp_listener_would_block error then
          if
            wait_readable
              poll
              ~token:(Kernel.Async.Token.make 802)
              (Kernel.Net.TcpListener.to_source listener)
          then
            loop ()
          else
            fail "expected property tcp listener to become readable"
        else
          fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))
  in
  loop ()

let write_all_stream = fun poll ~token stream buffer ->
  let rec loop pos len =
    if len = 0 then
      ()
    else
      match Kernel.Net.TcpStream.write stream ~pos ~len buffer with
      | Kernel.Result.Ok written ->
          if written <= 0 then
            fail "expected property tcp write to make progress"
          else
            loop (pos + written) (len - written)
      | Kernel.Result.Error error ->
          if is_tcp_stream_would_block error then
            if wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) then
              loop pos len
            else
              fail "expected property tcp write to become writable"
          else
            fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
  in
  loop 0 (Kernel.Bytes.length buffer)

let write_all_vectored = fun poll ~token stream iov ->
  let rec loop pos len =
    if len = 0 then
      ()
    else
      let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
      match Kernel.Net.TcpStream.write_vectored stream slice with
      | Kernel.Result.Ok written ->
          if written <= 0 then
            fail "expected property tcp vectored write to make progress"
          else
            loop (pos + written) (len - written)
      | Kernel.Result.Error error ->
          if is_tcp_stream_would_block error then
            if wait_writable poll ~token (Kernel.Net.TcpStream.to_source stream) then
              loop pos len
            else
              fail "expected property tcp vectored write to become writable"
          else
            fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
  in
  loop 0 (Kernel.IO.Iovec.length iov)

let read_exact_stream = fun poll ~token stream buffer ->
  let rec loop pos len =
    if len = 0 then
      ()
    else
      match Kernel.Net.TcpStream.read stream ~pos ~len buffer with
      | Kernel.Result.Ok read ->
          if read <= 0 then
            fail "expected property tcp read to make progress"
          else
            loop (pos + read) (len - read)
      | Kernel.Result.Error error ->
          if is_tcp_stream_would_block error then
            if wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream) then
              loop pos len
            else
              fail "expected property tcp read to become readable"
          else
            fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
  in
  loop 0 (Kernel.Bytes.length buffer)

let read_exact_vectored = fun poll ~token stream iov ~len ->
  let rec loop pos remaining =
    if remaining = 0 then
      ()
    else
      let slice = Kernel.IO.Iovec.sub ~pos ~len:remaining iov in
      match Kernel.Net.TcpStream.read_vectored stream slice with
      | Kernel.Result.Ok read ->
          if read <= 0 then
            fail "expected property tcp vectored read to make progress"
          else
            loop (pos + read) (remaining - read)
      | Kernel.Result.Error error ->
          if is_tcp_stream_would_block error then
            if wait_readable poll ~token (Kernel.Net.TcpStream.to_source stream) then
              loop pos remaining
            else
              fail "expected property tcp vectored read to become readable"
          else
            fail (Kernel.Error.to_string (Kernel.Error.of_net_tcp_stream error))
  in
  loop 0 len

let recv_from_udp = fun poll ~token socket buffer ->
  let rec loop () =
    match Kernel.Net.UdpSocket.recv_from socket buffer with
    | Kernel.Result.Ok value -> value
    | Kernel.Result.Error error ->
        if is_udp_would_block error then
          if wait_readable poll ~token (Kernel.Net.UdpSocket.to_source socket) then
            loop ()
          else
            fail "expected property udp socket to become readable"
        else
          fail (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))
  in
  loop ()

let ipv4_text = fun a b c d ->
  String.concat
    "."
    [
      Int.to_string (Int.abs a mod 256);
      Int.to_string (Int.abs b mod 256);
      Int.to_string (Int.abs c mod 256);
      Int.to_string (Int.abs d mod 256);
    ]

let hex_group = fun raw ->
  let digits = "0123456789abcdef" in
  let value = Int.abs raw mod 65_536 in
  if value = 0 then
    "0"
  else
    let rec loop remaining acc =
      if remaining = 0 then
        acc
      else
        let digit = String.make 1 (String.get digits (remaining mod 16)) in
        loop (remaining / 16) (digit :: acc)
    in
    String.concat "" (loop value [])

let ipv6_text = fun raw_groups ->
  let length = Kernel.Array.length raw_groups in
  let groups =
    Kernel.Array.init 8 (fun index -> hex_group (Kernel.Array.get raw_groups (index mod length)))
  in
  String.concat ":" (array_to_list groups)

let iovec_into_string_roundtrips =
  property "IO.Iovec of_string_array flattens with preserved order" Arbitrary.(array string)
    (fun values ->
      let iov = Kernel.IO.Iovec.of_string_array values in
      Kernel.IO.Iovec.into_string iov = String.concat "" (array_to_list values))

let iovec_sub_matches_flattened_substring =
  property "IO.Iovec.sub matches the flattened substring" Arbitrary.(pair
    (array string)
    (pair int int))
    (fun (values, (raw_pos, raw_len)) ->
      let iov = Kernel.IO.Iovec.of_string_array values in
      let total = Kernel.IO.Iovec.length iov in
      assume (total > 0);
      let pos = Int.abs raw_pos mod total in
      let remaining = total - pos in
      let len = Int.abs raw_len mod (remaining + 1) in
      let expected = String.sub (Kernel.IO.Iovec.into_string iov) pos len in
      let actual = Kernel.IO.Iovec.into_string (Kernel.IO.Iovec.sub ~pos ~len iov) in
      actual = expected)

let array_of_list_roundtrips =
  property
    "Array.of_list roundtrips arbitrary integer arrays"
    Arbitrary.(array int)
    (fun values -> array_to_list (Kernel.Array.of_list (array_to_list values)) = array_to_list values)

let array_map_preserves_order =
  property
    "Array.map preserves element order"
    Arbitrary.(array int)
    (fun values ->
      array_to_list (Kernel.Array.map (fun value -> value + 1) values)
      = List.map (fun value -> value + 1) (array_to_list values))

let char_of_int_roundtrips_valid_bytes =
  property "Char.of_int roundtrips valid byte values" Arbitrary.int
    (fun raw ->
      let value = Int.abs raw mod 256 in
      match Kernel.Char.of_int value with
      | Some char -> Kernel.Char.to_int char = value
      | None -> false)

let option_unwrap_or_prefers_some =
  property
    "Option.unwrap_or prefers the Some branch"
    Arbitrary.(pair int int)
    (fun (value, default) ->
      Kernel.Option.unwrap_or (Kernel.Option.Some value) ~default = value
      && Kernel.Option.unwrap_or Kernel.Option.None ~default = default)

let result_map_error_preserves_ok =
  property "Result.map_error preserves the Ok branch" Arbitrary.int
    (fun value ->
      match Kernel.Result.map_error (fun _ -> "mapped") (Kernel.Result.Ok value) with
      | Kernel.Result.Ok mapped -> mapped = value
      | Kernel.Result.Error _ -> false)

let bytes_string_roundtrip =
  property "Bytes.of_string and Bytes.to_string roundtrip arbitrary strings" Arbitrary.string
    (fun value ->
      Kernel.String.equal (Kernel.Bytes.to_string (Kernel.Bytes.of_string value)) value)

let bytes_sub_string_matches_string_sub =
  property "Bytes.sub_string matches String.sub on valid slices" Arbitrary.(pair
    string
    (pair int int))
    (fun (value, (raw_pos, raw_len)) ->
      let total = String.length value in
      assume (total > 0);
      let pos = Int.abs raw_pos mod total in
      let remaining = total - pos in
      let len = Int.abs raw_len mod (remaining + 1) in
      let bytes = Kernel.Bytes.of_string value in
      Kernel.String.equal (Kernel.Bytes.sub_string bytes pos len) (String.sub value pos len))

let string_bytes_roundtrip =
  property "String.to_bytes and String.of_bytes roundtrip arbitrary strings" Arbitrary.string
    (fun value ->
      Kernel.String.equal (Kernel.String.of_bytes (Kernel.String.to_bytes value)) value)

let path_join_preserves_segment_order =
  property "Path.join preserves simple segment order" Arbitrary.(pair string string)
    (fun (left, right) ->
      assume (String.length left > 0);
      assume (String.length right > 0);
      assume (not (contains_slash left));
      assume (not (contains_slash right));
      Kernel.Path.to_string (Kernel.Path.join left right)
      = format Format.[ str left; str "/"; str right ])

let path_fold_join_preserves_many_segment_order =
  property "Path.join preserves many simple segments in order" Arbitrary.(array string)
    (fun segments ->
      let values = array_to_list segments in
      let simple =
        List.filter (fun value -> String.length value > 0 && not (contains_slash value)) values
      in
      assume (not (List.is_empty simple));
      let actual =
        List.fold_left
          (fun acc part ->
            match acc with
            | None -> Some part
            | Some path -> Some (Kernel.Path.join path part))
          None
          simple
      in
      match actual with
      | None -> false
      | Some actual -> Kernel.Path.to_string actual = String.concat "/" simple)

let path_join_is_associative_for_simple_segments =
  property "Path.join is associative for simple segments" Arbitrary.(triple string string string)
    (fun (a, b, c) ->
      assume (String.length a > 0);
      assume (String.length b > 0);
      assume (String.length c > 0);
      assume (not (contains_slash a));
      assume (not (contains_slash b));
      assume (not (contains_slash c));
      let left = Kernel.Path.to_string (Kernel.Path.join (Kernel.Path.join a b) c) in
      let right = Kernel.Path.to_string (Kernel.Path.join a (Kernel.Path.join b c)) in
      left = right)

let path_of_string_roundtrips =
  property "Path.of_string and Path.to_string roundtrip arbitrary text" Arbitrary.string
    (fun value ->
      Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.of_string value)) value)

let path_join_treats_empty_sides_as_identity =
  property
    "Path.join treats empty sides as identity"
    Arbitrary.string
    (fun value ->
      Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.join "" value)) value
      && Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.join value "")) value)

let ip_addr_loopback_parse_render_roundtrips =
  property "Net.IpAddr loopback parse/render roundtrips" Arbitrary.bool
    (fun use_v6 ->
      let value =
        if use_v6 then
          Kernel.Net.IpAddr.v6_loopback
        else
          Kernel.Net.IpAddr.v4_loopback
      in
      match Kernel.Net.IpAddr.of_string (Kernel.Net.IpAddr.to_string value) with
      | Kernel.Result.Ok parsed -> Kernel.Net.IpAddr.equal parsed value
      | Kernel.Result.Error _ -> false)

let ip_addr_ipv4_parse_render_roundtrips =
  property "Net.IpAddr ipv4 parse/render roundtrips" Arbitrary.(pair (pair int int) (pair int int))
    (fun ((a, b), (c, d)) ->
      let text = ipv4_text a b c d in
      match Kernel.Net.IpAddr.of_string text with
      | Kernel.Result.Ok parsed -> Kernel.Net.IpAddr.to_string parsed = text
      | Kernel.Result.Error _ -> false)

let ip_addr_ipv6_parse_render_roundtrips =
  property "Net.IpAddr ipv6 parse/render roundtrips" Arbitrary.(array int)
    (fun raw_groups ->
      assume (Kernel.Array.length raw_groups > 0);
      let text = ipv6_text raw_groups in
      match Kernel.Net.IpAddr.of_string text with
      | Kernel.Result.Error _ -> false
      | Kernel.Result.Ok parsed ->
          match Kernel.Net.IpAddr.of_string (Kernel.Net.IpAddr.to_string parsed) with
          | Kernel.Result.Ok reparsed -> Kernel.Net.IpAddr.equal reparsed parsed
          | Kernel.Result.Error _ -> false)

let socket_addr_roundtrips =
  property "Net.SocketAddr.make roundtrips loopback parts" Arbitrary.(pair bool int)
    (fun (use_v6, raw_port) ->
      let port = Int.abs raw_port mod 65_536 in
      let ip =
        if use_v6 then
          Kernel.Net.IpAddr.v6_loopback
        else
          Kernel.Net.IpAddr.v4_loopback
      in
      match Kernel.Net.SocketAddr.make ~ip ~port with
      | Kernel.Result.Error _ -> false
      | Kernel.Result.Ok addr ->
          let addr_ip, addr_port = Kernel.Net.SocketAddr.to_parts addr in
          Kernel.Net.IpAddr.equal addr_ip ip && addr_port = port)

let socket_addr_ipv4_roundtrips =
  property "Net.SocketAddr.make roundtrips arbitrary ipv4 parts" Arbitrary.(pair
    (pair (pair int int) (pair int int))
    int)
    (fun (((a, b), (c, d)), raw_port) ->
      let text = ipv4_text a b c d in
      let port = Int.abs raw_port mod 65_536 in
      match Kernel.Net.IpAddr.of_string text with
      | Kernel.Result.Error _ -> false
      | Kernel.Result.Ok ip ->
          match Kernel.Net.SocketAddr.make ~ip ~port with
          | Kernel.Result.Error _ -> false
          | Kernel.Result.Ok addr ->
              let addr_ip, addr_port = Kernel.Net.SocketAddr.to_parts addr in
              Kernel.Net.IpAddr.equal addr_ip ip && addr_port = port)

let socket_addr_ipv6_roundtrips =
  property "Net.SocketAddr.make roundtrips arbitrary ipv6 parts" Arbitrary.(pair (array int) int)
    (fun (raw_groups, raw_port) ->
      assume (Kernel.Array.length raw_groups > 0);
      let text = ipv6_text raw_groups in
      let port = Int.abs raw_port mod 65_536 in
      match Kernel.Net.IpAddr.of_string text with
      | Kernel.Result.Error _ -> false
      | Kernel.Result.Ok ip ->
          match Kernel.Net.SocketAddr.make ~ip ~port with
          | Kernel.Result.Error _ -> false
          | Kernel.Result.Ok addr ->
              let addr_ip, addr_port = Kernel.Net.SocketAddr.to_parts addr in
              Kernel.Net.IpAddr.equal addr_ip ip && addr_port = port)

let socket_addr_loopback_v6_preserves_parts =
  property "Net.SocketAddr.loopback_v6 preserves loopback parts" Arbitrary.int
    (fun raw_port ->
      let port = Int.abs raw_port mod 65_536 in
      let addr = Kernel.Net.SocketAddr.loopback_v6 ~port in
      let addr_ip, addr_port = Kernel.Net.SocketAddr.to_parts addr in
      Kernel.Net.IpAddr.equal addr_ip Kernel.Net.IpAddr.v6_loopback && addr_port = port)

let file_slice_roundtrips =
  property "Fs.File partial write and read preserve the selected slice" Arbitrary.(pair
    string
    (pair int int))
    (fun (payload, (raw_pos, raw_len)) ->
      assume (String.length payload > 0);
      assume (String.length payload <= 256);
      let bytes = Kernel.Bytes.of_string payload in
      let total = Kernel.Bytes.length bytes in
      let pos = Int.abs raw_pos mod total in
      let remaining = total - pos in
      let len = (Int.abs raw_len mod remaining) + 1 in
      with_temp_path "kernel_new_property" "slice.bin"
        (fun path ->
          match Kernel.Fs.File.open_write path with
          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok file ->
              try
                match Kernel.Fs.File.write file ~pos ~len bytes with
                | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                | Kernel.Result.Ok written ->
                    let _ = Kernel.Fs.File.close file in
                    if written != len then
                      false
                    else
                      match Kernel.Fs.File.open_read path with
                      | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                      | Kernel.Result.Ok input ->
                          let buffer = Kernel.Bytes.create len in
                          match Kernel.Fs.File.read input buffer with
                          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                          | Kernel.Result.Ok read ->
                              let _ = Kernel.Fs.File.close input in
                              read = len
                              && Kernel.Bytes.sub_string bytes pos len
                              = Kernel.Bytes.sub_string buffer 0 len
              with
              | error ->
                  let _ = Kernel.Fs.File.close file in
                  raise error))

let file_vectored_roundtrips =
  property "Fs.File vectored writes and scalar reads preserve payload" Arbitrary.(array string)
    (fun values ->
      let pieces = array_to_list values in
      let total =
        List.fold_left (fun size part -> size + String.length part) 0 pieces
      in
      assume (total > 0);
      assume (total <= 256);
      with_temp_path "kernel_new_property" "vectored.bin"
        (fun path ->
          let iov = Kernel.IO.Iovec.of_string_array values in
          match Kernel.Fs.File.open_write path with
          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok file ->
              try
                match Kernel.Fs.File.write_vectored file iov with
                | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                | Kernel.Result.Ok written ->
                    let _ = Kernel.Fs.File.close file in
                    if written != Kernel.IO.Iovec.length iov then
                      false
                    else
                      match Kernel.Fs.File.open_read path with
                      | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                      | Kernel.Result.Ok input ->
                          let buffer = Kernel.Bytes.create total in
                          match Kernel.Fs.File.read input buffer with
                          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                          | Kernel.Result.Ok read ->
                              let _ = Kernel.Fs.File.close input in
                              read = total
                              && Kernel.Bytes.sub_string buffer 0 read = String.concat "" pieces
              with
              | error ->
                  let _ = Kernel.Fs.File.close file in
                  raise error))

let file_scalar_write_vectored_read_roundtrips =
  property "Fs.File scalar writes and vectored reads preserve payload" Arbitrary.string
    (fun payload ->
      assume (String.length payload > 0);
      assume (String.length payload <= 256);
      with_temp_path "kernel_new_property" "vectored-read.bin"
        (fun path ->
          let bytes = Kernel.Bytes.of_string payload in
          match Kernel.Fs.File.open_write path with
          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok file ->
              try
                match Kernel.Fs.File.write file bytes with
                | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                | Kernel.Result.Ok written ->
                    let _ = Kernel.Fs.File.close file in
                    if written != Kernel.Bytes.length bytes then
                      false
                    else
                      match Kernel.Fs.File.open_read path with
                      | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                      | Kernel.Result.Ok input ->
                          let iov = Kernel.IO.Iovec.create ~count:4 ~size:64 () in
                          match Kernel.Fs.File.read_vectored input iov with
                          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                          | Kernel.Result.Ok read ->
                              let _ = Kernel.Fs.File.close input in
                              read = String.length payload
                              && String.sub (Kernel.IO.Iovec.into_string iov) 0 read = payload
              with
              | error ->
                  let _ = Kernel.Fs.File.close file in
                  raise error))

let file_scalar_and_vectored_partial_writes_agree =
  property "Fs.File scalar and vectored partial writes agree on the selected slice" Arbitrary.(pair
    (array string)
    (pair int int))
    (fun (values, (raw_pos, raw_len)) ->
      let iov = Kernel.IO.Iovec.of_string_array values in
      let payload = Kernel.IO.Iovec.into_string iov in
      let total = String.length payload in
      assume (total > 0);
      assume (total <= 256);
      let bytes = Kernel.Bytes.of_string payload in
      let pos = Int.abs raw_pos mod total in
      let remaining = total - pos in
      let len = (Int.abs raw_len mod remaining) + 1 in
      let expected = String.sub payload pos len in
      let scalar_matches =
        with_temp_path "kernel_new_property" "scalar-partial-equivalence.bin"
          (fun path ->
            match Kernel.Fs.File.open_write path with
            | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
            | Kernel.Result.Ok file ->
                try
                  match Kernel.Fs.File.write file ~pos ~len bytes with
                  | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                  | Kernel.Result.Ok written ->
                      let _ = Kernel.Fs.File.close file in
                      if written != len then
                        false
                      else
                        match Kernel.Fs.File.open_read path with
                        | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                        | Kernel.Result.Ok input ->
                            let buffer = Kernel.Bytes.create len in
                            match Kernel.Fs.File.read input buffer with
                            | Kernel.Result.Error error -> fail
                              (Kernel.Fs.File.error_to_string error)
                            | Kernel.Result.Ok read ->
                                let _ = Kernel.Fs.File.close input in
                                read = len && Kernel.Bytes.sub_string buffer 0 read = expected
                with
                | error ->
                    let _ = Kernel.Fs.File.close file in
                    raise error)
      in
      let vectored_matches =
        with_temp_path "kernel_new_property" "vectored-partial-equivalence.bin"
          (fun path ->
            match Kernel.Fs.File.open_write path with
            | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
            | Kernel.Result.Ok file ->
                try
                  let slice = Kernel.IO.Iovec.sub ~pos ~len iov in
                  match Kernel.Fs.File.write_vectored file slice with
                  | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                  | Kernel.Result.Ok written ->
                      let _ = Kernel.Fs.File.close file in
                      if written != len then
                        false
                      else
                        match Kernel.Fs.File.open_read path with
                        | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                        | Kernel.Result.Ok input ->
                            let buffer = Kernel.Bytes.create len in
                            match Kernel.Fs.File.read input buffer with
                            | Kernel.Result.Error error -> fail
                              (Kernel.Fs.File.error_to_string error)
                            | Kernel.Result.Ok read ->
                                let _ = Kernel.Fs.File.close input in
                                read = len && Kernel.Bytes.sub_string buffer 0 read = expected
                with
                | error ->
                    let _ = Kernel.Fs.File.close file in
                    raise error)
      in
      scalar_matches && vectored_matches)

let file_scalar_and_vectored_partial_reads_agree =
  property "Fs.File scalar and vectored partial reads agree on the selected prefix" Arbitrary.(pair
    string
    int)
    (fun (payload, raw_len) ->
      assume (String.length payload > 0);
      assume (String.length payload <= 256);
      let total = String.length payload in
      let len = (Int.abs raw_len mod total) + 1 in
      let expected = String.sub payload 0 len in
      with_temp_path "kernel_new_property" "partial-read-equivalence.bin"
        (fun path ->
          let bytes = Kernel.Bytes.of_string payload in
          match Kernel.Fs.File.open_write path with
          | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok file ->
              try
                match Kernel.Fs.File.write file bytes with
                | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                | Kernel.Result.Ok written ->
                    let _ = Kernel.Fs.File.close file in
                    if written != total then
                      false
                    else
                      let scalar_result =
                        match Kernel.Fs.File.open_read path with
                        | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                        | Kernel.Result.Ok input ->
                            let buffer = Kernel.Bytes.create (len + 2) in
                            match Kernel.Fs.File.read input ~pos:1 ~len buffer with
                            | Kernel.Result.Error error -> fail
                              (Kernel.Fs.File.error_to_string error)
                            | Kernel.Result.Ok read ->
                                let _ = Kernel.Fs.File.close input in
                                read = len && Kernel.Bytes.sub_string buffer 1 len = expected
                      in
                      let vectored_result =
                        match Kernel.Fs.File.open_read path with
                        | Kernel.Result.Error error -> fail (Kernel.Fs.File.error_to_string error)
                        | Kernel.Result.Ok input ->
                            let iov = Kernel.IO.Iovec.create ~count:3 ~size:(len + 2) () in
                            let slice = Kernel.IO.Iovec.sub ~pos:1 ~len iov in
                            match Kernel.Fs.File.read_vectored input slice with
                            | Kernel.Result.Error error -> fail
                              (Kernel.Fs.File.error_to_string error)
                            | Kernel.Result.Ok read ->
                                let _ = Kernel.Fs.File.close input in
                                read = len && String.sub (Kernel.IO.Iovec.into_string iov) 1 len = expected
                      in
                      scalar_result && vectored_result
              with
              | error ->
                  let _ = Kernel.Fs.File.close file in
                  raise error))

let tcp_loopback_roundtrips_small_payload =
  property "Net.TcpStream loopback roundtrips small payloads" Arbitrary.string
    (fun payload ->
      assume (String.length payload > 0);
      assume (String.length payload <= 64);
      with_poll
        (fun poll ->
          match Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
          | Kernel.Result.Error error -> fail
            (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))
          | Kernel.Result.Ok listener ->
              protect ~finally:(fun () -> close_listener listener)
                (fun () ->
                  match Kernel.Net.TcpListener.local_addr listener with
                  | Kernel.Result.Error error -> fail
                    (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))
                  | Kernel.Result.Ok addr ->
                      let client = connect_stream poll addr in
                      protect ~finally:(fun () -> close_stream client)
                        (fun () ->
                          let server = accept_stream poll listener in
                          protect ~finally:(fun () -> close_stream server)
                            (fun () ->
                              let bytes = Kernel.Bytes.of_string payload in
                              let buffer = Kernel.Bytes.create (String.length payload) in
                              write_all_stream poll ~token:(Kernel.Async.Token.make 803) client bytes;
                              read_exact_stream poll ~token:(Kernel.Async.Token.make 804) server buffer;
                              Kernel.Bytes.sub_string buffer 0 (String.length payload) = payload)))))

let tcp_vectored_loopback_roundtrips_small_payload =
  property "Net.TcpStream loopback roundtrips small vectored payloads" Arbitrary.(array string)
    (fun values ->
      let pieces = array_to_list values in
      assume (List.for_all (fun value -> String.length value > 0) pieces);
      let payload = String.concat "" pieces in
      let total = String.length payload in
      assume (total > 0);
      assume (total <= 64);
      with_poll
        (fun poll ->
          match Kernel.Net.TcpListener.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
          | Kernel.Result.Error error -> fail
            (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))
          | Kernel.Result.Ok listener ->
              protect ~finally:(fun () -> close_listener listener)
                (fun () ->
                  match Kernel.Net.TcpListener.local_addr listener with
                  | Kernel.Result.Error error -> fail
                    (Kernel.Error.to_string (Kernel.Error.of_net_tcp_listener error))
                  | Kernel.Result.Ok addr ->
                      let client = connect_stream poll addr in
                      protect ~finally:(fun () -> close_stream client)
                        (fun () ->
                          let server = accept_stream poll listener in
                          protect ~finally:(fun () -> close_stream server)
                            (fun () ->
                              let count =
                                if total < 4 then
                                  total
                                else
                                  4
                              in
                              let outbound = Kernel.IO.Iovec.of_string_array values in
                              let inbound = Kernel.IO.Iovec.create ~count ~size:total () in
                              write_all_vectored poll ~token:(Kernel.Async.Token.make 806) client outbound;
                              read_exact_vectored
                                poll
                                ~token:(Kernel.Async.Token.make 807)
                                server
                                inbound
                                ~len:total;
                              String.sub (Kernel.IO.Iovec.into_string inbound) 0 total = payload)))))

let udp_loopback_roundtrips_small_payload =
  property "Net.UdpSocket loopback preserves small datagrams" Arbitrary.string
    (fun payload ->
      assume (String.length payload > 0);
      assume (String.length payload <= 64);
      with_poll
        (fun poll ->
          match Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
          | Kernel.Result.Error error -> fail
            (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))
          | Kernel.Result.Ok server ->
              protect ~finally:(fun () -> close_udp server)
                (fun () ->
                  match Kernel.Net.UdpSocket.bind (Kernel.Net.SocketAddr.loopback_v4 ~port:0) with
                  | Kernel.Result.Error error -> fail
                    (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))
                  | Kernel.Result.Ok client ->
                      protect ~finally:(fun () -> close_udp client)
                        (fun () ->
                          match Kernel.Net.UdpSocket.local_addr server with
                          | Kernel.Result.Error error -> fail
                            (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))
                          | Kernel.Result.Ok server_addr ->
                              let bytes = Kernel.Bytes.of_string payload in
                              match Kernel.Net.UdpSocket.send_to client server_addr bytes with
                              | Kernel.Result.Error error -> fail
                                (Kernel.Error.to_string (Kernel.Error.of_net_udp_socket error))
                              | Kernel.Result.Ok written ->
                                  if written != String.length payload then
                                    false
                                  else
                                    let buffer = Kernel.Bytes.create 96 in
                                    let read, _from = recv_from_udp
                                      poll
                                      ~token:(Kernel.Async.Token.make 805)
                                      server
                                      buffer in
                                    read = String.length payload
                                    && Kernel.Bytes.sub_string buffer 0 read = payload))))

let tests = [
  iovec_into_string_roundtrips;
  iovec_sub_matches_flattened_substring;
  array_of_list_roundtrips;
  array_map_preserves_order;
  char_of_int_roundtrips_valid_bytes;
  option_unwrap_or_prefers_some;
  result_map_error_preserves_ok;
  bytes_string_roundtrip;
  bytes_sub_string_matches_string_sub;
  string_bytes_roundtrip;
  path_join_preserves_segment_order;
  path_fold_join_preserves_many_segment_order;
  path_join_is_associative_for_simple_segments;
  path_of_string_roundtrips;
  path_join_treats_empty_sides_as_identity;
  ip_addr_loopback_parse_render_roundtrips;
  ip_addr_ipv4_parse_render_roundtrips;
  ip_addr_ipv6_parse_render_roundtrips;
  socket_addr_roundtrips;
  socket_addr_ipv4_roundtrips;
  socket_addr_ipv6_roundtrips;
  socket_addr_loopback_v6_preserves_parts;
  file_slice_roundtrips;
  file_vectored_roundtrips;
  file_scalar_write_vectored_read_roundtrips;
  file_scalar_and_vectored_partial_writes_agree;
  file_scalar_and_vectored_partial_reads_agree;
  tcp_loopback_roundtrips_small_payload;
  tcp_vectored_loopback_roundtrips_small_payload;
  udp_loopback_roundtrips_small_payload;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_property_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
