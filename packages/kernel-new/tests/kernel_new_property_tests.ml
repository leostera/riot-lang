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

let ipv4_text = fun a b c d ->
  String.concat
    "."
    [
      Int.to_string (Int.abs a mod 256);
      Int.to_string (Int.abs b mod 256);
      Int.to_string (Int.abs c mod 256);
      Int.to_string (Int.abs d mod 256);
    ]

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

let path_join_preserves_segment_order =
  property "Path.join preserves simple segment order" Arbitrary.(pair string string)
    (fun (left, right) ->
      assume (String.length left > 0);
      assume (String.length right > 0);
      assume (not (contains_slash left));
      assume (not (contains_slash right));
      Kernel.Path.to_string (Kernel.Path.join left right) = left ^ "/" ^ right)

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
              let result =
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
                            let read_result =
                              match Kernel.Fs.File.read input buffer with
                              | Kernel.Result.Error error -> fail
                                (Kernel.Fs.File.error_to_string error)
                              | Kernel.Result.Ok read ->
                                  let _ = Kernel.Fs.File.close input in
                                  read = len
                                  && Kernel.Bytes.sub_string bytes pos len
                                  = Kernel.Bytes.sub_string buffer 0 len
                            in
                            read_result
                with
                | error ->
                    let _ = Kernel.Fs.File.close file in
                    raise error
              in
              result))

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
              let result =
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
                            let read_result =
                              match Kernel.Fs.File.read input buffer with
                              | Kernel.Result.Error error -> fail
                                (Kernel.Fs.File.error_to_string error)
                              | Kernel.Result.Ok read ->
                                  let _ = Kernel.Fs.File.close input in
                                  read = total
                                  && Kernel.Bytes.sub_string buffer 0 read = String.concat "" pieces
                            in
                            read_result
                with
                | error ->
                    let _ = Kernel.Fs.File.close file in
                    raise error
              in
              result))

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
              let result =
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
                            let read_result =
                              match Kernel.Fs.File.read_vectored input iov with
                              | Kernel.Result.Error error -> fail
                                (Kernel.Fs.File.error_to_string error)
                              | Kernel.Result.Ok read ->
                                  let _ = Kernel.Fs.File.close input in
                                  read = String.length payload
                                  && String.sub (Kernel.IO.Iovec.into_string iov) 0 read = payload
                            in
                            read_result
                with
                | error ->
                    let _ = Kernel.Fs.File.close file in
                    raise error
              in
              result))

let tests = [
  iovec_into_string_roundtrips;
  iovec_sub_matches_flattened_substring;
  path_join_preserves_segment_order;
  path_fold_join_preserves_many_segment_order;
  path_join_is_associative_for_simple_segments;
  ip_addr_loopback_parse_render_roundtrips;
  ip_addr_ipv4_parse_render_roundtrips;
  socket_addr_roundtrips;
  socket_addr_ipv4_roundtrips;
  file_slice_roundtrips;
  file_vectored_roundtrips;
  file_scalar_write_vectored_read_roundtrips;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_property_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
