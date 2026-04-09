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
    Fs.with_tempdir
      ~prefix
      (fun tempdir ->
        let path = Kernel.Path.(Path.to_string tempdir / filename) in
        fn path)
  with
  | Ok value ->
      value
  | Error err ->
      fail (IO.error_message err)

let iovec_into_string_roundtrips =
  property
    "IO.Iovec of_string_array flattens with preserved order"
    Arbitrary.(array string)
    (fun values ->
      let iov = Kernel.IO.Iovec.of_string_array values in
      Kernel.IO.Iovec.into_string iov = String.concat "" (array_to_list values))

let iovec_sub_matches_flattened_substring =
  property
    "IO.Iovec.sub matches the flattened substring"
    Arbitrary.(pair (array string) (pair int int))
    (fun (values, (raw_pos, raw_len)) ->
      let iov = Kernel.IO.Iovec.of_string_array values in
      let total = Kernel.IO.Iovec.length iov in
      assume (total > 0);
      let pos = Int.abs raw_pos mod total in
      let remaining = total - pos in
      let len = Int.abs raw_len mod (remaining + 1) in
      let expected =
        String.sub (Kernel.IO.Iovec.into_string iov) pos len
      in
      let actual =
        Kernel.IO.Iovec.into_string (Kernel.IO.Iovec.sub ~pos ~len iov)
      in
      actual = expected)

let path_join_preserves_segment_order =
  property
    "Path.join preserves simple segment order"
    Arbitrary.(pair string string)
    (fun (left, right) ->
      assume (String.length left > 0);
      assume (String.length right > 0);
      assume (not (contains_slash left));
      assume (not (contains_slash right));
      Kernel.Path.to_string (Kernel.Path.join left right) = left ^ "/" ^ right)

let socket_addr_roundtrips =
  property
    "Net.SocketAddr.make roundtrips loopback parts"
    Arbitrary.(pair bool int)
    (fun (use_v6, raw_port) ->
      let port = Int.abs raw_port mod 65_536 in
      let ip =
        if use_v6 then
          Kernel.Net.IpAddr.v6_loopback
        else
          Kernel.Net.IpAddr.v4_loopback
      in
      match Kernel.Net.SocketAddr.make ~ip ~port with
      | Kernel.Result.Error _ ->
          false
      | Kernel.Result.Ok addr ->
          let addr_ip, addr_port = Kernel.Net.SocketAddr.to_parts addr in
          Kernel.Net.IpAddr.equal addr_ip ip && addr_port = port)

let file_slice_roundtrips =
  property
    "Fs.File partial write and read preserve the selected slice"
    Arbitrary.(pair string (pair int int))
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
          | Kernel.Result.Error error ->
              fail (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok file ->
              let result =
                try
                  match Kernel.Fs.File.write file ~pos ~len bytes with
                  | Kernel.Result.Error error ->
                      fail (Kernel.Fs.File.error_to_string error)
                  | Kernel.Result.Ok written ->
                      let _ = Kernel.Fs.File.close file in
                      if written != len then
                        false
                      else
                        match Kernel.Fs.File.open_read path with
                        | Kernel.Result.Error error ->
                            fail (Kernel.Fs.File.error_to_string error)
                        | Kernel.Result.Ok input ->
                            let buffer = Kernel.Bytes.create len in
                            let read_result =
                              match Kernel.Fs.File.read input buffer with
                              | Kernel.Result.Error error ->
                                  fail (Kernel.Fs.File.error_to_string error)
                              | Kernel.Result.Ok read ->
                                  let _ = Kernel.Fs.File.close input in
                                  read = len
                                  && Kernel.Bytes.sub_string bytes pos len
                                     = Kernel.Bytes.sub_string buffer 0 len
                            in
                            read_result
                with error ->
                  let _ = Kernel.Fs.File.close file in
                  raise error
              in
              result))

let tests = [
  iovec_into_string_roundtrips;
  iovec_sub_matches_flattened_substring;
  path_join_preserves_segment_order;
  socket_addr_roundtrips;
  file_slice_roundtrips;
]

let main = fun ~args ->
  Test.Cli.main ~name:"kernel_new_property_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
