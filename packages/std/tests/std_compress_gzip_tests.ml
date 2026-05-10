open Std
open Std.Compress

let gzip_hello =
  "\x1f\x8b\x08\x00\x00\x00\x00\x00\x02\xff\xcbH\xcd\xc9\xc9WH+\xca\xcfUH\xaf\xca,\xe0\x02\x00x-\xbdx\x10\x00\x00\x00"

let with_temp_dir = fun label fn ->
  let temp_root =
    Path.join (Path.v "/tmp") (Path.v ("riot_" ^ label ^ "_" ^ UUID.to_string (UUID.v4 ())))
  in
  match Fs.create_dir_all temp_root with
  | Error err -> Error ("failed to create temp dir: " ^ IO.error_message err)
  | Ok () ->
      let result = fn temp_root in
      let cleanup = Fs.remove_dir_all temp_root in
      (
        match (result, cleanup) with
        | (Error err, _) -> Error err
        | (Ok (), Ok ()) -> Ok ()
        | (Ok (), Error err) -> Error ("failed to remove temp dir: " ^ IO.error_message err)
      )

let render_gzip_error = Gzip.error_to_string

let test_decompress_string = fun _ctx ->
  match Gzip.decompress_string gzip_hello with
  | Ok "hello from gzip\n" -> Ok ()
  | Ok text -> Error ("unexpected decompressed string: " ^ text)
  | Error _ -> Error "failed to decompress known gzip payload"

let test_decompress_file = fun _ctx ->
  with_temp_dir
    "gzip_file"
    (fun dir ->
      let src = Path.join dir (Path.v "payload.txt.gz") in
      let dst = Path.join dir (Path.v "payload.txt") in
      match Fs.write gzip_hello src with
      | Error err -> Error ("failed to write gzip fixture: " ^ IO.error_message err)
      | Ok () ->
          match Gzip.decompress_file ~src ~dst with
          | Error _ -> Error "failed to decompress gzip file"
          | Ok () ->
              match Fs.read_to_string dst with
              | Ok "hello from gzip\n" -> Ok ()
              | Ok text -> Error ("unexpected decompressed file contents: " ^ text)
              | Error err -> Error ("failed to read decompressed file: " ^ IO.error_message err))

let test_compress_string_roundtrip = fun _ctx ->
  match Gzip.compress_string "hello from gzip\n" with
  | Error _ -> Error "failed to compress string into gzip payload"
  | Ok payload ->
      match Gzip.decompress_string payload with
      | Ok "hello from gzip\n" -> Ok ()
      | Ok text -> Error ("unexpected roundtrip string: " ^ text)
      | Error _ -> Error "failed to decompress compressed string payload"

let test_compress_file_roundtrip = fun _ctx ->
  with_temp_dir
    "gzip_compress_file"
    (fun dir ->
      let src = Path.join dir (Path.v "payload.txt") in
      let gzip_path = Path.join dir (Path.v "payload.txt.gz") in
      let roundtrip = Path.join dir (Path.v "payload.roundtrip.txt") in
      match Fs.write "hello from gzip\n" src with
      | Error err -> Error ("failed to write source file: " ^ IO.error_message err)
      | Ok () ->
          match Gzip.compress_file ~src ~dst:gzip_path with
          | Error _ -> Error "failed to compress file into gzip payload"
          | Ok () ->
              match Gzip.decompress_file ~src:gzip_path ~dst:roundtrip with
              | Error _ -> Error "failed to decompress roundtrip gzip file"
              | Ok () ->
                  match Fs.read_to_string roundtrip with
                  | Ok "hello from gzip\n" -> Ok ()
                  | Ok text -> Error ("unexpected roundtrip file contents: " ^ text)
                  | Error err -> Error ("failed to read roundtrip file: " ^ IO.error_message err))

let make_pseudorandom_string = fun len ->
  let bytes = IO.Bytes.create ~size:len in
  let state = ref 0x1234_abcd in
  for index = 0 to len - 1 do
    state := Int.rem ((1_103_515_245 * !state) + 12_345) 0x7fff_ffff;
    IO.Bytes.set_unchecked
      bytes
      ~at:index
      ~char:(Char.from_int_unchecked (Int.rem (Int.abs !state) 256))
  done;
  IO.Bytes.to_string bytes

let test_large_roundtrip = fun _ctx ->
  let original = make_pseudorandom_string (5 * 1_024 * 1_024) in
  match Gzip.compress_string original with
  | Error _ -> Error "failed to compress large payload"
  | Ok payload ->
      match Gzip.decompress_string payload with
      | Ok decoded when decoded = original -> Ok ()
      | Ok _ -> Error "large gzip roundtrip produced different contents"
      | Error err -> Error ("failed to decompress large payload: " ^ render_gzip_error err)

let tests =
  Test.[
    case "gzip compress string roundtrip" test_compress_string_roundtrip;
    case "gzip compress file roundtrip" test_compress_file_roundtrip;
    case "gzip decompress string" test_decompress_string;
    case "gzip decompress file" test_decompress_file;
    case "gzip large roundtrip" test_large_roundtrip;
  ]

let main ~args = Test.Cli.main ~name:"std_compress_gzip" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
